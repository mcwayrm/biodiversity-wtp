#!/usr/bin/env python3
"""
13_model_iv.py - Control-function mixed logit with IV-based residual inclusion.

Purpose:
  - Mirror the stage-11b xlogit estimation workflow.
  - Use the IV `IV_dt` from stage 10 in a first-stage linear projection.
  - Add the first-stage residual to the second-stage mixed logit as a control function.

Notes:
  - This script is not wired into run_all.R yet.
  - Second-stage standard errors remain the usual xlogit output.
  - The bootstrap section below is only a saved design scaffold for later use.
"""

import argparse
import json
import sys
import traceback
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import scipy.stats
import yaml

from xlogit_utils import (
	add_choice_identifiers,
	calculate_wtp,
	estimate_mixed_logit,
	extract_results,
	flatten_fe_columns,
	normalize_model_vars_for_demean,
	sample_trip_group,
)

warnings.filterwarnings("ignore")


ENDOGENOUS_CANDIDATES = ["expected_richness", "migrant_richness"]


def parse_arguments():
	parser = argparse.ArgumentParser(
		description="XLogit RUM Estimation with IV Control Function"
	)
	parser.add_argument("--scenario", required=True, help="Scenario name")
	parser.add_argument("--input-data", required=True, help="Path to prepped model parquet")
	parser.add_argument("--output-dir", required=True, help="Output directory")
	parser.add_argument("--models-config", default="models.yml", help="Models YAML config file")
	parser.add_argument("--models-subset", default=None, help="Comma-separated list of models to run")
	parser.add_argument("--instrument-var", default="IV_dt", help="Instrument column name")
	parser.add_argument(
		"--bootstrap-plan-reps",
		type=int,
		default=200,
		help="Bootstrap repetitions to record in the plan scaffold only",
	)
	parser.add_argument(
		"--bootstrap-plan-seed",
		type=int,
		default=20240623,
		help="Seed to record in the bootstrap plan scaffold only",
	)
	return parser.parse_args()


def identify_endogenous_variable(model_vars, mixed_vars):
	search_order = list(mixed_vars) + list(model_vars)
	for candidate in search_order:
		if candidate in ENDOGENOUS_CANDIDATES:
			return candidate
	return None


def build_mean_lookup(df, fe_cols, vars_to_mean):
	valid_vars = [var for var in vars_to_mean if var in df.columns]
	if len(fe_cols) == 0 or len(valid_vars) == 0:
		return {}

	grouped = df.groupby(fe_cols, dropna=False)[valid_vars].mean()
	return {var: grouped[var] for var in valid_vars}


def demean_with_lookup(df, source_var, fe_cols, mean_lookup):
	demeaned_col = f"{source_var}_dm"
	if source_var not in df.columns:
		return None

	if len(fe_cols) == 0 or source_var not in mean_lookup:
		df[demeaned_col] = df[source_var]
		return demeaned_col

	try:
		mapped_means = df.set_index(fe_cols).index.map(mean_lookup[source_var])
		df[demeaned_col] = df[source_var] - mapped_means
	except Exception:
		df[demeaned_col] = df[source_var]

	return demeaned_col


def fit_ols_projection(design, y_vec):
	design_matrix = np.column_stack([np.ones(len(design)), design.to_numpy()])
	n_obs, n_params = design_matrix.shape
	coeff = np.linalg.lstsq(design_matrix, y_vec.to_numpy(), rcond=None)[0]
	fitted = design_matrix @ coeff
	resid = y_vec.to_numpy() - fitted

	df_resid = max(n_obs - n_params, 1)
	ss_resid = float(np.sum(resid ** 2))
	ss_total = float(np.sum((y_vec.to_numpy() - y_vec.mean()) ** 2))
	sigma2 = ss_resid / df_resid
	xtx_inv = np.linalg.pinv(design_matrix.T @ design_matrix)
	vcov = sigma2 * xtx_inv
	std_err = np.sqrt(np.maximum(np.diag(vcov), 0))
	t_stats = np.divide(coeff, std_err, out=np.full_like(coeff, np.nan), where=std_err > 0)
	p_values = 2 * (1 - scipy.stats.t.cdf(np.abs(t_stats), df=df_resid))
	r_squared = np.nan if ss_total <= 0 else 1 - (ss_resid / ss_total)

	return {
		"design_matrix": design_matrix,
		"coeff": coeff,
		"fitted": fitted,
		"resid": resid,
		"n_obs": int(n_obs),
		"n_params": int(n_params),
		"df_resid": int(df_resid),
		"ss_resid": ss_resid,
		"ss_total": ss_total,
		"sigma2": sigma2,
		"std_err": std_err,
		"t_stats": t_stats,
		"p_values": p_values,
		"r_squared": None if np.isnan(r_squared) else float(r_squared),
		"condition_number": float(np.linalg.cond(design_matrix)),
	}


def summarize_first_stage(unrestricted_fit, restricted_fit, design_cols, endogenous_var, instrument_var):
	instrument_index = 1
	instrument_coef = float(unrestricted_fit["coeff"][instrument_index])
	instrument_se = float(unrestricted_fit["std_err"][instrument_index])
	instrument_t = float(unrestricted_fit["t_stats"][instrument_index])
	instrument_p = float(unrestricted_fit["p_values"][instrument_index])
	first_stage_f = float(instrument_t ** 2) if not np.isnan(instrument_t) else None

	restricted_ss = restricted_fit["ss_resid"]
	unrestricted_ss = unrestricted_fit["ss_resid"]
	partial_r2 = None
	if restricted_ss > 0 and restricted_ss >= unrestricted_ss:
		partial_r2 = float((restricted_ss - unrestricted_ss) / restricted_ss)

	coefficient_rows = [{"variable": "const", "estimate": float(unrestricted_fit["coeff"][0])}]
	for idx, col in enumerate(design_cols, start=1):
		coefficient_rows.append(
			{
				"variable": col,
				"estimate": float(unrestricted_fit["coeff"][idx]),
				"std_err": float(unrestricted_fit["std_err"][idx]),
				"t_stat": float(unrestricted_fit["t_stats"][idx]),
				"p_value": float(unrestricted_fit["p_values"][idx]),
			}
		)

	return {
		"endogenous_var": endogenous_var,
		"instrument_var": instrument_var,
		"n_obs": unrestricted_fit["n_obs"],
		"n_params": unrestricted_fit["n_params"],
		"df_resid": unrestricted_fit["df_resid"],
		"r_squared": unrestricted_fit["r_squared"],
		"residual_variance": float(unrestricted_fit["sigma2"]),
		"condition_number": unrestricted_fit["condition_number"],
		"instrument_diagnostic": {
			"coefficient": instrument_coef,
			"std_err": instrument_se,
			"t_stat": instrument_t,
			"p_value": instrument_p,
			"f_stat": first_stage_f,
			"partial_r_squared": partial_r2,
		},
		"coefficients": coefficient_rows,
	}


def run_control_function_first_stage(cs_model, endogenous_var, instrument_var, model_vars_for_demean):
	residual_name = f"{endogenous_var}_cf_resid"
	first_stage_df = cs_model[cs_model["avail"] == 1].copy()

	endog_col = f"{endogenous_var}_dm"
	inst_col = f"{instrument_var}_dm"
	if endog_col not in first_stage_df.columns or inst_col not in first_stage_df.columns:
		raise ValueError(f"Missing first-stage columns: {endog_col} or {inst_col}")

	control_vars = [var for var in model_vars_for_demean if var != endogenous_var]
	control_cols = [f"{var}_dm" for var in control_vars if f"{var}_dm" in first_stage_df.columns]
	design_cols = [inst_col] + control_cols

	design = first_stage_df[design_cols].apply(pd.to_numeric, errors="coerce")
	y_vec = pd.to_numeric(first_stage_df[endog_col], errors="coerce")
	valid_mask = design.notna().all(axis=1) & y_vec.notna()
	design = design.loc[valid_mask].reset_index(drop=True)
	y_vec = y_vec.loc[valid_mask].reset_index(drop=True)

	if len(design) == 0:
		raise ValueError("No valid first-stage observations after dropping missing values")

	unrestricted_fit = fit_ols_projection(design, y_vec)
	if control_cols:
		restricted_fit = fit_ols_projection(design[control_cols], y_vec)
	else:
		restricted_fit = fit_ols_projection(pd.DataFrame(index=design.index), y_vec)

	first_stage_df[residual_name] = np.nan
	first_stage_df.loc[valid_mask, residual_name] = unrestricted_fit["resid"]
	residual_map = first_stage_df[["obs_id_num_seq", "alt_id", residual_name]]

	cs_model = cs_model.merge(residual_map, on=["obs_id_num_seq", "alt_id"], how="left")
	cs_model[residual_name] = cs_model[residual_name].fillna(0.0)

	first_stage_summary = summarize_first_stage(
		unrestricted_fit=unrestricted_fit,
		restricted_fit=restricted_fit,
		design_cols=design_cols,
		endogenous_var=endogenous_var,
		instrument_var=instrument_var,
	)
	return cs_model, residual_name, first_stage_summary


def build_bootstrap_plan(
	scenario,
	model_name,
	endogenous_var,
	instrument_var,
	choice_set_sample_size,
	bootstrap_reps,
	bootstrap_seed,
):
	return {
		"status": "design_only",
		"scenario": scenario,
		"model": model_name,
		"endogenous_var": endogenous_var,
		"instrument_var": instrument_var,
		"bootstrap_reps": bootstrap_reps,
		"bootstrap_seed": bootstrap_seed,
		"resample_unit": "trip_id",
		"sample_with_replacement": True,
		"choice_set_sample_size": choice_set_sample_size,
		"stages_to_recompute": [
			"sample alternatives within each resampled trip",
			"demean regressors and instrument by configured FE structure",
			"re-run first-stage control-function projection",
			"re-run second-stage mixed logit with residual inclusion",
			"recompute coefficients, WTP, and first-stage diagnostics",
		],
		"inference_target": [
			"second-stage coefficients",
			"WTP estimates",
			"control-function residual coefficient",
		],
		"notes": [
			"This file is a scaffold only and is not executed by the script.",
			"Bootstrap draws should preserve the trip-level choice structure.",
			"Full Bradt-style inference should use the empirical distribution across bootstrap replications.",
		],
	}


def save_outputs(output_dir, output_prefix, scenario, coef_df, results, first_stage_summary, bootstrap_plan):
	summary_lines = []
	summary_lines.append("=" * 70)
	summary_lines.append(f"MODEL SUMMARY: {output_prefix}")
	summary_lines.append("=" * 70)
	summary_lines.append(f"Scenario: {scenario}")
	summary_lines.append("")
	summary_lines.append("FIT STATISTICS:")
	summary_lines.append(f"  Log-Likelihood: {results['LL']:.4f}")
	summary_lines.append(f"  AIC: {results['AIC']:.4f}")
	summary_lines.append(f"  BIC: {results['BIC']:.4f}")
	summary_lines.append(f"  N Observations: {results['N_obs']}")
	summary_lines.append(f"  N Choice Situations: {results['N_cov']}")
	summary_lines.append("")
	summary_lines.append("FIRST STAGE:")
	summary_lines.append(f"  Endogenous variable: {first_stage_summary['endogenous_var']}")
	summary_lines.append(f"  Instrument: {first_stage_summary['instrument_var']}")
	summary_lines.append(f"  N Observations: {first_stage_summary['n_obs']}")
	summary_lines.append(f"  R-squared: {first_stage_summary['r_squared']}")
	summary_lines.append(f"  Residual variance: {first_stage_summary['residual_variance']}")
	summary_lines.append(f"  Condition number: {first_stage_summary['condition_number']:.4f}")
	summary_lines.append(
		f"  IV coefficient: {first_stage_summary['instrument_diagnostic']['coefficient']:.6f}"
	)
	summary_lines.append(
		f"  IV std. err.: {first_stage_summary['instrument_diagnostic']['std_err']:.6f}"
	)
	summary_lines.append(
		f"  IV t-stat: {first_stage_summary['instrument_diagnostic']['t_stat']:.4f}"
	)
	summary_lines.append(
		f"  IV p-value: {first_stage_summary['instrument_diagnostic']['p_value']:.4f}"
	)
	summary_lines.append(
		f"  First-stage F-stat: {first_stage_summary['instrument_diagnostic']['f_stat']:.4f}"
	)
	summary_lines.append(
		f"  Partial R-squared: {first_stage_summary['instrument_diagnostic']['partial_r_squared']}"
	)
	summary_lines.append("")
	summary_lines.append("BOOTSTRAP PLAN:")
	summary_lines.append(f"  Status: {bootstrap_plan['status']}")
	summary_lines.append(f"  Repetitions: {bootstrap_plan['bootstrap_reps']}")
	summary_lines.append(f"  Resample unit: {bootstrap_plan['resample_unit']}")
	summary_lines.append("")
	summary_lines.append("COEFFICIENTS:")
	summary_lines.append(f"{'Variable':<35} {'Estimate':>12} {'Std.Err':>12} {'t-stat':>10} {'p-value':>10}")
	summary_lines.append("-" * 85)
	for var in coef_df.index:
		row = coef_df.loc[var]
		summary_lines.append(
			f"{var:<35} {row['Estimate']:>12.6f} {row['Std.Err']:>12.6f} {row['t-stat']:>10.4f} {row['p-value']:>10.4f}"
		)

	summary_file = output_dir / f"{output_prefix}_{scenario}_summary.txt"
	summary_file.write_text("\n".join(summary_lines), encoding="utf-8")

	coef_export = coef_df.copy()
	coef_export["Variable"] = coef_export.index
	coef_export = coef_export[["Variable", "Estimate", "Std.Err", "t-stat", "p-value"]]
	coef_export["Scenario"] = scenario
	coef_export["Model"] = output_prefix
	coef_export.to_csv(output_dir / f"{output_prefix}_{scenario}_coefficients.csv", index=False)

	with open(output_dir / f"{output_prefix}_{scenario}_first_stage.json", "w", encoding="utf-8") as handle:
		json.dump(first_stage_summary, handle, indent=2)

	with open(output_dir / f"{output_prefix}_{scenario}_bootstrap_plan.json", "w", encoding="utf-8") as handle:
		json.dump(bootstrap_plan, handle, indent=2)

	wtp_df = calculate_wtp(coef_df)
	if wtp_df is not None:
		wtp_export = wtp_df.copy()
		wtp_export["Variable"] = wtp_export.index
		wtp_export = wtp_export[["Variable", "WTP", "Std.Err", "t-stat", "p-value"]]
		wtp_export["Scenario"] = scenario
		wtp_export["Model"] = output_prefix
		wtp_export.to_csv(output_dir / f"{output_prefix}_{scenario}_wtp.csv", index=False)


def main():
	args = parse_arguments()

	print("=" * 70)
	print("13: XLOGIT CONTROL-FUNCTION IV ESTIMATION")
	print("=" * 70)
	print(f"Scenario: {args.scenario}")
	print(f"Input data: {args.input_data}")
	print(f"Instrument: {args.instrument_var}")

	output_dir = Path(args.output_dir)
	output_dir.mkdir(parents=True, exist_ok=True)

	with open(args.models_config, "r", encoding="utf-8") as handle:
		models_config = yaml.safe_load(handle)

	model_names = list(models_config.keys())
	if args.models_subset:
		keep = set(args.models_subset.split(","))
		model_names = [name for name in model_names if name in keep]

	print(f"Models to run: {len(model_names)}\n")

	try:
		cs = pd.read_parquet(args.input_data)
		print(f"Loaded: {len(cs):,} rows, {cs['trip_id'].nunique():,} trips\n")
	except Exception as exc:
		print(f"Error loading data: {exc}")
		sys.exit(1)

	if "avail" not in cs.columns:
		cs["avail"] = 1

	if args.instrument_var not in cs.columns:
		print(f"Instrument column not found: {args.instrument_var}")
		sys.exit(1)

	success_count = 0
	fail_count = 0

	for model_idx, model_name in enumerate(model_names, 1):
		print(f"\n[Run {model_idx}/{len(model_names)}] Model: {model_name}_CFMixed")

		try:
			model_config = models_config[model_name]
			model_vars = model_config["model_vars"].copy()
			fe_vars = model_config.get("fe_vars", [])
			mixed_vars = model_config.get("mixed_vars", [])
			choice_set_sample_size = model_config.get("choice_set_sample_size", 10)

			fe_cols = flatten_fe_columns(fe_vars, cs.columns)
			endogenous_var = identify_endogenous_variable(model_vars, mixed_vars)
			if endogenous_var is None:
				print("    No supported endogenous variable found, skipping model")
				fail_count += 1
				continue

			model_vars_for_demean = normalize_model_vars_for_demean(model_vars)
			mean_vars = sorted(set(model_vars_for_demean + mixed_vars + [args.instrument_var]))
			mean_lookup = build_mean_lookup(cs, fe_cols, mean_vars)

			print(f"    FE groups: {fe_cols}")
			print(f"    Endogenous variable: {endogenous_var}")
			print("    Sampling and demeaning...")

			cs_model = None
			trip_batch = []
			trip_count = 0
			seed = 12345
			batch_size = 5000

			for trip_id, group in cs.groupby("trip_id", sort=False):
				trip_count += 1
				trip_seed = seed + (hash(trip_id) % 10000)
				sampled_group = sample_trip_group(group, choice_set_sample_size, trip_seed)

				for var in mean_vars:
					demean_with_lookup(sampled_group, var, fe_cols, mean_lookup)

				trip_batch.append(sampled_group)
				if len(trip_batch) >= batch_size:
					batch_df = pd.concat(trip_batch, ignore_index=True)
					cs_model = batch_df if cs_model is None else pd.concat([cs_model, batch_df], ignore_index=True)
					trip_batch = []

			if len(trip_batch) > 0:
				batch_df = pd.concat(trip_batch, ignore_index=True)
				cs_model = batch_df if cs_model is None else pd.concat([cs_model, batch_df], ignore_index=True)

			print(f"    ✓ Complete: {trip_count:,} trips, {len(cs_model):,} rows")

			cs_model = add_choice_identifiers(cs_model)

			print("    Estimating first stage...")
			cs_model, residual_name, first_stage_summary = run_control_function_first_stage(
				cs_model=cs_model,
				endogenous_var=endogenous_var,
				instrument_var=args.instrument_var,
				model_vars_for_demean=model_vars_for_demean,
			)
			print(f"    ✓ Residual added: {residual_name}")
			print(
				"    ✓ First-stage diagnostics: "
				f"F={first_stage_summary['instrument_diagnostic']['f_stat']:.4f}, "
				f"partial_R2={first_stage_summary['instrument_diagnostic']['partial_r_squared']}"
			)

			model_vars_for_mixed_dm = [f"{var}_dm" for var in model_vars_for_demean]
			model_vars_for_mixed_dm.append(residual_name)

			randvars_available = {
				f"{var}_dm": "n" for var in mixed_vars if f"{var}_dm" in model_vars_for_mixed_dm
			}
			if len(randvars_available) == 0:
				print("    ERROR: No random vars available")
				fail_count += 1
				continue

			x_data = cs_model[model_vars_for_mixed_dm].fillna(0).reset_index(drop=True)
			y_data = cs_model["choice"].reset_index(drop=True)
			ids_data = cs_model["obs_id_num_seq"].reset_index(drop=True)
			alts_data = cs_model["alt_id"].reset_index(drop=True)
			avail_data = cs_model["avail"].reset_index(drop=True)

			print("    Estimating second-stage mixed model...")
			model_mixed, _, success = estimate_mixed_logit(
				x_data,
				y_data,
				ids_data,
				alts_data,
				avail_data,
				model_vars_for_mixed_dm,
				randvars_available,
				verbose=True,
			)

			if not success or model_mixed is None:
				print("    Estimation failed")
				fail_count += 1
				continue

			coef_df, results, extracted = extract_results(model_mixed, y_data, ids_data, alts_data)
			if not extracted or coef_df is None:
				print("    Results extraction failed")
				fail_count += 1
				continue

			bootstrap_plan = build_bootstrap_plan(
				scenario=args.scenario,
				model_name=model_name,
				endogenous_var=endogenous_var,
				instrument_var=args.instrument_var,
				choice_set_sample_size=choice_set_sample_size,
				bootstrap_reps=args.bootstrap_plan_reps,
				bootstrap_seed=args.bootstrap_plan_seed,
			)

			output_prefix = f"{model_name}_CFMixed"
			save_outputs(
				output_dir,
				output_prefix,
				args.scenario,
				coef_df,
				results,
				first_stage_summary,
				bootstrap_plan,
			)
			print(f"    ✓ Success - LL: {results['LL']:.2f}")
			success_count += 1

		except Exception as exc:
			print(f"    Error: {type(exc).__name__}: {exc}")
			traceback.print_exc()
			fail_count += 1

	print("\n" + "=" * 70)
	print(f"BATCH COMPLETE: {success_count} success, {fail_count} failed")
	print("=" * 70)

	sys.exit(1 if fail_count > 0 else 0)


if __name__ == "__main__":
	main()
