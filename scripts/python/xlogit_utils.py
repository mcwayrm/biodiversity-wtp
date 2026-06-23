#!/usr/bin/env python3
"""Shared helpers for xlogit estimation scripts."""

import time
import traceback

import numpy as np
import pandas as pd
import scipy.stats
from xlogit import MixedLogit


def standardize_data(x_data, eps=1e-10):
    """Robust standardization: normalize to mean=0, std=1."""
    scaler = {}
    x_scaled = x_data.copy()

    for col in x_data.columns:
        mean_val = x_data[col].mean()
        std_val = x_data[col].std()
        scaler[col] = {"mean": mean_val, "std": std_val}

        if std_val > eps:
            x_scaled[col] = (x_data[col] - mean_val) / std_val

    return x_scaled, scaler


def flatten_fe_columns(fe_vars, available_columns):
    """Flatten FE specs from models.yml and keep only columns present in the data."""
    fe_cols = []
    for fe_spec in fe_vars:
        if isinstance(fe_spec, list):
            fe_cols.extend(fe_spec)
        else:
            fe_cols.append(fe_spec)
    return [col for col in fe_cols if col in available_columns]


def normalize_model_vars_for_demean(model_vars):
    """Map configured model vars to the columns used in estimation prep."""
    return [var if var != "travel_cost_combined" else "log_travel_cost" for var in model_vars]


def sample_trip_group(group, choice_set_sample_size, trip_seed):
    """Sample non-chosen alternatives and pad to a fixed choice set size."""
    chosen = group[group["choice"] == 1]
    not_chosen = group[group["choice"] == 0]
    n_to_sample = max(0, choice_set_sample_size - len(chosen))

    if n_to_sample > 0 and len(not_chosen) > 0:
        sampled_not_chosen = not_chosen.sample(
            n=min(n_to_sample, len(not_chosen)),
            random_state=trip_seed,
        )
        sampled_group = pd.concat([chosen, sampled_not_chosen], ignore_index=True)
    else:
        sampled_group = group.copy()

    n_current = len(sampled_group)
    if n_current < choice_set_sample_size:
        n_pad = choice_set_sample_size - n_current
        pad_rows = sampled_group.iloc[[0] * n_pad].copy()
        pad_rows["choice"] = 0
        pad_rows["avail"] = 0
        sampled_group = pd.concat([sampled_group, pad_rows], ignore_index=True)
    elif n_current > choice_set_sample_size:
        sampled_group = sampled_group.iloc[:choice_set_sample_size].copy()

    return sampled_group


def add_choice_identifiers(cs_model):
    """Add sequential observation and alternative ids required by xlogit."""
    cs_model = cs_model.copy()
    cs_model["obs_id_num_seq"] = cs_model.groupby("obs_id_num", sort=False).ngroup()
    cs_model["alt_id"] = cs_model.groupby("obs_id_num_seq", sort=False).cumcount() + 1
    return cs_model


def estimate_mixed_logit(
    x_data,
    y_data,
    ids_data,
    alts_data,
    avail_data,
    model_vars_for_model,
    randvars,
    verbose=True,
):
    """Estimate a mixed logit model after standardizing regressors."""
    try:
        if verbose:
            print("    Data validation:")
            print(f"      X shape: {x_data.shape}")
            print(f"      X NaN: {x_data.isna().sum().sum()}")
            print(f"      X std range: [{x_data.std().min():.6f}, {x_data.std().max():.6f}]")

        if isinstance(x_data, pd.DataFrame):
            x_scaled, _ = standardize_data(x_data)
            x_data = x_scaled

        if verbose:
            print("    ✓ Standardized (mean=0, std=1)")
            print(f"      y choices: {y_data.sum()}/{len(y_data)}")
            print(f"      Choice situations: {ids_data.nunique()}")
            print("    Fitting MixedLogit (random coeff on richness, 10 Halton draws):")
            print(f"      Random vars: {list(randvars.keys())}")

        start_time = time.time()
        model = MixedLogit()
        model.fit(
            X=x_data,
            y=y_data,
            varnames=model_vars_for_model,
            ids=ids_data,
            alts=alts_data,
            avail=avail_data,
            randvars=randvars,
            n_draws=10,
            optim_method="L-BFGS-B",
            skip_std_errs=False,
            mnl_init=False,
        )

        elapsed = time.time() - start_time
        if verbose:
            print(f"    Estimation complete in {elapsed:.1f}s")

        return model, elapsed, True

    except Exception as exc:
        print(f"    ERROR: {type(exc).__name__}: {exc}")
        traceback.print_exc()
        return None, 0, False


def extract_results(model, y_data, ids_data, alts_data):
    """Extract coefficients and summary statistics from an estimated model."""
    try:
        coef_df = pd.DataFrame(
            {
                "Estimate": model.coeff_,
                "Std.Err": model.stderr,
                "p-value": model.pvalues,
            },
            index=model.coeff_names,
        )
        coef_df["t-stat"] = coef_df["Estimate"] / coef_df["Std.Err"]

        results = {
            "LL": model.loglikelihood,
            "AIC": model.aic,
            "BIC": model.bic,
            "N_obs": len(y_data),
            "N_cov": ids_data.nunique(),
            "N_alts": alts_data.max(),
        }

        return coef_df, results, True
    except Exception as exc:
        print(f"    ERROR extracting results: {exc}")
        return None, {}, False


def calculate_wtp(coef_df):
    """Calculate WTP using a delta-method approximation."""
    try:
        price_coeff_idx = None
        for idx, name in enumerate(coef_df.index):
            if "log_travel_cost" in name.lower() or "travel_cost" in name.lower():
                price_coeff_idx = idx
                break

        if price_coeff_idx is None:
            return None

        beta_price = coef_df.iloc[price_coeff_idx]["Estimate"]
        se_price = coef_df.iloc[price_coeff_idx]["Std.Err"]

        wtp_data = []
        for idx, var in enumerate(coef_df.index):
            if idx == price_coeff_idx:
                continue

            beta_attr = coef_df.iloc[idx]["Estimate"]
            se_attr = coef_df.iloc[idx]["Std.Err"]
            wtp_val = beta_attr / (-beta_price)
            se_wtp = np.sqrt((se_attr / (-beta_price)) ** 2 + (beta_attr * se_price / (beta_price ** 2)) ** 2)
            t_stat = wtp_val / (se_wtp + 1e-10)
            p_val = 2 * (1 - scipy.stats.t.cdf(np.abs(t_stat), df=len(coef_df)))

            wtp_data.append(
                {
                    "Variable": var,
                    "WTP": wtp_val,
                    "Std.Err": se_wtp,
                    "t-stat": t_stat,
                    "p-value": p_val,
                }
            )

        return pd.DataFrame(wtp_data).set_index("Variable")
    except Exception as exc:
        print(f"    Error calculating WTP: {exc}")
        return None