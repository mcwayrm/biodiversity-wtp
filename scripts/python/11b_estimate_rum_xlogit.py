#!/usr/bin/env python3
"""
11b_estimate_rum_xlogit.py - XLogit RUM Estimation (Stage 2)

Purpose:
  - Load PREPPED data from 11a (filtered, with seasonal vars)
  - Load pre-computed global means from JSON
  - Per-trip sampling + flexible FE demeaning
  - Fit mixed logit models as specified in models.yml
  - Output: coefficients, summaries, WTP

Input:
  --scenario: scenario name (above_median_c5km_v5km_r50km_mInf)
  --input-data: path to model_data_{scenario}.parquet (PREPPED, not raw)
  --output-dir: output directory
  --models-config: models.yml

Note: This script assumes data is already filtered, has season, hour_of_day, 
and log_travel_cost columns. It loads pre-computed global means from JSON files
in the same directory as output.
"""

import pandas as pd
import numpy as np
import yaml
import json
import time
import sys
import argparse
from pathlib import Path
import warnings
import traceback

from xlogit_utils import (
    add_choice_identifiers,
    calculate_wtp,
    estimate_mixed_logit,
    extract_results,
    flatten_fe_columns,
    normalize_model_vars_for_demean,
    sample_trip_group,
)

warnings.filterwarnings('ignore')


# ============================================================================
# CLI ARGUMENT PARSING
# ============================================================================

def parse_arguments():
    parser = argparse.ArgumentParser(
        description='XLogit RUM Estimation - Stage 2 (Estimation from Prepped Data)'
    )
    parser.add_argument('--scenario', required=True,
                        help='Scenario name')
    parser.add_argument('--input-data', required=True,
                        help='Path to model_data_{scenario}.parquet (prepped data)')
    parser.add_argument('--output-dir', required=True,
                        help='Output directory')
    parser.add_argument('--models-config', default='models.yml',
                        help='Models YAML config file')
    parser.add_argument('--models-subset', default=None,
                        help='Comma-separated list of models to run')
    
    return parser.parse_args()


# ============================================================================
# MAIN PROCESSING
# ============================================================================

def main():
    args = parse_arguments()
    
    print("=" * 70)
    print("11B: XLOGIT RUM ESTIMATION (FROM PREPPED DATA)")
    print("=" * 70)
    print(f"Scenario: {args.scenario}")
    print(f"Input data: {args.input_data}")
    print(f"Output dir: {args.output_dir}")
    
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Load configurations
    with open(args.models_config, 'r') as f:
        models_config = yaml.safe_load(f)
    
    model_names = list(models_config.keys())
    if args.models_subset:
        model_names = [m for m in model_names if m in args.models_subset.split(',')]
    
    print(f"Models to run: {len(model_names)}\n")
    
    # =========================================================================
    # LOAD PREPPED DATA (already filtered, has season, hour_of_day, etc.)
    # =========================================================================
    
    try:
        print(f"[Loading Prepped Data]")
        print(f"  Reading: {args.input_data}")
        cs = pd.read_parquet(args.input_data)
        print(f"  ✓ Loaded: {len(cs):,} rows, {cs['trip_id'].nunique():,} trips\n")
    except Exception as e:
        print(f"  ✗ Error loading data: {e}")
        sys.exit(1)
    
    # =========================================================================
    # DEFINE VARIABLES FOR MEANS (same as in 11a)
    # =========================================================================
    
    all_possible_vars = [
        'expected_richness', 'migrant_richness', 'resident_richness',
        'expected_congestion', 'precip', 'temp', 'trees', 'log_travel_cost',
        'expected_richness_Winter', 'expected_richness_Spring', 'expected_richness_Summer',
        'migrant_richness_Winter', 'migrant_richness_Spring', 'migrant_richness_Summer',
        'resident_richness_Winter', 'resident_richness_Spring', 'resident_richness_Summer'
    ]
    
    if 'avail' not in cs.columns:
        cs['avail'] = 1
    
    # =========================================================================
    # LOOP THROUGH MODELS
    # =========================================================================
    
    success_count = 0
    fail_count = 0
    
    for model_idx, model_name in enumerate(model_names, 1):
        print(f"\n[Run {model_idx}/{len(model_names)}] Model: {model_name}_Mixed")
        
        # Check if output files already exist
        output_prefix = f"{model_name}_Mixed_{args.scenario}"
        summary_file = output_dir / f"{output_prefix}_summary.txt"
        coef_file = output_dir / f"{output_prefix}_coefficients.csv"
        
        if summary_file.exists() and coef_file.exists():
            print(f"    ℹ Output files already exist, skipping...")
            success_count += 1
            continue
        
        try:
            model_config = models_config[model_name]
            model_vars = model_config['model_vars'].copy()
            fe_vars = model_config.get('fe_vars', [])
            mixed_vars = model_config.get('mixed_vars', [])
            
            # Extract FE column names
            fe_cols = flatten_fe_columns(fe_vars, cs.columns)
            
            # Create FE key for loading means
            fe_key = "-".join(sorted(fe_cols)) if fe_cols else "no_fe"
            
            print(f"    FE groups: {fe_cols}")
            print(f"    Loading pre-computed global means...")
            
            # Load global means JSON
            means_file = output_dir / f"global_means_{fe_key}_{args.scenario}.json"
            if not means_file.exists():
                print(f"    WARNING: Means file not found: {means_file.name}")
                print(f"    Skipping model")
                fail_count += 1
                continue
            
            with open(means_file, 'r') as f:
                global_means_json = json.load(f)
            
            # Convert JSON to pandas Series format for demeaning
            # JSON format: {variable: {fe_key1: mean1, fe_key2: mean2, ...}, ...}
            # Convert to: {variable: Series indexed by FE groups}
            global_means_model = {}
            for var, means_dict in global_means_json.items():
                if isinstance(means_dict, dict):
                    # Create multi-index from fe_key
                    index_data = []
                    for key_str, mean_val in means_dict.items():
                        # Parse key: "col1=val1|col2=val2" back to tuple
                        parts = key_str.split('|')
                        idx = tuple([p.split('=')[1] for p in parts])
                        index_data.append((idx, mean_val))
                    
                    if index_data:
                        indices, values = zip(*index_data)
                        global_means_model[var] = pd.Series(values, index=pd.MultiIndex.from_tuples(indices, names=fe_cols))
            
            print(f"    ✓ Loaded means for {len(global_means_model)} variables")
            
            # ===================================================================
            # PER-TRIP SAMPLING AND DEMEANING
            # ===================================================================
            
            print(f"    Sampling and demeaning (batch processing)...")
            
            model_vars_for_demean = normalize_model_vars_for_demean(model_vars)
            
            choice_set_sample_size = 10
            seed = 12345
            batch_size = 5000
            
            cs_model = None
            trip_batch = []
            trip_count = 0
            
            for trip_id, group in cs.groupby('trip_id', sort=False):
                trip_count += 1
                
                trip_seed = seed + (hash(trip_id) % 10000)
                sampled_group = sample_trip_group(group, choice_set_sample_size, trip_seed)
                
                # Apply demeaning
                for var in model_vars_for_demean + mixed_vars:
                    if var in sampled_group.columns and var in global_means_model:
                        means_series = global_means_model[var]
                        demeaned_col = f'{var}_dm'
                        
                        if len(fe_cols) > 0:
                            try:
                                sampled_group[demeaned_col] = (
                                    sampled_group[var] - 
                                    sampled_group.set_index(fe_cols).index.map(means_series)
                                )
                            except Exception as e:
                                sampled_group[demeaned_col] = sampled_group[var]
                        else:
                            sampled_group[demeaned_col] = sampled_group[var]
                    elif var in sampled_group.columns:
                        sampled_group[f'{var}_dm'] = sampled_group[var]
                
                trip_batch.append(sampled_group)
                
                # Process batch
                if len(trip_batch) >= batch_size:
                    batch_df = pd.concat(trip_batch, ignore_index=True)
                    if cs_model is None:
                        cs_model = batch_df
                    else:
                        cs_model = pd.concat([cs_model, batch_df], ignore_index=True)
                    trip_batch = []
                    print(f"      Processed {trip_count:,} trips")
            
            # Process remaining
            if len(trip_batch) > 0:
                batch_df = pd.concat(trip_batch, ignore_index=True)
                if cs_model is None:
                    cs_model = batch_df
                else:
                    cs_model = pd.concat([cs_model, batch_df], ignore_index=True)
            
            print(f"    ✓ Complete: {trip_count:,} trips, {len(cs_model):,} rows")
            
            # ===================================================================
            # PREPARE DATA ARRAYS
            # ===================================================================
            
            cs_model = add_choice_identifiers(cs_model)
            
            model_vars_for_mixed_dm = [f"{v}_dm" for v in model_vars_for_demean]
            randvars_available = {f"{var}_dm": 'n' for var in mixed_vars if f"{var}_dm" in model_vars_for_mixed_dm}
            
            if len(randvars_available) == 0:
                print(f"    ERROR: No random vars available")
                fail_count += 1
                continue
            
            print(f"    Model vars: {model_vars_for_mixed_dm}")
            print(f"    Random vars: {list(randvars_available.keys())}")
            
            X_data = cs_model[model_vars_for_mixed_dm].fillna(0).reset_index(drop=True)
            y_data = cs_model['choice'].reset_index(drop=True)
            ids_data = cs_model['obs_id_num_seq'].reset_index(drop=True)
            alts_data = cs_model['alt_id'].reset_index(drop=True)
            avail_data = cs_model['avail'].reset_index(drop=True)
            
            # ===================================================================
            # ESTIMATE MODEL
            # ===================================================================
            
            print(f"    Estimating mixed model...")
            model_mixed, elapsed_mixed, success_mixed = estimate_mixed_logit(
                X_data,
                y_data,
                ids_data,
                alts_data,
                avail_data,
                model_vars_for_mixed_dm,
                randvars_available,
                verbose=True,
            )
            
            if not success_mixed or model_mixed is None:
                print(f"  ✗ Estimation failed")
                fail_count += 1
                continue
            
            # Extract results
            coef_df_mixed, results_mixed, success = extract_results(model_mixed, y_data, ids_data, alts_data)
            
            if not success or coef_df_mixed is None:
                print(f"  ✗ Results extraction failed")
                fail_count += 1
                continue
            
            # ===================================================================
            # SAVE OUTPUTS
            # ===================================================================
            
            pass_1_name = f"{model_name}_Mixed"
            
            # Summary
            try:
                summary_lines = []
                summary_lines.append(f"{'='*70}")
                summary_lines.append(f"MODEL SUMMARY: {pass_1_name}")
                summary_lines.append(f"{'='*70}")
                summary_lines.append(f"Scenario: {args.scenario}")
                summary_lines.append(f"Fixed Effects: {fe_vars}")
                summary_lines.append(f"")
                summary_lines.append(f"FIT STATISTICS:")
                summary_lines.append(f"  Log-Likelihood: {results_mixed['LL']:.4f}")
                summary_lines.append(f"  AIC: {results_mixed['AIC']:.4f}")
                summary_lines.append(f"  BIC: {results_mixed['BIC']:.4f}")
                summary_lines.append(f"  N Observations: {results_mixed['N_obs']}")
                summary_lines.append(f"  N Choice Situations: {results_mixed['N_cov']}")
                summary_lines.append(f"")
                summary_lines.append(f"COEFFICIENTS:")
                summary_lines.append(f"{'Variable':<30} {'Estimate':>12} {'Std.Err':>12} {'t-stat':>10} {'p-value':>10}")
                summary_lines.append(f"{'-'*70}")
                for var in coef_df_mixed.index:
                    est = coef_df_mixed.loc[var, 'Estimate']
                    se = coef_df_mixed.loc[var, 'Std.Err']
                    ts = coef_df_mixed.loc[var, 't-stat']
                    pv = coef_df_mixed.loc[var, 'p-value']
                    summary_lines.append(f"{var:<30} {est:>12.6f} {se:>12.6f} {ts:>10.4f} {pv:>10.4f}")
                
                summary_text = "\n".join(summary_lines)
                summary_file = output_dir / f"{pass_1_name}_{args.scenario}_summary.txt"
                with open(summary_file, 'w') as f:
                    f.write(summary_text)
                print(f"    ✓ Summary: {summary_file.name}")
            except Exception as e:
                print(f"    Note: Could not save summary ({str(e)[:50]})")
            
            # WTP
            try:
                wtp_df = calculate_wtp(coef_df_mixed)
                if wtp_df is not None:
                    wtp_export = wtp_df.copy()
                    wtp_export['Variable'] = wtp_export.index
                    wtp_export = wtp_export[['Variable', 'WTP', 'Std.Err', 't-stat', 'p-value']]
                    wtp_export['Scenario'] = args.scenario
                    wtp_export['Model'] = pass_1_name
                    
                    wtp_csv_file = output_dir / f"{pass_1_name}_{args.scenario}_wtp.csv"
                    wtp_export.to_csv(wtp_csv_file, index=False)
                    print(f"    ✓ WTP: {wtp_csv_file.name}")
            except Exception as e:
                print(f"    Note: Could not save WTP ({str(e)[:50]})")
            
            # Coefficients
            try:
                coef_export = coef_df_mixed.copy()
                coef_export['Variable'] = coef_export.index
                coef_export = coef_export[['Variable', 'Estimate', 'Std.Err', 't-stat', 'p-value']]
                coef_export['Scenario'] = args.scenario
                coef_export['Model'] = pass_1_name
                
                coef_file = output_dir / f"{pass_1_name}_{args.scenario}_coefficients.csv"
                coef_export.to_csv(coef_file, index=False)
                print(f"    ✓ Coefficients: {coef_file.name}")
            except Exception as e:
                print(f"    Note: Could not save coefficients ({str(e)[:50]})")
            
            sig_count = (coef_df_mixed['p-value'] < 0.05).sum()
            print(f"  ✓ Success - LL: {results_mixed['LL']:.2f}, Sig(p<0.05): {sig_count}/{len(coef_df_mixed)}")
            success_count += 1
            
        except Exception as e:
            print(f"  ✗ Error: {type(e).__name__}: {str(e)[:100]}")
            traceback.print_exc()
            fail_count += 1
            continue
    
    # =========================================================================
    # GENERATE AGGREGATED SUMMARY
    # =========================================================================
    
    print("\n" + "=" * 70)
    print("Generating aggregated summary file...")
    print("=" * 70)
    
    try:
        summary_rows = []
        coef_files = sorted(output_dir.glob("*_coefficients.csv"))
        
        for coef_file in coef_files:
            try:
                filename = coef_file.stem
                parts = filename.rsplit("_coefficients", 1)[0]
                
                if "_Mixed_" in parts:
                    model_name, scenario = parts.split("_Mixed_", 1)
                else:
                    continue
                
                summary_file = coef_file.parent / f"{model_name}_Mixed_{scenario}_summary.txt"
                if not summary_file.exists():
                    continue
                
                summary_text = summary_file.read_text()
                
                ll = n_obs = n_choice_sits = aic = bic = elapsed_time = None
                
                for line in summary_text.split('\n'):
                    if "Log-Likelihood:" in line and "=" in line:
                        try:
                            ll = float(line.split("=")[-1].strip().split()[0])
                        except:
                            pass
                    elif "N. Observations:" in line:
                        try:
                            n_obs = int(line.split(":")[-1].strip())
                        except:
                            pass
                    elif "N. Choice" in line and "Situations" in line:
                        try:
                            n_choice_sits = int(line.split(":")[-1].strip())
                        except:
                            pass
                    elif "AIC:" in line:
                        try:
                            aic = float(line.split(":")[-1].strip())
                        except:
                            pass
                    elif "BIC:" in line:
                        try:
                            bic = float(line.split(":")[-1].strip())
                        except:
                            pass
                
                summary_rows.append({
                    'Model': model_name,
                    'Scenario': scenario,
                    'N_obs': n_obs,
                    'N_choice_sits': n_choice_sits,
                    'LL': ll,
                    'AIC': aic,
                    'BIC': bic,
                })
            except Exception as e:
                print(f"  Warning: Could not process {coef_file.name}: {e}")
        
        if summary_rows:
            summary_df = pd.DataFrame(summary_rows)
            summary_csv = output_dir / "all_models_summary.csv"
            summary_df.to_csv(summary_csv, index=False)
            print(f"✓ Aggregated summary: {summary_csv.name}")
    except Exception as e:
        print(f"Error generating summary: {e}")
    
    # =========================================================================
    # FINAL SUMMARY
    # =========================================================================
    
    print(f"\n" + "=" * 70)
    print(f"BATCH COMPLETE: {success_count} success, {fail_count} failed")
    print("=" * 70)
    
    if fail_count > 0:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()