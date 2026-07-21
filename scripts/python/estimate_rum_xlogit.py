#!/usr/bin/env python3
"""
estimate_rum_xlogit.py - XLogit RUM Estimation

Runs mixed logit models (as specified in models.yml) on prepped choice-set
data. Called either in-process via reticulate (recommended, see
utils_xlogit_reticulate.R) or as a standalone CLI script for testing.

Demeaning (fixed-effect absorption) is computed entirely in this script:
group means are computed directly from the prepped data with pandas, so
there is no R->Python JSON hand-off anymore (that was the source of a real
bug -- R's data.table serialized to column-oriented JSON, but the old Python
code expected a nested per-group dict).

Note on FE handling: each entry in a model's fe_vars is absorbed as its own
dimension via iterative alternating-projection demeaning (demean by dim 1,
then dim 2 on the residual, then dim 3, repeat until a full cycle removes
essentially nothing) -- not flattened into one combined group. Flattening
was tried first and produced mostly-singleton groups in this project's data
(median group size of 1), which silently zeroed out ~19% of rows and made
the estimation Hessian singular.

When save_demeaned=True, the sampled + demeaned dataset used for estimation
is written to <output_dir>/demeaned/<model>_Mixed_<scenario>_demeaned.parquet
for debugging.
"""

import argparse
import gc
import hashlib
import sys
import time
import traceback
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import scipy.stats
import yaml
from xlogit import MixedLogit

warnings.filterwarnings("ignore")

SEED = 12345
PROGRESS_EVERY = 5000  # print a progress line every N trips while sampling


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def standardize_data(X_data, eps=1e-10):
    """Robust standardization: normalize to mean=0, std=1"""
    X_scaled = X_data.copy()
    for col in X_data.columns:
        mean_val = X_data[col].mean()
        std_val = X_data[col].std()
        if std_val > eps:
            X_scaled[col] = (X_data[col] - mean_val) / std_val
    return X_scaled


def get_fe_groups(fe_vars):
    """Parse models.yml fe_vars into a list of separate FE dimensions.

    Each entry in fe_vars becomes its OWN grouping to be absorbed -- a list
    like [user_id, year] means "group by the interaction of user_id and year
    together" (one dimension), not "flatten user_id and year into the same
    combined group as everything else." This matches how fe_vars is written
    in models.yml (three separate entries = three separate FE dimensions).
    """
    fe_groups = []
    for fe_spec in fe_vars:
        if isinstance(fe_spec, list):
            fe_groups.append(fe_spec)
        else:
            fe_groups.append([fe_spec])
    return fe_groups


def _short_hash(s, length=10):
    return hashlib.md5(s.encode()).hexdigest()[:length]


def fe_structure_key(fe_groups):
    """Canonical, order-independent string key for a set of FE dimensions.
    Two fe_vars specs that absorb the SAME dimensions (regardless of the
    order they're listed in models.yml, or column order within a dimension)
    produce the same key -- e.g. [[user_id,year],[cluster_id,season],
    hour_id] and [[year,user_id],hour_id,[season,cluster_id]] are the same
    structure and should share cache.
    """
    parts = sorted("-".join(sorted(g)) for g in fe_groups)
    return "+".join(parts)


def data_fingerprint(parquet_path):
    """Cheap fingerprint (row count + mtime + file size, no data read) used
    to detect whether the underlying model_data has changed since a cache
    entry was written. Embedded directly in cache filenames -- if the
    source data changes (e.g. 11a re-run with different filtering/GDP
    handling), the fingerprint changes, the filename changes, and old
    cache entries are simply never found again rather than silently
    misapplied to different data.
    """
    p = Path(parquet_path)
    stat = p.stat()
    n_rows = pq.ParquetFile(p).metadata.num_rows
    return f"{n_rows}_{int(stat.st_mtime)}_{stat.st_size}"


def fe_cache_path(fe_cache_dir, scenario, fe_groups, model_data_path, var):
    """Cache path for one (FE structure, variable) pair, keyed so that ANY
    model sharing this exact FE structure and demeaning this exact variable
    reuses the same cached result -- regardless of what OTHER variables or
    model_vars differ between the models. Does not support reuse across
    DIFFERENT FE structures (e.g. dropping one dimension) -- multi-way
    iterative demeaning doesn't decompose per-dimension, so that reuse
    wouldn't be mathematically valid.
    """
    fe_key = fe_structure_key(fe_groups)
    fp = data_fingerprint(model_data_path)
    key_hash = _short_hash(f"{fe_key}|{fp}")
    return Path(fe_cache_dir) / f"{scenario}__{key_hash}__{var}.parquet"


def demean_by_fe_iterative(df, vars_to_demean, fe_groups, max_iter=1000, tol=1e-4,
                            fe_cache_dir=None, scenario=None, model_data_path=None):
    """Absorb multiple (possibly non-nested) fixed-effect dimensions via
    Irons & Tuck (1969) accelerated alternating-projection demeaning -- the
    same acceleration technique fixest/lfe use internally for this exact
    problem (see Gaure 2013).

    Naive alternating projection (cycle through each FE dimension, subtract
    its group mean, repeat) is only fast when the FE dimensions are close to
    orthogonal. When they're correlated -- as user_id x year and
    cluster_id x season are here (specific users concentrate visits on
    specific site clusters) -- naive cycling converges extremely slowly
    (observed: shift only dropped from 0.19 to 0.026 over 450 extra
    iterations). Irons & Tuck extrapolation uses two consecutive cycle steps
    to jump toward where the sequence is actually heading, rather than
    crawling there one small step at a time. Falls back safely to the raw
    (unaccelerated) iterate if the extrapolation is unstable.

    If fe_cache_dir/scenario/model_data_path are provided, each variable's
    demeaned result is cached per (FE structure, variable) -- so a different
    model that shares this exact FE structure and demeans this same
    variable (e.g. two models both using fe_vars = [[user_id,year],
    [cluster_id,season],hour_of_day], differing only in mixed_vars) skips
    the ~hundreds-of-cycles acceleration loop entirely and loads the cached
    result instead. See fe_cache_path() for the invalidation approach.
    """
    df = df.copy()
    valid_fe_groups = [[c for c in grp if c in df.columns] for grp in fe_groups]
    valid_fe_groups = [g for g in valid_fe_groups if len(g) > 0]

    if len(valid_fe_groups) == 0:
        for var in vars_to_demean:
            if var in df.columns:
                df[f"{var}_dm"] = df[var]
        return df

    use_cache = fe_cache_dir is not None and scenario is not None and model_data_path is not None
    if use_cache:
        Path(fe_cache_dir).mkdir(parents=True, exist_ok=True)

    # Integer group codes + counts depend only on the grouping columns, not
    # the variable being demeaned -- compute once, reuse across all variables
    # (still needed even for cache hits below, since we don't know ahead of
    # time which variables will hit vs. miss)
    group_codes = []
    group_counts = []
    for grp in valid_fe_groups:
        codes = df.groupby(grp, sort=False).ngroup().to_numpy()
        counts = np.bincount(codes)
        sizes = pd.Series(counts[counts > 0])
        print(f"    FE dim {grp}: {len(sizes):,} groups, "
              f"size min={sizes.min()}, median={int(sizes.median())}, max={sizes.max()}")
        group_codes.append(codes)
        group_counts.append(counts)

    def apply_cycle(x):
        """One full pass: subtract each FE dimension's current group mean."""
        x = x.copy()
        max_shift = 0.0
        for codes, counts in zip(group_codes, group_counts):
            sums = np.bincount(codes, weights=x, minlength=len(counts))
            means = sums / counts
            x = x - means[codes]
            if means.size:
                max_shift = max(max_shift, float(np.max(np.abs(means))))
        return x, max_shift

    n_cache_hits = 0
    n_cache_misses = 0

    for var in vars_to_demean:
        if var not in df.columns:
            continue

        cache_path = None
        if use_cache:
            cache_path = fe_cache_path(fe_cache_dir, scenario, valid_fe_groups, model_data_path, var)
            if cache_path.exists():
                cached = pd.read_parquet(cache_path)
                if len(cached) == len(df):
                    df[f"{var}_dm"] = cached[f"{var}_dm"].to_numpy()
                    n_cache_hits += 1
                    print(f"      {var}: loaded from FE cache ({cache_path.name})")
                    continue
                else:
                    print(f"      {var}: cache row-count mismatch ({len(cached)} vs {len(df)}), recomputing")

        n_cache_misses += 1
        resid = df[var].to_numpy(dtype="float64", copy=True)
        scale = np.std(resid)
        tol_var = tol * scale if scale > 0 else tol

        n_iter_used = max_iter
        max_change = 0.0
        iteration = 0

        while iteration < max_iter:
            x0 = resid
            x1, shift1 = apply_cycle(x0)
            iteration += 1
            if shift1 < tol_var:
                resid, n_iter_used, max_change = x1, iteration, shift1
                break

            x2, shift2 = apply_cycle(x1)
            iteration += 1
            if shift2 < tol_var:
                resid, n_iter_used, max_change = x2, iteration, shift2
                break

            # Irons & Tuck extrapolation using the last two cycle steps
            d1 = x1 - x0
            d2 = x2 - x1
            denom = np.dot(d2 - d1, d2 - d1)
            if denom > 1e-300:
                x_accel = x2 - (np.dot(d2, d2 - d1) / denom) * d2
                # Safety net: fall back if extrapolation blew up or produced
                # non-finite values (can happen if denom is tiny but nonzero)
                if not np.all(np.isfinite(x_accel)) or \
                   np.max(np.abs(x_accel)) > 10 * max(np.max(np.abs(x2)), 1e-12):
                    x_accel = x2
            else:
                x_accel = x2

            resid = x_accel
            max_change = shift2  # re-verified from scratch next loop pass

        if n_iter_used == max_iter and max_change >= tol_var:
            print(f"    WARNING: demeaning for '{var}' did not fully converge "
                  f"in {max_iter} cycle(s) (last max shift: {max_change:.2e}, "
                  f"tolerance: {tol_var:.2e})")
        else:
            print(f"      {var}: converged in {n_iter_used} cycle(s)")

        df[f"{var}_dm"] = resid

        if use_cache and cache_path is not None:
            pd.DataFrame({f"{var}_dm": resid}).to_parquet(cache_path, index=False)

    if use_cache:
        print(f"    FE cache: {n_cache_hits} hit(s), {n_cache_misses} miss(es)")

    return df


def sample_choice_set(group, choice_set_sample_size, seed=SEED):
    """Sample a fixed-size choice set for one trip: keep the chosen
    alternative, sample down/pad the rest to choice_set_sample_size."""
    chosen = group[group["choice"] == 1]
    not_chosen = group[group["choice"] == 0]

    trip_id = group["trip_id"].iloc[0]
    trip_seed = seed + (hash(trip_id) % 10000)

    n_to_sample = max(0, choice_set_sample_size - len(chosen))
    if n_to_sample > 0 and len(not_chosen) > 0:
        sampled_not_chosen = not_chosen.sample(
            n=min(n_to_sample, len(not_chosen)), random_state=trip_seed
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


def build_model_data(cs, model_vars, fe_vars, mixed_vars, choice_set_sample_size,
                      demeaned_full_path=None, demeaned_sampled_path=None,
                      fe_cache_dir=None, scenario=None, model_data_path=None):
    """Demean by FE dimension(s), then sample a fixed-size choice set per trip.

    Three layers of caching, checked in order from cheapest to most granular:
      1. demeaned_sampled_path: the final sampled+demeaned data ready for
         xlogit. If this exists, everything below is skipped entirely.
      2. demeaned_full_path: the demeaned data BEFORE sampling. If this
         exists, only sampling re-runs.
      3. Per-variable FE cache (fe_cache_dir): if neither of the above
         exists (e.g. a genuinely new model), each variable being demeaned
         is checked individually -- a different model that shares this
         exact FE structure and demeans this same variable reuses the
         cached result instead of re-running the acceleration loop.

    This means: a downstream xlogit failure never requires redoing
    demeaning (layer 1/2); a change to choice_set_sample_size alone doesn't
    require re-demeaning (layer 2); and testing a new model spec that
    shares FE structure/variables with an existing one only pays for the
    genuinely new variables (layer 3).

    NOTE: layers 1/2 are keyed by model name only, not content -- if you
    edit fe_vars/mixed_vars/model_vars for an EXISTING model name, delete
    its cached files manually. Layer 3 is content-keyed (via
    fe_structure_key + data_fingerprint) so it invalidates automatically.

    Returns (cs_model, model_vars_for_demean).
    """
    model_vars_for_demean = [
        "log_travel_cost" if v == "travel_cost_combined" else v for v in model_vars
    ]
    vars_to_demean = list(dict.fromkeys(model_vars_for_demean + mixed_vars))

    if demeaned_sampled_path is not None and Path(demeaned_sampled_path).exists():
        print(f"    Loading cached sampled+demeaned data: {Path(demeaned_sampled_path).name}")
        cs_model = pd.read_parquet(demeaned_sampled_path)
        return cs_model, model_vars_for_demean

    if demeaned_full_path is not None and Path(demeaned_full_path).exists():
        print(f"    Loading cached demeaned data (pre-sampling): {Path(demeaned_full_path).name}")
        cs_demeaned = pd.read_parquet(demeaned_full_path)
    else:
        fe_groups = get_fe_groups(fe_vars)
        print(f"    FE dimensions (absorbed separately, iteratively): {fe_groups}")
        print(f"    Demeaning {len(vars_to_demean)} variable(s)...")
        cs_demeaned = demean_by_fe_iterative(
            cs, vars_to_demean, fe_groups,
            fe_cache_dir=fe_cache_dir, scenario=scenario, model_data_path=model_data_path,
        )
        if demeaned_full_path is not None:
            Path(demeaned_full_path).parent.mkdir(parents=True, exist_ok=True)
            cs_demeaned.to_parquet(demeaned_full_path, index=False)
            print(f"    Saved full demeaned dataset: {Path(demeaned_full_path).name}")

    print(f"    Sampling choice sets (target size {choice_set_sample_size})...")
    # Concatenate in batches rather than accumulating every trip's tiny
    # DataFrame in one giant list until a single final concat -- that pattern
    # briefly needs ~2x memory (all the fragments + the freshly concatenated
    # result at once) and carries real per-object overhead across hundreds
    # of thousands of small DataFrames. Batching keeps peak memory bounded.
    SAMPLE_BATCH_SIZE = 20000
    batch_fragments = []
    batch_results = []
    trip_count = 0
    for _, group in cs_demeaned.groupby("trip_id", sort=False):
        trip_count += 1
        batch_fragments.append(sample_choice_set(group, choice_set_sample_size))
        if len(batch_fragments) >= SAMPLE_BATCH_SIZE:
            batch_results.append(pd.concat(batch_fragments, ignore_index=True))
            batch_fragments = []
        if trip_count % PROGRESS_EVERY == 0:
            print(f"      Processed {trip_count:,} trips")

    if batch_fragments:
        batch_results.append(pd.concat(batch_fragments, ignore_index=True))
        batch_fragments = []

    # The full (pre-sampling) demeaned data is no longer needed once we have
    # the sampled fragments -- release it before the final concat rather
    # than holding both simultaneously
    del cs_demeaned

    cs_model = pd.concat(batch_results, ignore_index=True)
    del batch_results
    print(f"    Complete: {trip_count:,} trips, {len(cs_model):,} rows")

    cs_model["obs_id_num_seq"] = cs_model.groupby("obs_id_num", sort=False).ngroup()
    cs_model["alt_id"] = cs_model.groupby("obs_id_num_seq", sort=False).cumcount() + 1

    if demeaned_sampled_path is not None:
        Path(demeaned_sampled_path).parent.mkdir(parents=True, exist_ok=True)
        cs_model.to_parquet(demeaned_sampled_path, index=False)
        print(f"    Saved sampled+demeaned dataset: {Path(demeaned_sampled_path).name}")

    return cs_model, model_vars_for_demean


def estimate_model(X_data, y_data, ids_data, alts_data, avail_data, randvars, verbose=True):
    """Estimate xlogit MixedLogit model with standardization."""
    try:
        if verbose:
            print(f"    Data validation:")
            print(f"      X shape: {X_data.shape}")
            print(f"      X NaN: {X_data.isna().sum().sum()}")
            print(f"      X std range: [{X_data.std().min():.6f}, {X_data.std().max():.6f}]")

        X_scaled = standardize_data(X_data)

        if verbose:
            print(f"    Standardized (mean=0, std=1)")
            print(f"      y choices: {y_data.sum()}/{len(y_data)}")
            print(f"      Choice situations: {ids_data.nunique()}")
            print(f"    Fitting MixedLogit (random coeff, 10 Halton draws):")
            print(f"      Random vars: {list(randvars.keys())}")

        start_time = time.time()
        model = MixedLogit()
        model.fit(
            X=X_scaled, y=y_data, varnames=list(X_data.columns),
            ids=ids_data, alts=alts_data, avail=avail_data,
            randvars=randvars, n_draws=10,
            optim_method="L-BFGS-B",
            skip_std_errs=False, mnl_init=False,
        )
        elapsed = time.time() - start_time
        if verbose:
            print(f"    Estimation complete in {elapsed:.1f}s")
        return model, elapsed, True

    except Exception as e:
        print(f"    ERROR: {type(e).__name__}: {str(e)}")
        traceback.print_exc()
        return None, 0, False


def extract_results(model, y_data, ids_data):
    """Extract coefficients and fit statistics."""
    try:
        coef_df = pd.DataFrame({
            "Estimate": model.coeff_,
            "Std.Err": model.stderr,
            "p-value": model.pvalues,
        }, index=model.coeff_names)
        coef_df["t-stat"] = coef_df["Estimate"] / coef_df["Std.Err"]

        results = {
            "LL": model.loglikelihood,
            "AIC": model.aic,
            "BIC": model.bic,
            "N_obs": len(y_data),
            "N_choice_sits": ids_data.nunique(),
        }
        return coef_df, results, True
    except Exception as e:
        print(f"    ERROR extracting results: {e}")
        return None, {}, False


def calculate_wtp(coef_df, attr_stds, price_level=None):
    """Calculate WTP using the delta method, relative to log_travel_cost.

    Two corrections applied, neither of which changes t-stats/p-values
    (both are linear rescalings of the same estimate/SE ratio):

    1. Standardization: coefficients come from a model fit on STANDARDIZED
       variables, converted back to raw-scale units via attr_stds (each
       variable's std BEFORE standardization, captured at build time).

    2. Log-price: the price variable is log(travel_cost), not raw travel
       cost, so beta_attr_raw / beta_price_raw is only "utility per unit of
       log(price)," not "utility per rupee." Converting requires
       multiplying by a reference price level P (dU/dPrice = beta_logprice/P
       via the chain rule) -- pass price_level (e.g. the estimation
       sample's mean travel_cost_combined) to apply this. If price_level is
       None, WTP remains in per-log-unit terms rather than per-rupee.
    """
    try:
        price_idx = next(
            (i for i, name in enumerate(coef_df.index) if "travel_cost" in name.lower()),
            None,
        )
        if price_idx is None:
            return None

        price_var = coef_df.index[price_idx]
        std_price = attr_stds.get(price_var)
        if not std_price:
            return None

        beta_price_raw = coef_df.iloc[price_idx]["Estimate"] / std_price
        se_price_raw = coef_df.iloc[price_idx]["Std.Err"] / std_price

        rows = []
        for idx, var in enumerate(coef_df.index):
            if idx == price_idx:
                continue
            base_var = var[3:] if var.startswith("sd.") else var
            std_attr = attr_stds.get(base_var)
            if not std_attr:
                continue

            beta_attr_raw = coef_df.iloc[idx]["Estimate"] / std_attr
            se_attr_raw = coef_df.iloc[idx]["Std.Err"] / std_attr

            wtp_val = beta_attr_raw / (-beta_price_raw)
            se_wtp = np.sqrt(
                (se_attr_raw / (-beta_price_raw)) ** 2
                + (beta_attr_raw * se_price_raw / (beta_price_raw ** 2)) ** 2
            )

            if price_level is not None:
                wtp_val = wtp_val * price_level
                se_wtp = se_wtp * price_level

            t_stat = wtp_val / (se_wtp + 1e-10)
            p_val = 2 * (1 - scipy.stats.t.cdf(np.abs(t_stat), df=len(coef_df)))

            rows.append({
                "Variable": var, "WTP": wtp_val, "Std.Err": se_wtp,
                "t-stat": t_stat, "p-value": p_val,
            })

        return pd.DataFrame(rows).set_index("Variable")
    except Exception as e:
        print(f"    Error calculating WTP: {e}")
        return None


# ============================================================================
# MAIN ENTRY POINT (called directly from R via reticulate, or from CLI)
# ============================================================================

def run_all_models(scenario, input_data_path, output_dir, models_config_path="models.yml",
                    save_demeaned=True, models_subset=None):
    """Run xlogit mixed logit estimation for every model in models.yml.

    Returns a dict with success_count, fail_count, and summary_df (a
    DataFrame of LL/AIC/BIC/N_obs/N_choice_sits per model) -- built directly
    in memory as each model finishes, so there's no re-parsing of saved text
    files (the old approach silently produced blank LL/N_obs columns because
    the writer and parser used different label formats).
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    demeaned_dir = output_dir / "demeaned"
    if save_demeaned:
        demeaned_dir.mkdir(parents=True, exist_ok=True)

    with open(models_config_path, "r") as f:
        models_config = yaml.safe_load(f)

    model_names = list(models_config.keys())
    if models_subset:
        subset = models_subset.split(",") if isinstance(models_subset, str) else models_subset
        model_names = [m for m in model_names if m in subset]

    print("=" * 70)
    print("XLOGIT RUM ESTIMATION")
    print("=" * 70)
    print(f"Scenario: {scenario}")
    print(f"Input data: {input_data_path}")
    print(f"Output dir: {output_dir}")
    print(f"Models to run: {len(model_names)}\n")

    print(f"[Loading Prepped Data]")
    cs = pd.read_parquet(input_data_path)
    print(f"  Loaded: {len(cs):,} rows, {cs['trip_id'].nunique():,} trips\n")

    if "avail" not in cs.columns:
        cs["avail"] = 1

    # Reference price level for converting log-price WTP to per-rupee terms
    # (dU/dPrice = beta_logprice/P via the chain rule) -- same value used
    # across all models since they share this scenario's data
    reference_price = cs["travel_cost_combined"].mean() if "travel_cost_combined" in cs.columns else None
    if reference_price is not None:
        print(f"Reference price for WTP conversion (mean travel_cost_combined): {reference_price:.2f}\n")
    else:
        print("WARNING: 'travel_cost_combined' not found -- WTP will be in per-log-unit terms, not per-rupee\n")

    success_count = 0
    fail_count = 0
    summary_rows = []

    for model_idx, model_name in enumerate(model_names, 1):
        print(f"\n[Run {model_idx}/{len(model_names)}] Model: {model_name}_Mixed")
        output_prefix = f"{model_name}_Mixed_{scenario}"
        summary_file = output_dir / f"{output_prefix}_summary.txt"
        coef_file = output_dir / f"{output_prefix}_coefficients.csv"

        if summary_file.exists() and coef_file.exists():
            print(f"    Output files already exist, skipping...")
            success_count += 1
            continue

        try:
            model_config = models_config[model_name]
            model_vars = model_config["model_vars"].copy()
            fe_vars = model_config.get("fe_vars", [])
            mixed_vars = model_config.get("mixed_vars", [])
            choice_set_sample_size = model_config.get("choice_set_sample_size", 10)

            cs_model, model_vars_for_demean = build_model_data(
                cs, model_vars, fe_vars, mixed_vars, choice_set_sample_size,
                demeaned_full_path=(demeaned_dir / f"{output_prefix}_demeaned_full.parquet") if save_demeaned else None,
                demeaned_sampled_path=(demeaned_dir / f"{output_prefix}_demeaned_sampled.parquet") if save_demeaned else None,
                fe_cache_dir=demeaned_dir / "fe_cache",
                scenario=scenario,
                model_data_path=input_data_path,
            )

            model_vars_dm = [f"{v}_dm" for v in model_vars_for_demean]
            randvars = {f"{v}_dm": "n" for v in mixed_vars if f"{v}_dm" in model_vars_dm}

            if len(randvars) == 0:
                print(f"    ERROR: No random vars available")
                fail_count += 1
                continue

            X_data = cs_model[model_vars_dm].fillna(0).reset_index(drop=True)
            y_data = cs_model["choice"].reset_index(drop=True)
            ids_data = cs_model["obs_id_num_seq"].reset_index(drop=True)
            alts_data = cs_model["alt_id"].reset_index(drop=True)
            avail_data = cs_model["avail"].reset_index(drop=True)

            # Captured BEFORE standardize_data() runs inside estimate_model() --
            # this is what lets calculate_wtp() convert coefficients back to
            # raw-scale units afterward
            attr_stds = {col: X_data[col].std() for col in X_data.columns}

            print(f"    Estimating mixed model...")
            model, elapsed, ok = estimate_model(
                X_data, y_data, ids_data, alts_data, avail_data, randvars
            )
            if not ok or model is None:
                print(f"  Estimation failed")
                fail_count += 1
                continue

            coef_df, results, ok = extract_results(model, y_data, ids_data)
            if not ok or coef_df is None:
                print(f"  Results extraction failed")
                fail_count += 1
                continue

            # ---- Save summary.txt (kept as a human-readable record) ----
            summary_lines = [
                "=" * 70,
                f"MODEL SUMMARY: {model_name}_Mixed",
                "=" * 70,
                f"Scenario: {scenario}",
                f"Fixed Effects: {fe_vars}",
                "",
                "FIT STATISTICS:",
                f"  Log-Likelihood: {results['LL']:.4f}",
                f"  AIC: {results['AIC']:.4f}",
                f"  BIC: {results['BIC']:.4f}",
                f"  N Observations: {results['N_obs']}",
                f"  N Choice Situations: {results['N_choice_sits']}",
                "",
                "COEFFICIENTS:",
                f"{'Variable':<30} {'Estimate':>12} {'Std.Err':>12} {'t-stat':>10} {'p-value':>10}",
                "-" * 70,
            ]
            for var in coef_df.index:
                row = coef_df.loc[var]
                summary_lines.append(
                    f"{var:<30} {row['Estimate']:>12.6f} {row['Std.Err']:>12.6f} "
                    f"{row['t-stat']:>10.4f} {row['p-value']:>10.4f}"
                )
            summary_file.write_text("\n".join(summary_lines))
            print(f"    Summary: {summary_file.name}")

            # ---- Save WTP ----
            wtp_df = calculate_wtp(coef_df, attr_stds, price_level=reference_price)
            if wtp_df is not None:
                wtp_export = wtp_df.reset_index().rename(columns={"index": "Variable"})
                wtp_export["Scenario"] = scenario
                wtp_export["Model"] = f"{model_name}_Mixed"
                wtp_csv = output_dir / f"{output_prefix}_wtp.csv"
                wtp_export.to_csv(wtp_csv, index=False)
                print(f"    WTP: {wtp_csv.name}")

            # ---- Save coefficients ----
            coef_export = coef_df.reset_index().rename(columns={"index": "Variable"})
            coef_export["Scenario"] = scenario
            coef_export["Model"] = f"{model_name}_Mixed"
            coef_csv = output_dir / f"{output_prefix}_coefficients.csv"
            coef_export.to_csv(coef_csv, index=False)
            print(f"    Coefficients: {coef_csv.name}")

            # ---- Record for the in-memory aggregated summary ----
            summary_rows.append({
                "Model": model_name,
                "Scenario": scenario,
                "N_obs": results["N_obs"],
                "N_choice_sits": results["N_choice_sits"],
                "LL": results["LL"],
                "AIC": results["AIC"],
                "BIC": results["BIC"],
            })

            sig_count = (coef_df["p-value"] < 0.05).sum()
            print(f"  Success - LL: {results['LL']:.2f}, Sig(p<0.05): {sig_count}/{len(coef_df)}")
            success_count += 1

            # Release this model's large objects before moving to the next
            # model, rather than trusting refcounting + eventual GC to do it
            # before the next (potentially larger) model's memory need peaks
            del cs_model, X_data, y_data, ids_data, alts_data, avail_data, model, coef_df
            gc.collect()

        except Exception as e:
            print(f"  Error: {type(e).__name__}: {str(e)[:200]}")
            traceback.print_exc()
            fail_count += 1
            continue

    # ---- Aggregated summary across all models (built in-memory) ----
    summary_df = pd.DataFrame(summary_rows)
    if not summary_df.empty:
        summary_csv = output_dir / "all_models_summary.csv"
        summary_df.to_csv(summary_csv, index=False)
        print(f"\nAggregated summary: {summary_csv.name}")

    print("\n" + "=" * 70)
    print(f"BATCH COMPLETE: {success_count} success, {fail_count} failed")
    print("=" * 70)

    return {
        "success_count": success_count,
        "fail_count": fail_count,
        "summary_df": summary_df,
    }


# ============================================================================
# OPTIONAL STANDALONE CLI (for testing outside R/reticulate)
# ============================================================================

def _parse_cli_args():
    parser = argparse.ArgumentParser(description="XLogit RUM Estimation")
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--input-data", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--models-config", default="models.yml")
    parser.add_argument("--models-subset", default=None)
    parser.add_argument("--no-save-demeaned", action="store_true")
    return parser.parse_args()


if __name__ == "__main__":
    args = _parse_cli_args()
    result = run_all_models(
        scenario=args.scenario,
        input_data_path=args.input_data,
        output_dir=args.output_dir,
        models_config_path=args.models_config,
        save_demeaned=not args.no_save_demeaned,
        models_subset=args.models_subset,
    )
    sys.exit(1 if result["fail_count"] > 0 else 0)