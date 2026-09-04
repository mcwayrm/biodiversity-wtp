# scripts/python/12_individual_wtp.py
# ===========================================================================
# INDIVIDUAL-LEVEL (CONDITIONAL/BAYESIAN) WTP
# ===========================================================================
# xlogit only estimates POPULATION-level random-coefficient parameters
# (mean b, sd w) -- it has no built-in method for individual-specific
# coefficients (checked xlogit.mixed_logit.MixedLogit: only fit()/predict()
# exist). This script adds that step ourselves, using the standard
# conditional (Bayesian) estimator from Revelt & Train (1998) / Train,
# "Discrete Choice Methods with Simulation", ch. 11 -- the same formula
# `mlogit`/`gmnl`/`Apollo` use in R, just not tied to any of those packages:
#
#   E[beta_n | choices_n] ~= sum_r [ L_n(beta_r) * beta_r ] / sum_r [ L_n(beta_r) ]
#
# For each individual n, simulate R draws beta_r ~ N(b, w) of the random
# coefficient (population mean/sd from the fitted model), compute the JOINT
# likelihood L_n(beta_r) of that individual's entire observed sequence of
# choices under each draw (other coefficients held at their fixed point
# estimates), then take the likelihood-weighted average of the draws. That
# weighted average is individual n's posterior coefficient, which converts
# to individual WTP the same way the population coefficient does (divide by
# the price coefficient, rescale to INR).
#
# Reuses the *_demeaned_sampled.parquet file already written by
# 11b_estimate_rum_xlogit.py (save_demeaned=True) -- this is the exact
# sampled+demeaned data the model was fit on, so X is reconstructed
# byte-for-byte the same way (same standardize_data() z-scoring, applied
# fresh here since only the fitted coefficients -- not the per-column
# mean/std used at fit time -- are saved to disk; recomputing them from this
# file reproduces the same values because standardize_data() is a pure
# function of the data, and this IS the same data). No re-estimation.
#
# Scope (see conversation): main spec only
# (ER_indxyear-sitexseason-hour_choiceset10, Mixed), panel unit = user_id
# (pools all of a person's trips across years, not just within one year).
# ===========================================================================

import numpy as np
import pandas as pd
from pathlib import Path


def standardize_data(X_data, eps=1e-10):
    """Same z-scoring as estimate_rum_xlogit.py's standardize_data() -- kept
    duplicated (not imported) so this script has no import-order dependency
    on the estimation script; it's a pure, trivial function so drift risk is
    low, but if you change one, change both."""
    X_scaled = X_data.copy()
    for col in X_data.columns:
        mean_val = X_data[col].mean()
        std_val = X_data[col].std()
        if std_val > eps:
            X_scaled[col] = (X_data[col] - mean_val) / std_val
    return X_scaled


def compute_individual_wtp(scenario,
                            model_name="ER_indxyear-sitexseason-hour_choiceset10",
                            model_type="Mixed",
                            random_var_dm="expected_richness_dm",
                            price_var_dm="log_travel_cost_dm",
                            n_draws=500,
                            batch_size=20000,
                            random_state=42,
                            models_dir="output/models"):
    """Compute individual (per-user_id) conditional WTP for one model/scenario.

    Returns the output Path (whether freshly computed or already cached).
    """
    models_dir = Path(models_dir)
    output_prefix = f"{model_name}_{model_type}_{scenario}"
    demeaned_path = models_dir / "demeaned" / f"{output_prefix}_demeaned_sampled.parquet"
    coef_path = models_dir / f"{output_prefix}_coefficients.csv"
    model_data_dir = models_dir / f"model_data_{scenario}"
    output_path = models_dir / f"{output_prefix}_individual_wtp.parquet"

    if output_path.exists():
        print(f"    Output already exists, skipping: {output_path.name}")
        return output_path

    if not demeaned_path.exists():
        raise FileNotFoundError(
            f"Demeaned+sampled data not found: {demeaned_path}\n"
            "(requires the population model to have been estimated with save_demeaned=True)"
        )
    if not coef_path.exists():
        raise FileNotFoundError(f"Coefficients not found: {coef_path}")

    print(f"[Individual WTP] {output_prefix}")
    print(f"    Loading demeaned data: {demeaned_path.name}")
    cs_model = pd.read_parquet(demeaned_path)

    dm_cols = [c for c in cs_model.columns if c.endswith("_dm")]
    if random_var_dm not in dm_cols:
        raise ValueError(f"{random_var_dm} not among demeaned columns: {dm_cols}")
    if price_var_dm not in dm_cols:
        raise ValueError(f"{price_var_dm} not among demeaned columns: {dm_cols}")

    X_data = cs_model[dm_cols].fillna(0).reset_index(drop=True)
    # Raw-scale (pre-standardization) std per variable -- same quantity
    # estimate_rum_xlogit.py captures as attr_stds, used the same way here
    # to convert the standardized-scale posterior coefficient back to
    # currency-comparable units.
    attr_stds = {c: X_data[c].std() for c in dm_cols}
    X_scaled = standardize_data(X_data)

    coefs = pd.read_csv(coef_path)
    coef_mean = dict(zip(coefs["Variable"], coefs["Estimate"]))
    sd_row = coefs.loc[coefs["Variable"] == f"sd.{random_var_dm}"]
    if sd_row.empty:
        raise ValueError(
            f"No 'sd.{random_var_dm}' row in {coef_path.name} -- "
            f"{random_var_dm} must be a mixed_vars random coefficient in this model."
        )
    beta_mean = float(coef_mean[random_var_dm])
    # The sign of the "sd." coefficient isn't identified in mixed logit (only
    # its square enters the likelihood, since it scales a symmetric normal
    # draw) -- xlogit can converge to either sign, so take abs() to get an
    # actual standard deviation for simulation.
    beta_sd = abs(float(sd_row["Estimate"].iloc[0]))
    beta_price = float(coef_mean[price_var_dm])

    # ---- Fixed (non-random) utility component, vectorized across rows ----
    fixed_vars = [c for c in dm_cols if c != random_var_dm]
    U_fixed = np.zeros(len(X_scaled), dtype=np.float64)
    for v in fixed_vars:
        U_fixed += X_scaled[v].to_numpy() * float(coef_mean[v])
    x_rand = X_scaled[random_var_dm].to_numpy()

    avail = cs_model["avail"].to_numpy(dtype=np.float64)
    choice = cs_model["choice"].to_numpy(dtype=np.float64)
    user_id = cs_model["user_id"].to_numpy()
    obs_seq = cs_model["obs_id_num_seq"].to_numpy()
    alt_id = cs_model["alt_id"].to_numpy()

    # ---- Reshape to (n_choice_situations, K) -- choice sets are a fixed
    # size K (sample_choice_set() in 11b pads/truncates every trip to
    # exactly choice_set_sample_size), which lets utilities/probabilities
    # be computed as dense array ops instead of a per-situation Python loop.
    order = np.lexsort((alt_id, obs_seq))
    situations, first_idx, counts = np.unique(obs_seq[order], return_index=True, return_counts=True)
    K = int(counts[0])
    if not np.all(counts == K):
        raise ValueError("Choice sets are not a uniform size -- reshape assumption violated")
    n_situations = len(situations)

    U_fixed_r = U_fixed[order].reshape(n_situations, K)
    x_rand_r = x_rand[order].reshape(n_situations, K)
    avail_r = avail[order].reshape(n_situations, K)
    choice_r = choice[order].reshape(n_situations, K)
    user_by_situation = user_id[order].reshape(n_situations, K)[:, 0]
    chosen_idx = np.argmax(choice_r, axis=1)

    print(f"    {n_situations:,} choice situations, {len(np.unique(user_by_situation)):,} users, K={K} alts/situation")
    print(f"    Simulating {n_draws} draws of {random_var_dm} ~ N({beta_mean:.4f}, {beta_sd:.4f}) [standardized scale]")

    rng = np.random.default_rng(random_state)
    draws = rng.normal(loc=beta_mean, scale=beta_sd, size=n_draws)

    # ---- Per-situation, per-draw log-probability of the CHOSEN alternative,
    # batched over choice situations to bound peak memory (a single
    # n_situations x K x n_draws array would be tens of GB at this data's
    # scale) ----
    NEG_INF = -1e10
    logp_chosen = np.empty((n_situations, n_draws), dtype=np.float64)
    for start in range(0, n_situations, batch_size):
        end = min(start + batch_size, n_situations)
        Uf = U_fixed_r[start:end]   # (b, K)
        xr = x_rand_r[start:end]    # (b, K)
        av = avail_r[start:end]     # (b, K)
        ch = chosen_idx[start:end]  # (b,)

        U = Uf[:, :, None] + xr[:, :, None] * draws[None, None, :]  # (b, K, R)
        U = np.where(av[:, :, None] > 0, U, NEG_INF)

        Umax = U.max(axis=1, keepdims=True)                          # (b, 1, R)
        logsumexp = Umax[:, 0, :] + np.log(np.exp(U - Umax).sum(axis=1))  # (b, R)
        U_chosen = U[np.arange(end - start), ch, :]                  # (b, R)
        logp_chosen[start:end] = U_chosen - logsumexp

    # ---- Aggregate to the individual (user_id) panel: sum log-likelihood
    # across all of a person's choice situations, THEN weight the draws --
    # equivalent to multiplying probabilities across situations, but stable
    # for users with many trips (where the raw product could underflow) ----
    ll_df = pd.DataFrame(logp_chosen)
    ll_df["user_id"] = user_by_situation
    grouped = ll_df.groupby("user_id")
    user_loglik = grouped.sum()
    n_trips = grouped.size()

    ll_vals = user_loglik.to_numpy()          # (n_users, R)
    ll_max = ll_vals.max(axis=1, keepdims=True)
    w = np.exp(ll_vals - ll_max)
    weights = w / w.sum(axis=1, keepdims=True)  # normalized posterior weights per draw

    beta_n = (weights * draws[None, :]).sum(axis=1)
    beta_n_var = (weights * (draws[None, :] - beta_n[:, None]) ** 2).sum(axis=1)
    beta_n_sd = np.sqrt(beta_n_var)

    # ---- Convert standardized-scale posterior coefficient to individual
    # WTP (2021 INR), same conversion calculate_wtp() applies to the
    # population coefficient: raw-scale via attr_stds, then divide by the
    # (population, fixed) price coefficient, then rescale by the reference
    # price level. beta_n_sd is only the WITHIN-individual posterior spread
    # (how much the simulation trusts this person's own point estimate) --
    # it does NOT include the population-level sampling uncertainty in
    # beta_price, so wtp_richness_sd_inr understates total uncertainty and
    # should be read as a relative/ranking measure across individuals, not
    # a full standard error. ----
    std_attr = attr_stds[random_var_dm]
    std_price = attr_stds[price_var_dm]
    beta_n_raw = beta_n / std_attr
    beta_n_sd_raw = beta_n_sd / std_attr
    beta_price_raw = beta_price / std_price

    ref_price = pd.read_parquet(model_data_dir, columns=["travel_cost_combined"])["travel_cost_combined"].mean()

    wtp_n = (beta_n_raw / (-beta_price_raw)) * ref_price
    wtp_n_sd = (beta_n_sd_raw / abs(beta_price_raw)) * ref_price

    result = pd.DataFrame({
        "user_id": user_loglik.index.to_numpy(),
        "n_trips": n_trips.to_numpy(),
        "beta_richness_utils": beta_n,
        "beta_richness_sd_utils": beta_n_sd,
        "wtp_richness_2021inr": wtp_n,
        "wtp_richness_sd_2021inr": wtp_n_sd,
        "scenario": scenario,
        "model": f"{model_name}_{model_type}",
    })

    output_path.parent.mkdir(parents=True, exist_ok=True)
    result.to_parquet(output_path, index=False)
    print(f"    Saved: {output_path.name} ({len(result):,} individuals)")
    return output_path


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Compute individual-level conditional WTP")
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--model-name", default="ER_indxyear-sitexseason-hour_choiceset10")
    parser.add_argument("--model-type", default="Mixed")
    parser.add_argument("--random-var-dm", default="expected_richness_dm")
    parser.add_argument("--price-var-dm", default="log_travel_cost_dm")
    parser.add_argument("--n-draws", type=int, default=500)
    parser.add_argument("--batch-size", type=int, default=20000)
    parser.add_argument("--random-state", type=int, default=42)
    parser.add_argument("--models-dir", default="output/models")
    args = parser.parse_args()

    compute_individual_wtp(
        scenario=args.scenario,
        model_name=args.model_name,
        model_type=args.model_type,
        random_var_dm=args.random_var_dm,
        price_var_dm=args.price_var_dm,
        n_draws=args.n_draws,
        batch_size=args.batch_size,
        random_state=args.random_state,
        models_dir=args.models_dir,
    )
