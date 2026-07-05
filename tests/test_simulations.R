library(MASS)
library(pROC)

source("R/trap1_sim.R")
source("R/trap2_sim.R")
source("R/trap3_sim.R")
source("R/trap4_sim.R")

pass <- 0L
fail <- 0L

check <- function(desc, expr) {
  result <- tryCatch(expr, error = function(e) e)
  if (inherits(result, "error")) {
    fail <<- fail + 1L
    cat(sprintf("FAIL: %s\n  Error: %s\n", desc, result$message))
  } else if (isTRUE(result)) {
    pass <<- pass + 1L
    cat(sprintf("PASS: %s\n", desc))
  } else {
    fail <<- fail + 1L
    cat(sprintf("FAIL: %s\n  Got: %s\n", desc, deparse(result)))
  }
}

# --- Trap 1 Tests ---

cat("\n=== Trap 1: Change-Score Paradox ===\n\n")

d1 <- simulate_trap1(seed = 123)
check("Trap1 returns correct structure",
  all(c("X1", "Y1", "X2", "Y2", "stats") %in% names(d1)))

check("Trap1 returns correct length vectors",
  length(d1$X1) == 1e5 && length(d1$Y2) == 1e5)

check("Trap1 stats has all fields",
  all(c("trt_X", "trt_Y", "conv_T1", "conv_T2", "change_corr") %in% names(d1$stats)))

check("Trap1 default: high convergent validity at T1",
  d1$stats$conv_T1 > 0.8)

check("Trap1 default: high convergent validity at T2",
  d1$stats$conv_T2 > 0.7)

check("Trap1 default: low change-score correlation",
  abs(d1$stats$change_corr) < 0.3)

check("Trap1 deterministic with seed",
  identical(simulate_trap1(seed = 99)$stats$change_corr,
            simulate_trap1(seed = 99)$stats$change_corr))

d1_small <- simulate_trap1(n = 100, seed = 1)
check("Trap1 works with small n=100",
  length(d1_small$X1) == 100)

d1_zero_state <- simulate_trap1(var_S = 0, seed = 1)
check("Trap1 works with var_S=0",
  is.finite(d1_zero_state$stats$change_corr))

d1_perfect_change <- simulate_trap1(r_dT = 1, var_S = 0, var_e = 0, var_dT = 0.5, seed = 1)
check("Trap1 with r_dT=1, no noise: high change correlation",
  d1_perfect_change$stats$change_corr > 0.9)

d1_no_cross <- simulate_trap1(r_T = 0, seed = 1)
check("Trap1 works with r_T=0",
  abs(d1_no_cross$stats$conv_T1) < 0.1)

# --- Trap 2 Tests ---

cat("\n=== Trap 2: Reverse Simpson's Paradox ===\n\n")

d2 <- simulate_trap2(seed = 123)
check("Trap2 returns correct structure",
  all(c("data", "delta_X", "delta_Y", "stats") %in% names(d2)))

check("Trap2 data has correct columns",
  all(c("subject", "time", "X", "Y") %in% names(d2$data)))

check("Trap2 data has correct rows",
  nrow(d2$data) == 200 * 5)

check("Trap2 stats has all fields",
  all(c("r_cross_obs", "r_within_obs", "icc_x", "icc_y") %in% names(d2$stats)))

check("Trap2 default: weak cross-sectional correlation",
  abs(d2$stats$r_cross_obs) < 0.3)

check("Trap2 default: strong within-person correlation",
  d2$stats$r_within_obs > 0.5)

check("Trap2 deterministic with seed",
  identical(simulate_trap2(seed = 99)$stats$r_within_obs,
            simulate_trap2(seed = 99)$stats$r_within_obs))

d2_small <- simulate_trap2(n_subjects = 20, n_timepoints = 3, seed = 1)
check("Trap2 works with small n_subjects=20, n_timepoints=3",
  nrow(d2_small$data) == 60)

# --- Trap 3 Tests ---

cat("\n=== Trap 3: Portfolio Approach ===\n\n")

d3 <- simulate_trap3(seed = 123)
check("Trap3 returns correct structure",
  all(c("markers", "truth", "combined", "individual_rocs",
        "combined_roc", "stats") %in% names(d3)))

check("Trap3 markers is correct size",
  nrow(d3$markers) == 5000 && ncol(d3$markers) == 4)

check("Trap3 truth is binary",
  all(d3$truth %in% c(0, 1)))

check("Trap3 prevalence is approximately correct",
  abs(mean(d3$truth) - 0.10) < 0.03)

check("Trap3 individual_rocs has k elements",
  length(d3$individual_rocs) == 4)

check("Trap3 stats has all fields",
  all(c("individual_sens", "individual_spec", "individual_ppv",
        "individual_npv", "combined_sens", "combined_spec",
        "combined_ppv", "combined_npv") %in% names(d3$stats)))

check("Trap3 all stats are between 0 and 1",
  all(unlist(d3$stats) >= 0, na.rm = TRUE) && all(unlist(d3$stats) <= 1, na.rm = TRUE))

for (rule in c("and", "or", "majority", "weighted", "logistic")) {
  d3r <- simulate_trap3(combination_rule = rule, seed = 42)
  check(sprintf("Trap3 '%s' rule runs without error", rule),
    !is.null(d3r$combined))
}

d3_k2 <- simulate_trap3(k_markers = 2,
  sensitivities = c(0.9, 0.8), specificities = c(0.6, 0.9), seed = 1)
check("Trap3 works with k_markers=2",
  ncol(d3_k2$markers) == 2)

d3_high_prev <- simulate_trap3(prevalence = 0.5, seed = 1)
check("Trap3 works with prevalence=0.5",
  abs(mean(d3_high_prev$truth) - 0.5) < 0.05)

# --- Trap 4 Tests ---

cat("\n=== Trap 4: Expected Attenuation ===\n\n")

# Panel A — Groups
d4a <- simulate_trap4_groups(seed = 123)
check("Trap4A returns correct structure",
  all(c("data", "stats") %in% names(d4a)))

check("Trap4A data has correct columns",
  all(c("group", "dX", "dY") %in% names(d4a$data)))

check("Trap4A stats has all fields",
  all(c("pooled_r", "within_r_mean", "within_r", "slopes", "n_groups") %in% names(d4a$stats)))

check("Trap4A default: within r much higher than pooled r",
  d4a$stats$within_r_mean > 0.5 && abs(d4a$stats$pooled_r) < d4a$stats$within_r_mean)

check("Trap4A deterministic with seed",
  identical(simulate_trap4_groups(seed = 99)$stats$pooled_r,
            simulate_trap4_groups(seed = 99)$stats$pooled_r))

check("Trap4A works with small n_per_group=50",
  nrow(simulate_trap4_groups(n_per_group = 50, seed = 1)$data) == 200)

# Panel B — Decimation
d4b <- simulate_trap4_decimation(seed = 123)
check("Trap4B returns correct structure",
  all(c("data", "mapping", "gain_fun", "stats") %in% names(d4b)))

check("Trap4B stats has all fields",
  all(c("r_binned_obs", "r_latent_lowrange", "r_latent_overall", "pct_low") %in% names(d4b$stats)))

check("Trap4B default: latent r higher than binned r",
  abs(d4b$stats$r_latent_overall) > abs(d4b$stats$r_binned_obs))

check("Trap4B deterministic with seed",
  identical(simulate_trap4_decimation(seed = 99)$stats$r_binned_obs,
            simulate_trap4_decimation(seed = 99)$stats$r_binned_obs))

check("Trap4B works with few bins",
  length(simulate_trap4_decimation(n_bins = 4, seed = 1)$mapping$bin) == 4)

# Panel C — Dilution
d4c <- simulate_trap4_dilution(seed = 123)
check("Trap4C returns correct structure",
  all(c("data", "stats") %in% names(d4c)))

check("Trap4C stats has all fields",
  all(c("r_target", "r_total", "cos_to_total", "loadings", "k_factors") %in% names(d4c$stats)))

check("Trap4C default: target r higher than total r",
  abs(d4c$stats$r_target) > abs(d4c$stats$r_total))

check("Trap4C deterministic with seed",
  identical(simulate_trap4_dilution(seed = 99)$stats$r_target,
            simulate_trap4_dilution(seed = 99)$stats$r_target))

check("Trap4C works with k_factors=3",
  simulate_trap4_dilution(k_factors = 3, seed = 1)$stats$k_factors == 3)

check("Trap4C works with custom loadings",
  !is.null(simulate_trap4_dilution(k_factors = 4, loadings = c(1, 0, 0, 0), seed = 1)$stats$r_target))

# Panel C visualization — Edge-on planes
vis <- generate_trap4c_visual(seed = 42)
check("Trap4C visual returns all components",
  all(c("tiles_df", "outlines_df", "key_df", "key_origin", "coa_tip", "colorbar_df", "stats") %in% names(vis)))

check("Trap4C visual tiles_df has correct columns",
  all(c("X", "Y", "grp", "op") %in% names(vis$tiles_df)))

check("Trap4C visual tiles have valid opacity",
  all(vis$tiles_df$op >= 0.02) && all(vis$tiles_df$op <= 0.85))

check("Trap4C visual key_df has 5 trait axes by default",
  nrow(vis$key_df) == 5)

check("Trap4C visual stats match linear model",
  !is.null(vis$stats$r_target) && !is.null(vis$stats$r_total) && !is.null(vis$stats$cos_to_total))

check("Trap4C visual works with k_traits=3",
  nrow(generate_trap4c_visual(k_traits = 3, seed = 1)$key_df) == 3)

check("Trap4C visual mode_spread changes stats",
  abs(generate_trap4c_visual(mode_spread = 0.3, seed = 42)$stats$r_target -
      generate_trap4c_visual(mode_spread = 3.0, seed = 42)$stats$r_target) > 0.01)

# Panel C 3D surfaces (for plotly rendering)
surf3d <- generate_trap4c_surfaces(seed = 42)
check("Trap4C surfaces returns surfaces and stats",
  all(c("surfaces", "stats") %in% names(surf3d)))

check("Trap4C surfaces has 7 planes by default",
  length(surf3d$surfaces) == 7)

check("Trap4C surface matrices are 17x17",
  nrow(surf3d$surfaces[[1]]$x) == 17 && ncol(surf3d$surfaces[[1]]$x) == 17)

check("Trap4C surface has x/y/z/density matrices",
  all(c("x", "y", "z", "density") %in% names(surf3d$surfaces[[1]])))

check("Trap4C surface density is non-negative",
  all(surf3d$surfaces[[1]]$density >= 0))

check("Trap4C surfaces stats match linear model",
  !is.null(surf3d$stats$r_target) && !is.null(surf3d$stats$r_total))

check("Trap4C surfaces works with k_traits=3",
  length(generate_trap4c_surfaces(k_traits = 3, seed = 1)$surfaces) == 7)

check("Trap4C surfaces mode_spread changes stats",
  abs(generate_trap4c_surfaces(mode_spread = 0.3, seed = 42)$stats$r_target -
      generate_trap4c_surfaces(mode_spread = 3.0, seed = 42)$stats$r_target) > 0.01)

# Panel D — Temporal
d4d <- generate_trap4_temporal()
check("Trap4D returns correct structure",
  all(c("curves", "stats") %in% names(d4d)))

check("Trap4D curves has correct columns",
  all(c("t", "biomarker", "coa") %in% names(d4d$curves)))

check("Trap4D stats has all fields",
  all(c("bio_peak_t", "coa_peak_t", "temporal_gap") %in% names(d4d$stats)))

check("Trap4D default: biomarker peaks after t=0",
  d4d$stats$bio_peak_t > 0)

check("Trap4D default: COA peaks before t=0",
  d4d$stats$coa_peak_t < 0)

check("Trap4D curves bounded by floor and ceiling",
  all(d4d$curves$biomarker >= 0.19) && all(d4d$curves$biomarker <= 0.81) &&
  all(d4d$curves$coa >= 0.19) && all(d4d$curves$coa <= 0.81))

check("Trap4D works with different params",
  !is.null(generate_trap4_temporal(shape_bio = 2, rate_bio = 0.5,
                                    shape_coa = 2, rate_coa = 0.5)$stats$temporal_gap))

# --- Summary ---

cat(sprintf("\n=== RESULTS: %d passed, %d failed ===\n", pass, fail))
if (fail > 0) quit(status = 1)
