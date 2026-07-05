# Trap 4 prototype simulations — "The False Alarm: Innocent Attenuation of Change Correlations"
# Pure functions. Each returns a list containing raw data/derived structures AND summary stats.
# Contract mirrors R/trap1_sim.R .. R/trap3_sim.R for forward-compatible promotion to R/.
# See prototype/trap4/PLAN.md for the generative-model rationale.

# ------------------------------------------------------------------------------
# Panel A — Latent-group heterogeneity (Simpson's paradox in change-space).
# Each latent subgroup has its OWN slope, intercept, and x-centre, with overlapping x-ranges, so the
# picture looks realistic rather than four tidy parallel bands. The between-group trend is solved
# analytically to null the pooled covariance: within each subtype ΔX tracks ΔCOA, but the aggregate is
# flat. (Law of total covariance: pooled cov = mean within-group cov + cov of group centres.)
# ------------------------------------------------------------------------------
simulate_trap4_groups <- function(
  n_per_group = 150,
  slopes = c(1.35, 0.55, 0.95, 0.40),
  x_centers = c(-1.7, -0.6, 0.6, 1.7),
  x_sd = 0.85, within_r = 0.78,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  n_groups <- length(slopes)

  # Residual sd per group to hit the target within-group correlation given that group's slope:
  # within_r = b*x_sd / sqrt(b^2 x_sd^2 + resid^2)  ->  resid = |b|*x_sd*sqrt(1/within_r^2 - 1)
  resid_sd <- abs(slopes) * x_sd * sqrt(1 / within_r^2 - 1)

  # Between-group trend chosen so the pooled covariance ~ 0 (centres assumed mean-zero in x).
  within_cov <- mean(slopes * x_sd^2)
  denom <- mean(x_centers^2)
  between_slope <- if (denom > 0) -within_cov / denom else 0
  y_centers <- between_slope * x_centers
  intercepts <- y_centers - slopes * x_centers

  parts <- lapply(seq_len(n_groups), function(g) {
    x <- rnorm(n_per_group, x_centers[g], x_sd)
    y <- intercepts[g] + slopes[g] * x + rnorm(n_per_group, 0, resid_sd[g])
    data.frame(group = factor(g), dX = x, dY = y)
  })
  data <- do.call(rbind, parts)
  within_rs <- vapply(split(data, data$group),
                      function(d) cor(d$dX, d$dY), numeric(1))

  list(
    data = data,
    stats = list(
      pooled_r = cor(data$dX, data$dY),
      within_r_mean = mean(within_rs),
      within_r = within_rs,
      slopes = slopes,
      n_groups = n_groups
    )
  )
}

# ------------------------------------------------------------------------------
# Panel B — Range-localized biomarker × nonisotropic COA decimation.
# TWO things co-occur at the LOW end of the latent trait: (1) the biomarker has its greatest dynamic
# range / sensitivity there and is nearly flat elsewhere (a range-localized marker, NOT a uniform
# tracker); (2) the COA quantizes the trait into unequal ordinal bins that are COARSE (wide) exactly at
# that low end. So where the biomarker validly registers change, the COA cannot resolve it -> the
# observed ΔBiomarker–ΔCOA correlation is low, yet within its active window the biomarker faithfully
# tracks true latent change.
# ------------------------------------------------------------------------------
simulate_trap4_decimation <- function(
  n = 700, n_bins = 10,
  L_center = 4.2, L_sd = 2.0, dL_sd = 1.0,
  warp_exp = 0.5,
  gain_center = 3.0, gain_width = 1.0, gain_floor = 0.08,
  track_noise = 0.42, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  L1 <- rnorm(n, mean = L_center, sd = L_sd)
  dL <- rnorm(n, mean = 0, sd = dL_sd)
  L2 <- L1 + dL

  # Biomarker gain concentrated at LOW latent (sigmoid: ~1 below gain_center, -> gain_floor above).
  gain_fun <- function(L) gain_floor + (1 - gain_floor) * plogis(-(L - gain_center) / gain_width)
  gain <- gain_fun(L1)
  dX <- gain * dL + track_noise * rnorm(n, 0, dL_sd)   # tracks latent change only where gain is high

  # Unequal bin edges on [0, 10]: warp_exp < 1 -> wide/coarse bins at the low end, fine bins high.
  raw <- seq(0, 1, length.out = n_bins + 1)
  warped <- raw^warp_exp
  finite_edges <- 10 * warped
  edges <- finite_edges
  edges[1] <- -Inf
  edges[length(edges)] <- Inf

  bin_of <- function(v) as.integer(cut(v, breaks = edges, labels = FALSE, right = FALSE))
  dCOA <- bin_of(L2) - bin_of(L1)

  mapping <- data.frame(
    edge_lo = head(finite_edges, -1),
    edge_hi = tail(finite_edges, -1),
    bin = seq_len(n_bins)
  )
  mapping$mid <- (mapping$edge_lo + mapping$edge_hi) / 2
  mapping$width <- mapping$edge_hi - mapping$edge_lo
  mapping$resolution <- 1 / mapping$width               # bins per unit latent (COA resolution)

  low_mask <- L1 < gain_center

  list(
    data = data.frame(dX = dX, dL = dL, dCOA = dCOA, L1 = L1, L2 = L2,
                      gain = gain, low_range = low_mask),
    mapping = mapping,
    gain_fun = gain_fun,
    stats = list(
      r_binned_obs = cor(dX, dCOA),                      # observed (disappointing) correlation
      r_latent_lowrange = cor(dX[low_mask], dL[low_mask]),   # honest tracking in the active window
      r_latent_highrange = cor(dX[!low_mask], dL[!low_mask]),
      r_latent_overall = cor(dX, dL),
      pct_low = mean(low_mask),
      gain_center = gain_center,
      n_bins = n_bins
    )
  )
}

# ------------------------------------------------------------------------------
# Panel C — Construct dilution across factors (multidimensional loading geometry).
# The COA spans k latent factors. The biomarker is a linear combination with a loading vector w that
# is concentrated (and mixed-sign) rather than aligned with the equal-weight total-score direction
# (1,1,...,1). The Δbiomarker–Δtotal correlation equals the cosine between w and the diagonal, which
# shrinks as the dimensionality k grows — even though the biomarker tracks its dominant factor strongly.
# ------------------------------------------------------------------------------
simulate_trap4_dilution <- function(
  n = 800, k_factors = 10,
  loadings = NULL, noise_sd = 0.62, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(loadings)) {
    # First three (shown in the 3-D panel) are distinct and mixed-sign; the rest are near-zero.
    loadings <- c(0.85, -0.30, 0.25, rep(0.05, max(0, k_factors - 3)))
  }
  loadings <- loadings[seq_len(k_factors)]

  Fac <- matrix(rnorm(n * k_factors), n, k_factors)   # factor changes (iid latent innovations)
  dX <- as.numeric(Fac %*% loadings) + rnorm(n, 0, noise_sd)
  total <- rowSums(Fac)

  # Angle between the loading vector and the equal-weight diagonal (theoretical dilution).
  diag_dir <- rep(1, k_factors) / sqrt(k_factors)
  cos_to_total <- sum(loadings * diag_dir) / sqrt(sum(loadings^2))

  list(
    data = data.frame(dX = dX, total = total,
                      f1 = Fac[, 1], f2 = Fac[, 2], f3 = Fac[, 3]),
    stats = list(
      r_target = cor(dX, Fac[, 1]),
      r_total = cor(dX, total),
      cos_to_total = cos_to_total,
      loadings = loadings,
      loadings_shown = loadings[1:3],
      k_factors = k_factors
    )
  )
}

# ------------------------------------------------------------------------------
# Panel D — Temporal weighting mismatch (autocorrelation of two measures).
# Both the biomarker and the COA are causal weightings of the same latent innovation stream, but with
# DIFFERENT temporal kernels: the COA integrates the state over a recall window (a perceptual weighting
# function -> broad autocorrelation), while the biomarker responds transiently (short memory -> narrow
# autocorrelation). Because their memory kernels differ, changes sampled over a fixed interval correlate
# only modestly even though both track the identical underlying process. A perfect match would require
# continuously recording the biomarker and re-weighting it by the COA's recall function.
# ------------------------------------------------------------------------------
simulate_trap4_recall <- function(
  n_t = 6000,
  tau_b = 1.0,          # biomarker memory constant (short -> transient, leading)
  coa_delay = 3,        # lag before the clinical outcome manifests the latent state (progression lag)
  W_recall = 6,         # COA recall window length
  tau_c = 3,            # COA recall decay within the window
  noise_b = 0,          # independent measurement noise sd for biomarker (relative to signal sd)
  noise_c = 0,          # independent measurement noise sd for COA (relative to signal sd)
  sample_interval = 4,  # fixed Δ sampling interval
  lag_max = 10, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  U <- rnorm(n_t)                                   # latent innovations (white)
  K <- max(coa_delay + W_recall, ceiling(8 * tau_b), 20)
  tau <- 0:K

  # Biomarker: a short-memory transient response -> registers the state early (leads).
  # COA: manifests the state after coa_delay, then integrated over the recall window -> reflects the
  # state later and more diffusely (its autocorrelation encodes the recall weighting).
  k_b <- exp(-tau / tau_b)
  k_c <- ifelse(tau >= coa_delay & tau <= coa_delay + W_recall,
                exp(-(tau - coa_delay) / tau_c), 0)
  k_b_disp <- k_b / max(k_b)
  k_c_disp <- k_c / max(k_c)

  conv <- function(k) as.numeric(stats::filter(U, k, method = "convolution", sides = 1))
  b <- conv(k_b); cc <- conv(k_c)
  ok <- !is.na(b) & !is.na(cc)
  b <- b[ok]; cc <- cc[ok]
  if (noise_b > 0) b <- b + rnorm(length(b), 0, noise_b * sd(b))
  if (noise_c > 0) cc <- cc + rnorm(length(cc), 0, noise_c * sd(cc))
  N <- length(b)

  acf_b <- as.numeric(acf(b, lag.max = lag_max, plot = FALSE)$acf)
  acf_c <- as.numeric(acf(cc, lag.max = lag_max, plot = FALSE)$acf)

  # Cross-correlation surface: r(offset) = cor(b[t], cc[t + offset]). offset > 0 => COA later (biomarker
  # leads / is prognostic for future COA).
  offsets <- -lag_max:lag_max
  ccf_r <- vapply(offsets, function(o) {
    if (o >= 0) cor(b[1:(N - o)], cc[(1 + o):N])
    else { oo <- -o; cor(b[(1 + oo):N], cc[1:(N - oo)]) }
  }, numeric(1))

  m <- sample_interval
  idx <- 1:(N - m)
  dB <- b[idx + m] - b[idx]
  dC <- cc[idx + m] - cc[idx]

  peak_i <- which.max(ccf_r)

  list(
    acf = data.frame(lag = 0:lag_max, biomarker = acf_b, coa = acf_c),
    ccf = data.frame(offset = offsets, r = ccf_r),
    kernels = data.frame(tau = tau, biomarker = k_b_disp, coa = k_c_disp),
    stats = list(
      r_change = cor(dB, dC),          # contemporaneous fixed-interval Δ–Δ correlation (attenuated)
      r_level = cor(b, cc),
      peak_offset = offsets[peak_i],   # lead of biomarker over COA (>0 => prognostic)
      peak_r = ccf_r[peak_i],
      lag_max = lag_max,
      W_recall = W_recall,
      tau_b = tau_b,
      sample_interval = m
    )
  )
}
