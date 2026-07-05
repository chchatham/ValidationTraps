library(MASS)

# Helper: generate drive + biomarker + COA for a given length
.generate_series <- function(n, dt, ar_coef, drive_sd, n_impulses,
                             tau_bio, noise_bio, noise_coa, coa_unique_sd,
                             coa_configs) {
  drive <- numeric(n)
  drive[1] <- rnorm(1)
  for (i in 2:n) {
    drive[i] <- ar_coef * drive[i - 1] + rnorm(1, sd = drive_sd)
  }
  imp_pool <- 15:(n - 20)
  if (length(imp_pool) >= n_impulses) {
    impulse_times <- sort(sample(imp_pool, n_impulses))
    for (ti in impulse_times) {
      drive[ti] <- drive[ti] + sample(c(-1, 1), 1) * runif(1, 2.5, 5)
    }
  }

  bio_raw <- numeric(n)
  for (i in seq_len(n)) {
    weights <- exp(-((i - seq_len(i)) * dt) / tau_bio)
    weights <- weights / sum(weights)
    bio_raw[i] <- sum(drive[seq_len(i)] * weights)
  }
  biomarker <- bio_raw + rnorm(n, sd = noise_bio)
  bio_z <- (biomarker - mean(biomarker)) / sd(biomarker)

  coa_unique <- numeric(n)
  coa_unique[1] <- rnorm(1, sd = coa_unique_sd)
  for (i in 2:n) {
    coa_unique[i] <- 0.92 * coa_unique[i - 1] + rnorm(1, sd = coa_unique_sd * 0.4)
  }

  variants <- lapply(coa_configs, function(cfg) {
    kernel_len <- min(cfg$recall_period, n)
    kernel_lags <- seq(0, kernel_len - 1)
    kernel <- dgamma(kernel_lags, shape = cfg$recall_shape, rate = cfg$recall_rate)
    if (max(kernel) > 0) kernel <- kernel / sum(kernel)

    coa_raw <- numeric(n)
    for (i in seq_len(n)) {
      lookback <- min(i, kernel_len)
      idx <- seq(i, i - lookback + 1)
      w <- kernel[seq_len(lookback)]
      sw <- sum(w)
      if (sw < 1e-12) {
        coa_raw[i] <- bio_raw[i]
      } else {
        w <- w / sw
        coa_raw[i] <- sum(bio_raw[idx] * w)
      }
    }
    coa <- coa_raw + coa_unique + rnorm(n, sd = noise_coa)
    coa_z <- (coa - mean(coa)) / sd(coa)
    kernel_com <- sum(kernel_lags * kernel) / sum(kernel)

    list(
      label = cfg$label,
      recall_period = cfg$recall_period,
      coa = coa, coa_z = coa_z,
      kernel = kernel, kernel_lags = kernel_lags,
      kernel_com = kernel_com
    )
  })

  list(biomarker = biomarker, bio_raw = bio_raw, bio_z = bio_z,
       drive = drive, coa_unique = coa_unique, variants = variants)
}

generate_figure5 <- function(
  n_timepoints = 300,
  n_timepoints_ccf = 10000,
  dt = 1,
  ar_coef = 0.75,
  drive_sd = 0.7,
  n_impulses = 8,
  tau_bio = 0.2,
  noise_bio = 0.08,
  noise_coa = 0.02,
  coa_unique_sd = 0.08,
  max_lag = 60,
  coa_configs = NULL,
  seed = 42
) {
  if (is.null(coa_configs)) {
    coa_configs <- list(
      list(label = "7-day recall",  recall_period = 7,  recall_shape = 2.0, recall_rate = 0.40),
      list(label = "28-day recall", recall_period = 28, recall_shape = 2.5, recall_rate = 0.12),
      list(label = "42-day recall", recall_period = 42, recall_shape = 3.0, recall_rate = 0.08)
    )
  }

  common_args <- list(dt = dt, ar_coef = ar_coef, drive_sd = drive_sd,
                      n_impulses = n_impulses, tau_bio = tau_bio,
                      noise_bio = noise_bio, noise_coa = noise_coa,
                      coa_unique_sd = coa_unique_sd, coa_configs = coa_configs)

  # --- Short series for display (Panel A) ---
  set.seed(seed)
  display <- do.call(.generate_series, c(list(n = n_timepoints), common_args))

  # --- Long series for asymptotic CCF (Panel C) ---
  set.seed(seed + 1000)
  n_impulses_long <- round(n_impulses * n_timepoints_ccf / n_timepoints)
  long <- do.call(.generate_series,
    c(list(n = n_timepoints_ccf),
      modifyList(common_args, list(n_impulses = n_impulses_long))))

  # --- Compute CCF from the long series ---
  lags <- seq(-max_lag, max_lag)

  coa_variants <- lapply(seq_along(coa_configs), function(vi) {
    dv <- display$variants[[vi]]
    lv <- long$variants[[vi]]

    ccf_values <- numeric(length(lags))
    for (li in seq_along(lags)) {
      lag <- lags[li]
      if (lag >= 0) {
        n_overlap <- n_timepoints_ccf - abs(lag)
        ccf_values[li] <- sum(long$bio_z[1:n_overlap] *
          lv$coa_z[(1 + lag):(n_overlap + lag)]) / (n_overlap - 1)
      } else {
        alag <- abs(lag)
        n_overlap <- n_timepoints_ccf - alag
        ccf_values[li] <- sum(long$bio_z[(1 + alag):(n_overlap + alag)] *
          lv$coa_z[1:n_overlap]) / (n_overlap - 1)
      }
    }

    peak_idx <- which.max(ccf_values)

    list(
      label = dv$label,
      recall_period = dv$recall_period,
      coa = dv$coa,
      coa_z = dv$coa_z,
      kernel = dv$kernel,
      kernel_lags = dv$kernel_lags,
      kernel_com = dv$kernel_com,
      ccf = ccf_values,
      peak_lag = lags[peak_idx],
      peak_r = ccf_values[peak_idx],
      r_at_zero = ccf_values[lags == 0]
    )
  })

  t <- seq_len(n_timepoints) * dt

  list(
    t = t,
    biomarker = display$biomarker,
    bio_raw = display$bio_raw,
    bio_z = display$bio_z,
    drive = display$drive,
    coa_unique = display$coa_unique,
    lags = lags,
    coa_variants = coa_variants
  )
}
