simulate_trap5 <- function(
  n_display = 300,
  n_ccf = 10000,
  ar_coef = 0.75,
  drive_sd = 0.7,
  tau_bio = 0.2,
  recall_period = 42,
  recall_shape = 2.5,
  recall_rate = 0.12,
  custom_kernel = NULL,
  noise_bio = 0.08,
  noise_coa = 0.02,
  coa_unique_sd = 0.08,
  max_lag = 60,
  seed = 42
) {
  # --- Build kernel ---
  if (!is.null(custom_kernel)) {
    kernel <- pmax(custom_kernel, 0)
    if (sum(kernel) < 1e-12) kernel <- rep(1, length(kernel))
    kernel <- kernel / sum(kernel)
    kernel_len <- length(kernel)
  } else {
    kernel_len <- recall_period
    kernel_lags <- seq(0, kernel_len - 1)
    kernel <- dgamma(kernel_lags, shape = recall_shape, rate = recall_rate)
    if (max(kernel) < 1e-12) kernel <- rep(1, kernel_len)
    kernel <- kernel / sum(kernel)
  }
  kernel_lags <- seq(0, kernel_len - 1)
  kernel_com <- sum(kernel_lags * kernel) / sum(kernel)

  # --- Helper: generate one realisation of length n ---
  make_series <- function(n, s) {
    set.seed(s)

    drive <- numeric(n)
    drive[1] <- rnorm(1)
    for (i in 2:n) {
      drive[i] <- ar_coef * drive[i - 1] + rnorm(1, sd = drive_sd)
    }
    n_imp <- max(1, round(n / 100 * 2.7))
    pool <- 15:(n - 20)
    if (length(pool) >= n_imp) {
      imp_t <- sort(sample(pool, n_imp))
      for (ti in imp_t) {
        drive[ti] <- drive[ti] + sample(c(-1, 1), 1) * runif(1, 2.5, 5)
      }
    }

    # Biomarker: IIR recursive filter (O(n))
    decay <- exp(-1 / tau_bio)
    bio_raw <- numeric(n)
    bio_raw[1] <- drive[1]
    for (i in 2:n) {
      bio_raw[i] <- (1 - decay) * drive[i] + decay * bio_raw[i - 1]
    }
    biomarker <- bio_raw + rnorm(n, sd = noise_bio)

    # COA-unique latent drift
    coa_u <- numeric(n)
    coa_u[1] <- rnorm(1, sd = coa_unique_sd)
    for (i in 2:n) {
      coa_u[i] <- 0.92 * coa_u[i - 1] + rnorm(1, sd = coa_unique_sd * 0.4)
    }

    # COA: kernel convolution of true biomarker + unique + noise
    coa_raw <- numeric(n)
    for (i in seq_len(n)) {
      lb <- min(i, kernel_len)
      idx <- seq(i, i - lb + 1)
      w <- kernel[seq_len(lb)]
      sw <- sum(w)
      if (sw < 1e-12) {
        coa_raw[i] <- bio_raw[i]
      } else {
        w <- w / sw
        coa_raw[i] <- sum(bio_raw[idx] * w)
      }
    }
    coa <- coa_raw + coa_u + rnorm(n, sd = noise_coa)

    list(biomarker = biomarker, coa = coa, bio_raw = bio_raw, drive = drive)
  }

  # --- Display series (short, for Panel A) ---
  disp <- make_series(n_display, seed)
  bio_z_disp <- (disp$biomarker - mean(disp$biomarker)) / sd(disp$biomarker)
  coa_z_disp <- (disp$coa - mean(disp$coa)) / sd(disp$coa)

  # --- Long series for asymptotic CCF (Panel C) ---
  lng <- make_series(n_ccf, seed + 1000L)
  bio_z_lng <- (lng$biomarker - mean(lng$biomarker)) / sd(lng$biomarker)
  coa_z_lng <- (lng$coa - mean(lng$coa)) / sd(lng$coa)

  lags <- seq(-max_lag, max_lag)
  ccf_values <- numeric(length(lags))
  for (li in seq_along(lags)) {
    lag <- lags[li]
    if (lag >= 0) {
      n_ov <- n_ccf - lag
      ccf_values[li] <- sum(bio_z_lng[1:n_ov] * coa_z_lng[(1 + lag):(n_ov + lag)]) / (n_ov - 1)
    } else {
      alag <- -lag
      n_ov <- n_ccf - alag
      ccf_values[li] <- sum(bio_z_lng[(1 + alag):(n_ov + alag)] * coa_z_lng[1:n_ov]) / (n_ov - 1)
    }
  }

  peak_idx <- which.max(ccf_values)
  peak_lag <- lags[peak_idx]
  peak_r <- ccf_values[peak_idx]
  r_at_zero <- ccf_values[lags == 0]

  list(
    t = seq_len(n_display),
    biomarker = disp$biomarker,
    coa = disp$coa,
    bio_z = bio_z_disp,
    coa_z = coa_z_disp,
    kernel = kernel,
    kernel_lags = kernel_lags,
    lags = lags,
    ccf = ccf_values,
    stats = list(
      peak_lag = peak_lag,
      peak_r = peak_r,
      r_at_zero = r_at_zero,
      kernel_com = kernel_com,
      recall_period = kernel_len
    )
  )
}
