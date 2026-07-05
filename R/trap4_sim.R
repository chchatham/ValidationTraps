# Trap 4 simulations — "Expected Attenuation: Innocent Attenuation of Change Correlations"
# Four mechanisms that depress ΔBiomarker–ΔCOA correlation without invalidating the biomarker.
# Each function returns a list with raw data AND summary stats (consistent with R/trap1-3_sim.R).

# ------------------------------------------------------------------------------
# Panel A — Latent-group heterogeneity (Simpson's paradox in change-space).
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

  resid_sd <- abs(slopes) * x_sd * sqrt(1 / within_r^2 - 1)

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

  gain_fun <- function(L) gain_floor + (1 - gain_floor) * plogis(-(L - gain_center) / gain_width)
  gain <- gain_fun(L1)
  dX <- gain * dL + track_noise * rnorm(n, 0, dL_sd)

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
  mapping$resolution <- 1 / mapping$width

  low_mask <- L1 < gain_center

  list(
    data = data.frame(dX = dX, dL = dL, dCOA = dCOA, L1 = L1, L2 = L2,
                      gain = gain, low_range = low_mask),
    mapping = mapping,
    gain_fun = gain_fun,
    stats = list(
      r_binned_obs = cor(dX, dCOA),
      r_latent_lowrange = cor(dX[low_mask], dL[low_mask]),
      r_latent_highrange = cor(dX[!low_mask], dL[!low_mask]),
      r_latent_overall = cor(dX, dL),
      pct_low = mean(low_mask),
      gain_center = gain_center,
      n_bins = n_bins
    )
  )
}

# ------------------------------------------------------------------------------
# Panel C — Construct dilution across factors.
# ------------------------------------------------------------------------------
simulate_trap4_dilution <- function(
  n = 800, k_factors = 10,
  loadings = NULL, noise_sd = 0.62, seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(loadings)) {
    loadings <- c(0.85, -0.30, 0.25, rep(0.05, max(0, k_factors - 3)))
  }
  loadings <- loadings[seq_len(k_factors)]

  Fac <- matrix(rnorm(n * k_factors), n, k_factors)
  dX <- as.numeric(Fac %*% loadings) + rnorm(n, 0, noise_sd)
  total <- rowSums(Fac)

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
# Panel C visualization — Edge-on stacked planes in latent-trait space.
# The biomarker is a multimodal density field; opacity = biomarker value.
# Viewed through a pinhole camera at the stack's centre height.
# Returns all geometry needed for rendering, plus stats from the linear model.
# ------------------------------------------------------------------------------
generate_trap4c_visual <- function(
  k_traits = 5,
  trait_weights = NULL,
  trait_dirs = NULL,
  bio_modes = NULL,
  mode_spread = 1.0,
  n_planes = 7, nx = 16, nv = 16,
  Zn = 1.8, Zf = 7.4, fcam = 2.4, Wx = 1.6, dy = 0.5,
  key_origin = c(-3.0, -1.25), klen = 0.95,
  seed = NULL
) {
  if (is.null(trait_dirs)) {
    if (k_traits == 5) {
      trait_dirs <- list(c(0.92, 0.45), c(-0.55, 0.85), c(0.80, -0.45),
                         c(-0.80, -0.30), c(0.10, 0.99))
    } else {
      angles <- seq(0, pi * (1 - 1 / k_traits), length.out = k_traits)
      trait_dirs <- lapply(angles, function(a) c(cos(a), sin(a)))
    }
  }
  if (is.null(trait_weights)) {
    if (k_traits == 5) {
      trait_weights <- c(1.00, 0.72, 0.90, 0.50, 0.66)
    } else {
      trait_weights <- 0.5 + 0.5 * abs(sin(seq_len(k_traits) * 1.3 + 0.7))
    }
  }
  if (is.null(bio_modes)) {
    bio_modes <- list(
      list(c = c(0.55, 0.80, 5.8), s = c(0.60, 0.50, 1.20) * mode_spread, w = 1.00),
      list(c = c(-0.85, -0.55, 2.5), s = c(0.55, 0.45, 0.70) * mode_spread, w = 0.98),
      list(c = c(1.00, -0.05, 4.1), s = c(0.45, 0.50, 0.75) * mode_spread, w = 0.60)
    )
  }

  yv <- (seq_len(n_planes) - (n_planes + 1) / 2) * dy
  proj <- function(P) c(fcam * P[1] / P[3], fcam * P[2] / P[3])
  deform <- function(X, Z) 0.20 * sin(1.05 * X + 0.35 * Z) + 0.13 * cos(0.85 * Z - 0.6) - 0.05 * X
  bio_density <- function(X, Y, Z) {
    d <- 0
    for (m in bio_modes) { dc <- (c(X, Y, Z) - m$c) / m$s; d <- d + m$w * exp(-0.5 * sum(dc^2)) }
    d
  }

  uu <- seq(0, 1, length.out = nx + 1); vv <- seq(0, 1, length.out = nv + 1)
  umid <- (head(uu, -1) + tail(uu, -1)) / 2; vmid <- (head(vv, -1) + tail(vv, -1)) / 2
  X_of <- function(u) -Wx + u * 2 * Wx
  Z_of <- function(v) Zn + v * (Zf - Zn)
  world <- function(u, v, yy) { X <- X_of(u); Z <- Z_of(v); c(X, yy + deform(X, Z), Z) }

  tiles <- list(); outlines <- list(); gid <- 0
  for (si in order(-abs(yv))) {
    yy <- yv[si]
    for (gi in 1:nx) for (gj in 1:nv) {
      gid <- gid + 1
      cab <- rbind(c(uu[gi], vv[gj]), c(uu[gi + 1], vv[gj]),
                   c(uu[gi + 1], vv[gj + 1]), c(uu[gi], vv[gj + 1]))
      pts <- t(apply(cab, 1, function(z) proj(world(z[1], z[2], yy))))
      wc <- world(umid[gi], vmid[gj], yy)
      op <- bio_density(wc[1], wc[2], wc[3])
      tiles[[gid]] <- data.frame(X = pts[, 1], Y = pts[, 2], grp = gid,
                                 op = max(0.02, min(0.85, 0.02 + 0.90 * op)))
    }
    peri <- rbind(cbind(seq(0, 1, length.out = 12), 0), cbind(1, seq(0, 1, length.out = 6)),
                  cbind(seq(1, 0, length.out = 12), 1), cbind(0, seq(1, 0, length.out = 6)))
    ppts <- t(apply(peri, 1, function(z) proj(world(z[1], z[2], yy))))
    outlines[[length(outlines) + 1]] <- data.frame(X = ppts[, 1], Y = ppts[, 2], grp = paste0("o", si))
  }
  tiles_df <- do.call(rbind, tiles)
  outlines_df <- do.call(rbind, outlines)

  key_o <- key_origin
  key_df <- do.call(rbind, lapply(seq_along(trait_dirs), function(k) {
    d <- trait_dirs[[k]] / sqrt(sum(trait_dirs[[k]]^2))
    tip <- key_o + klen * trait_weights[k] * d
    data.frame(x = key_o[1], y = key_o[2], xend = tip[1], yend = tip[2], lab = paste0("t", k))
  }))
  Vtot <- Reduce(`+`, Map(function(d, w) w * d / sqrt(sum(d^2)), trait_dirs, trait_weights))
  coa_tip <- key_o + klen * Vtot

  cb_n <- 20; cb_yb <- -0.8; cb_yt <- 1.4; cb_h <- (cb_yt - cb_yb) / cb_n
  cb_df <- data.frame(
    xmin = 3.0, xmax = 3.22,
    ymin = cb_yb + (seq_len(cb_n) - 1) * cb_h,
    ymax = cb_yb + seq_len(cb_n) * cb_h,
    a = seq(0.02, 0.85, length.out = cb_n)
  )

  sim_stats <- simulate_trap4_dilution(k_factors = k_traits, noise_sd = 0.62 * mode_spread, seed = seed)$stats

  list(
    tiles_df = tiles_df,
    outlines_df = outlines_df,
    key_df = key_df,
    key_origin = key_o,
    coa_tip = coa_tip,
    colorbar_df = cb_df,
    stats = sim_stats
  )
}

# Render the edge-on planes visualization from generate_trap4c_visual() output.
plot_trap4c <- function(vis, title = NULL, subtitle = "C.",
                        base_size = 13, label_size = 2.0, anno_size = 2.3,
                        NAVY = "#2c3e50", RED = "#dc3545", BLUE = "steelblue", GREY = "grey60") {
  ggplot() +
    geom_polygon(data = vis$tiles_df, aes(X, Y, group = grp, alpha = op), fill = BLUE, color = NA) +
    geom_polygon(data = vis$outlines_df, aes(X, Y, group = grp), fill = NA, color = BLUE,
                 linewidth = 0.25, alpha = 0.4) +
    scale_alpha_identity() +
    geom_segment(data = vis$key_df, aes(x, y, xend = xend, yend = yend), color = GREY, linewidth = 0.4,
                 arrow = arrow(length = unit(0.08, "cm"), type = "closed")) +
    geom_text(data = vis$key_df, aes(xend, yend, label = lab), size = label_size,
              color = "#555", vjust = -0.4) +
    geom_segment(aes(x = vis$key_origin[1], y = vis$key_origin[2],
                     xend = vis$coa_tip[1], yend = vis$coa_tip[2]), color = RED,
                 linewidth = 1.0, arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
    annotate("text", x = vis$coa_tip[1], y = vis$coa_tip[2], label = "COA total", color = RED,
             size = anno_size, fontface = "bold", hjust = 0.4, vjust = -0.6) +
    annotate("text", x = vis$key_origin[1] + 0.3, y = vis$key_origin[2] - 0.32,
             label = "latent-trait axes (len ~ COA weight)",
             size = label_size, color = "#888", hjust = 0.4) +
    geom_rect(data = vis$colorbar_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = a),
              fill = BLUE, color = NA, inherit.aes = FALSE) +
    annotate("rect", xmin = 3.0, xmax = 3.22, ymin = -0.8, ymax = 1.4,
             fill = NA, color = "grey60", linewidth = 0.3) +
    annotate("text", x = 3.11, y = 1.55, label = "biomarker", size = label_size, color = "#555") +
    annotate("text", x = 3.11, y = -0.95, label = "low", size = label_size * 0.9, color = "#888") +
    annotate("text", x = 3.11, y = 1.42, label = "high", size = label_size * 0.9, color = "#888", vjust = 0) +
    coord_fixed(ratio = 1, xlim = c(-3.6, 3.6), ylim = c(-2.4, 2.4), clip = "off") +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = base_size - 2, color = "#666", hjust = 0),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
}

# ------------------------------------------------------------------------------
# Panel C — 3D surface data for interactive plotly rendering.
# Same DGP as generate_trap4c_visual() but returns raw 3D coordinates as
# matrices suitable for plotly::add_surface().
# ------------------------------------------------------------------------------
generate_trap4c_surfaces <- function(
  k_traits = 5,
  trait_weights = NULL,
  trait_dirs = NULL,
  bio_modes = NULL,
  mode_spread = 1.0,
  n_planes = 7, nx = 16, nv = 16,
  Zn = 1.8, Zf = 7.4, Wx = 1.6, dy = 0.5,
  seed = NULL
) {
  if (is.null(trait_dirs)) {
    if (k_traits == 5) {
      trait_dirs <- list(c(0.92, 0.45), c(-0.55, 0.85), c(0.80, -0.45),
                         c(-0.80, -0.30), c(0.10, 0.99))
    } else {
      angles <- seq(0, pi * (1 - 1 / k_traits), length.out = k_traits)
      trait_dirs <- lapply(angles, function(a) c(cos(a), sin(a)))
    }
  }
  if (is.null(trait_weights)) {
    if (k_traits == 5) {
      trait_weights <- c(1.00, 0.72, 0.90, 0.50, 0.66)
    } else {
      trait_weights <- 0.5 + 0.5 * abs(sin(seq_len(k_traits) * 1.3 + 0.7))
    }
  }
  if (is.null(bio_modes)) {
    bio_modes <- list(
      list(c = c(0.55, 0.80, 5.8), s = c(0.60, 0.50, 1.20) * mode_spread, w = 1.00),
      list(c = c(-0.85, -0.55, 2.5), s = c(0.55, 0.45, 0.70) * mode_spread, w = 0.98),
      list(c = c(1.00, -0.05, 4.1), s = c(0.45, 0.50, 0.75) * mode_spread, w = 0.60)
    )
  }

  yv <- (seq_len(n_planes) - (n_planes + 1) / 2) * dy
  deform <- function(X, Z) 0.20 * sin(1.05 * X + 0.35 * Z) + 0.13 * cos(0.85 * Z - 0.6) - 0.05 * X
  bio_density <- function(X, Y, Z) {
    d <- 0
    for (m in bio_modes) { dc <- (c(X, Y, Z) - m$c) / m$s; d <- d + m$w * exp(-0.5 * sum(dc^2)) }
    d
  }

  u_seq <- seq(0, 1, length.out = nx + 1)
  v_seq <- seq(0, 1, length.out = nv + 1)

  surfaces <- lapply(seq_len(n_planes), function(si) {
    yy <- yv[si]
    x_mat <- matrix(NA_real_, nx + 1, nv + 1)
    y_mat <- matrix(NA_real_, nx + 1, nv + 1)
    z_mat <- matrix(NA_real_, nx + 1, nv + 1)
    d_mat <- matrix(NA_real_, nx + 1, nv + 1)
    for (i in seq_along(u_seq)) {
      for (j in seq_along(v_seq)) {
        X <- -Wx + u_seq[i] * 2 * Wx
        Z <- Zn + v_seq[j] * (Zf - Zn)
        Y <- yy + deform(X, Z)
        x_mat[i, j] <- X
        y_mat[i, j] <- Y
        z_mat[i, j] <- Z
        d_mat[i, j] <- bio_density(X, Y, Z)
      }
    }
    list(x = x_mat, y = y_mat, z = z_mat, density = d_mat)
  })

  mid_y <- mean(range(yv))
  key_center <- c(-Wx - 1.8, mid_y, (Zn + Zf) / 2)
  klen_3d <- 1.5

  golden_ratio <- (1 + sqrt(5)) / 2
  trait_dirs_3d <- lapply(seq_len(k_traits), function(i) {
    theta <- 2 * pi * i / golden_ratio
    phi <- acos(1 - 2 * (i - 0.5) / k_traits)
    c(sin(phi) * cos(theta), cos(phi), sin(phi) * sin(theta))
  })

  key_arrows <- do.call(rbind, lapply(seq_len(k_traits), function(k) {
    d3 <- trait_dirs_3d[[k]] / sqrt(sum(trait_dirs_3d[[k]]^2))
    len <- klen_3d * trait_weights[k]
    data.frame(
      x0 = key_center[1], y0 = key_center[2], z0 = key_center[3],
      x1 = key_center[1] + len * d3[1],
      y1 = key_center[2] + len * d3[2],
      z1 = key_center[3] + len * d3[3],
      lab = paste0("t", k), stringsAsFactors = FALSE
    )
  }))

  Vtot_3d <- Reduce(`+`, Map(function(d, w) w * d / sqrt(sum(d^2)), trait_dirs_3d, trait_weights))
  coa_arrow <- data.frame(
    x0 = key_center[1], y0 = key_center[2], z0 = key_center[3],
    x1 = key_center[1] + klen_3d * Vtot_3d[1],
    y1 = key_center[2] + klen_3d * Vtot_3d[2],
    z1 = key_center[3] + klen_3d * Vtot_3d[3],
    stringsAsFactors = FALSE
  )

  sim_stats <- simulate_trap4_dilution(k_factors = k_traits, noise_sd = 0.62 * mode_spread, seed = seed)$stats

  list(surfaces = surfaces, key_arrows = key_arrows, coa_arrow = coa_arrow,
       key_center = key_center, stats = sim_stats)
}

# ------------------------------------------------------------------------------
# Panel D — Temporal mismatch (biomarker vs COA sensitivity curves).
# Analytical (deterministic): two gamma-PDF sensitivity curves showing that the
# biomarker is prognostic (peaks after assessment) while the COA is retrospective
# (peaks before assessment). No stochastic simulation needed.
# ------------------------------------------------------------------------------
generate_trap4_temporal <- function(
  shape_bio = 4, rate_bio = 1.1,
  shape_coa = 3.5, rate_coa = 1.0,
  floor = 0.20, ceiling = 0.80,
  t_range = c(-10, 10), dt = 0.05
) {
  t <- seq(t_range[1], t_range[2], by = dt)

  bio_raw <- ifelse(t > -0.5,
    dgamma(pmax(1e-6, t + 0.5), shape = shape_bio, rate = rate_bio) /
      dgamma((shape_bio - 1) / rate_bio, shape = shape_bio, rate = rate_bio),
    0)
  bio_sens <- floor + (ceiling - floor) * bio_raw

  coa_raw <- ifelse(t < 0.5,
    dgamma(pmax(1e-6, -(t - 0.5)), shape = shape_coa, rate = rate_coa) /
      dgamma((shape_coa - 1) / rate_coa, shape = shape_coa, rate = rate_coa),
    0)
  coa_sens <- floor + (ceiling - floor) * coa_raw

  bio_peak_t <- (shape_bio - 1) / rate_bio - 0.5
  coa_peak_t <- -((shape_coa - 1) / rate_coa - 0.5)

  list(
    curves = data.frame(t = t, biomarker = bio_sens, coa = coa_sens),
    stats = list(
      bio_peak_t = bio_peak_t,
      coa_peak_t = coa_peak_t,
      temporal_gap = bio_peak_t - coa_peak_t,
      shape_bio = shape_bio,
      rate_bio = rate_bio,
      shape_coa = shape_coa,
      rate_coa = rate_coa
    )
  )
}
