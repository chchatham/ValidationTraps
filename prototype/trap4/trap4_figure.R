# Trap 4 prototype figure — builds the 2×2 multipanel and writes out/trap4.png
# Rapid-iteration surface: edit params, then `Rscript prototype/trap4/trap4_figure.R`.
# Aesthetics replicate the shipped theme_report. See prototype/trap4/PLAN.md.

suppressMessages({
  library(ggplot2)
  library(patchwork)
})

this_dir <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())

source(file.path(this_dir, "trap4_sim.R"))
out_dir <- file.path(this_dir, "out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Shared aesthetics (replicated from report/about.Rmd) ---------------------
theme_report <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#666", hjust = 0),
    panel.grid.minor = element_blank()
  )
NAVY  <- "#2c3e50"
RED   <- "#dc3545"
BLUE  <- "steelblue"
GREY  <- "grey60"
GROUP_PAL <- c("steelblue", "#5b6b73", "#3f7d78", "#8a6d52")

fmt_r <- function(x) sprintf("%.2f", x)
SEED     <- 42L
PLOT_SUB <- 220

# ==============================================================================
# Panel A — Latent-group heterogeneity (varied slopes / intercepts, overlapping ranges)
# ==============================================================================
A <- simulate_trap4_groups(seed = SEED)
dA <- A$data
set.seed(SEED); idxA <- sample(nrow(dA), min(PLOT_SUB, nrow(dA)))
pA <- ggplot(dA[idxA, ], aes(dX, dY, color = group)) +
  geom_point(shape = 1, alpha = 0.5, size = 1.5, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, aes(group = group)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, color = RED, aes(group = 1)) +
  scale_color_manual(values = GROUP_PAL, guide = "none") +
  labs(
    subtitle = "A.",
    x = expression(Delta*"Biomarker"), y = expression(Delta*"COA")
  ) +
  theme_report

# ==============================================================================
# Panel B — Range-localized biomarker × nonisotropic COA decimation
# ==============================================================================
B <- simulate_trap4_decimation(seed = SEED)
gcurve <- data.frame(L = seq(0, 10, length.out = 200))
gcurve$sens <- B$gain_fun(gcurve$L); gcurve$sens <- gcurve$sens / max(gcurve$sens)
res <- B$mapping; res$res_n <- res$resolution / max(res$resolution)
pB <- ggplot() +
  geom_line(data = gcurve, aes(L, sens), color = NAVY, linewidth = 1.1) +
  geom_line(data = res, aes(mid, res_n), color = RED, linewidth = 0.8) +
  geom_point(data = res, aes(mid, res_n), color = RED, size = 1.2) +
  annotate("text", x = 0.5, y = 0.65, label = "biomarker\nsensitivity", color = NAVY,
           size = 3.5, hjust = 0, lineheight = 0.85, fontface = "bold") +
  annotate("text", x = 9.5, y = 0.55, label = "COA\nresolution", color = RED,
           size = 3.5, hjust = 1, lineheight = 0.85, fontface = "bold") +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.08)) +
  labs(
    subtitle = "B.",
    x = "latent trait", y = "relative sensitivity / resolution"
  ) +
  theme_report

# ==============================================================================
# Panel C — Construct dilution: stacked iso-planes cutting askew through oblique factor axes.
# Opacity encodes biomarker value; high-opacity "fibers" thread across successive cross-sections.
# The COA total score is the equal-weight diagonal; its large angle to the biomarker loading is dilution.
# ==============================================================================
C <- simulate_trap4_dilution(seed = SEED)

# Cross-sections of the latent-trait space, viewed EDGE-ON via a pinhole camera at the stack's centre
# height. Each sheet spans width (X) and depth (Z) and warps by a CONSISTENT through-passing deformation
# so the stack traces a nonlinear submanifold. OPACITY on each sheet = the biomarker's value at that
# combination of latent traits — a MULTIMODAL density field (several modes at different parts of the
# space, one nearer the foreground). The latent-trait axes are oblique with lengths ∝ their COA weight.
Zn <- 1.8; Zf <- 7.4; fcam <- 2.4; Wx <- 1.6; nP <- 7; dy <- 0.5
yv <- (seq_len(nP) - (nP + 1) / 2) * dy
proj <- function(P) c(fcam * P[1] / P[3], fcam * P[2] / P[3])

# Consistent deformation applied to every sheet -> a coherent nonlinear submanifold.
deform <- function(X, Z) 0.20 * sin(1.05 * X + 0.35 * Z) + 0.13 * cos(0.85 * Z - 0.6) - 0.05 * X

# Multimodal biomarker density over the latent-trait space (anisotropic 3-D Gaussians).
bio_modes <- list(
  list(c = c(0.55,  0.80, 5.8), s = c(0.60, 0.50, 1.20), w = 1.00),  # deeper / background mode
  list(c = c(-0.85, -0.55, 2.5), s = c(0.55, 0.45, 0.70), w = 0.98), # foreground mode (near the viewer)
  list(c = c(1.00, -0.05, 4.1), s = c(0.45, 0.50, 0.75), w = 0.60)   # third, weaker mode
)
bio_density <- function(X, Y, Z) {
  d <- 0
  for (m in bio_modes) { dc <- (c(X, Y, Z) - m$c) / m$s; d <- d + m$w * exp(-0.5 * sum(dc^2)) }
  d
}

nx <- 16; nv <- 16
uu <- seq(0, 1, length.out = nx + 1); vv <- seq(0, 1, length.out = nv + 1)
umid <- (head(uu, -1) + tail(uu, -1)) / 2; vmid <- (head(vv, -1) + tail(vv, -1)) / 2
X_of <- function(u) -Wx + u * 2 * Wx
Z_of <- function(v) Zn + v * (Zf - Zn)
world <- function(u, v, yy) { X <- X_of(u); Z <- Z_of(v); c(X, yy + deform(X, Z), Z) }

tiles <- list(); outlines <- list(); gid <- 0
for (si in order(-abs(yv))) {                           # painter's order: outer sheets first, centre last
  yy <- yv[si]
  for (gi in 1:nx) for (gj in 1:nv) {
    gid <- gid + 1
    cab <- rbind(c(uu[gi], vv[gj]), c(uu[gi+1], vv[gj]),
                 c(uu[gi+1], vv[gj+1]), c(uu[gi], vv[gj+1]))
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
tiles_df <- do.call(rbind, tiles); out_df <- do.call(rbind, outlines)

# Latent-trait basis key (lower-left corner): oblique axes, lengths ∝ COA weighting, plus the COA-total
# resultant. Kept as a compact key so it stays legible against the sheet stack.
key_o <- c(-3.0, -1.25)
trait_dirs <- list(c(0.92, 0.45), c(-0.55, 0.85), c(0.80, -0.45),
                   c(-0.80, -0.30), c(0.10, 0.99))
trait_w <- c(1.00, 0.72, 0.90, 0.50, 0.66)
klen <- 0.95
key <- do.call(rbind, lapply(seq_along(trait_dirs), function(k) {
  d <- trait_dirs[[k]] / sqrt(sum(trait_dirs[[k]]^2))
  tip <- key_o + klen * trait_w[k] * d
  data.frame(x = key_o[1], y = key_o[2], xend = tip[1], yend = tip[2], lab = paste0("t", k))
}))
Vtot <- Reduce(`+`, Map(function(d, w) w * d / sqrt(sum(d^2)), trait_dirs, trait_w))
coa_tip <- key_o + klen * Vtot

cb_n <- 20; cb_yb <- -0.8; cb_yt <- 1.4; cb_h <- (cb_yt - cb_yb) / cb_n
cb_df <- data.frame(
  xmin = 3.0, xmax = 3.22,
  ymin = cb_yb + (seq_len(cb_n) - 1) * cb_h,
  ymax = cb_yb + seq_len(cb_n) * cb_h,
  a = seq(0.02, 0.85, length.out = cb_n)
)

pC <- ggplot() +
  geom_polygon(data = tiles_df, aes(X, Y, group = grp, alpha = op), fill = BLUE, color = NA) +
  geom_polygon(data = out_df, aes(X, Y, group = grp), fill = NA, color = BLUE,
               linewidth = 0.25, alpha = 0.4) +
  scale_alpha_identity() +
  geom_segment(data = key, aes(x, y, xend = xend, yend = yend), color = GREY, linewidth = 0.4,
               arrow = arrow(length = unit(0.08, "cm"), type = "closed")) +
  geom_text(data = key, aes(xend, yend, label = lab), size = 2.0, color = "#555", vjust = -0.4) +
  geom_segment(aes(x = key_o[1], y = key_o[2], xend = coa_tip[1], yend = coa_tip[2]), color = RED,
               linewidth = 1.0, arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  annotate("text", x = coa_tip[1], y = coa_tip[2], label = "COA total", color = RED,
           size = 2.3, fontface = "bold", hjust = 0.4, vjust = -0.6) +
  annotate("text", x = key_o[1] + 0.3, y = key_o[2] - 0.32, label = "latent-trait axes (len ~ COA weight)",
           size = 2.0, color = "#888", hjust = 0.4) +
  geom_rect(data = cb_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = a),
            fill = BLUE, color = NA, inherit.aes = FALSE) +
  annotate("rect", xmin = 3.0, xmax = 3.22, ymin = -0.8, ymax = 1.4,
           fill = NA, color = "grey60", linewidth = 0.3) +
  annotate("text", x = 3.11, y = 1.55, label = "biomarker", size = 2.0, color = "#555") +
  annotate("text", x = 3.11, y = -0.95, label = "low", size = 1.8, color = "#888") +
  annotate("text", x = 3.11, y = 1.42, label = "high", size = 1.8, color = "#888", vjust = 0) +
  coord_fixed(ratio = 1, xlim = c(-3.6, 3.6), ylim = c(-2.4, 2.4), clip = "off") +
  labs(subtitle = "C.", x = NULL, y = NULL) +
  theme_report + theme(axis.text = element_blank(), panel.grid = element_blank())

# ==============================================================================
# Panel D — Temporal reach: impulse response curves by biomarker level
# The biomarker change triggers an effect on the clinical state that unfolds over time.
# Higher biomarker levels produce longer-lasting effects. The COA only captures this when
# administered with sufficient delay and a recall period long enough to integrate the effect.
# ==============================================================================
t_D <- seq(-10, 10, by = 0.05)

# Two gamma-like sensitivity curves mirroring Panel B's style.
# Biomarker: prognostic — minimal before t=0, gamma peak around t=3-6.
# COA: retrospective — minimal after t=0, gamma peak around t=-3.
shape_bio <- 4; rate_bio <- 1.1    # peak at (4-1)/1.1 ≈ 2.7, shorter duration
shape_coa <- 3.5; rate_coa <- 1.0  # peak at 2.5/1.0 = 2.5 (mirrored → t=-2.5)
floor_D <- 0.20; ceil_D <- 0.80

bio_raw <- ifelse(t_D > -0.5,
  dgamma(pmax(1e-6, t_D + 0.5), shape = shape_bio, rate = rate_bio) /
    dgamma((shape_bio - 1) / rate_bio, shape = shape_bio, rate = rate_bio),
  0)
bio_sens <- floor_D + (ceil_D - floor_D) * bio_raw

coa_raw <- ifelse(t_D < 0.5,
  dgamma(pmax(1e-6, -(t_D - 0.5)), shape = shape_coa, rate = rate_coa) /
    dgamma((shape_coa - 1) / rate_coa, shape = shape_coa, rate = rate_coa),
  0)
coa_sens <- floor_D + (ceil_D - floor_D) * coa_raw

curves_D <- data.frame(t = t_D, biomarker = bio_sens, coa = coa_sens)

pD <- ggplot(curves_D) +
  annotate("rect", xmin = -6, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = GREY, alpha = 0.10) +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.4) +
  geom_line(aes(t, biomarker), color = NAVY, linewidth = 1.1) +
  geom_line(aes(t, coa), color = RED, linewidth = 0.8) +
  annotate("text", x = 5.5, y = 0.65, label = "causal markers\nbetter predict\nfuture states",
           color = NAVY, size = 3, hjust = 0, lineheight = 0.85, fontface = "bold") +
  annotate("text", x = -5.5, y = 0.65, label = "COA not uniformly\nsensitive across\nrecall period",
           color = RED, size = 3, hjust = 1, lineheight = 0.85, fontface = "bold") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-9, 9, 3)) +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.08)) +
  labs(subtitle = "D.",
       x = "time from assessment",
       y = "variance explained in clinical state") +
  theme_report

# ==============================================================================
# Compose + caption
# ==============================================================================
caption <- paste0(
  "Four benign mechanisms that depress the ΔBiomarker–ΔCOA correlation without invalidating the biomarker.  ",
  "Seed=", SEED, "; full-sample statistics.\n",
  "A: ", A$stats$n_groups, " subtypes, distinct slopes, within-r=0.78.   ",
  "B: range-localized biomarker × coarse low-range COA bins.\n",
  "C: multimodal biomarker density over COA-weighted latent traits.   ",
  "D: biomarker sensitivity is prognostic (peaks after t=0); COA sensitivity is retrospective (peaks before t=0)."
)

fig <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title = "Expected Attenuation: Innocent Attenuation of Change Correlations",
    subtitle = "When a weak Δbiomarker–ΔCOA correlation is not a validation problem",
    caption = caption,
    theme = theme_report + theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "#666"),
      plot.caption = element_text(size = 8, color = "#888", hjust = 0)
    )
  )

out_file <- file.path(out_dir, "trap4.png")
ggsave(out_file, fig, width = 10.5, height = 8.6, dpi = 150)

cat("Wrote:", out_file, "\n")
cat(sprintf("  A pooled r=%.3f  within r=%.3f  slopes=%s\n",
            A$stats$pooled_r, A$stats$within_r_mean, paste(A$stats$slopes, collapse = ",")))
cat(sprintf("  B within-low r=%.3f  high-range r=%.3f  observed binned r=%.3f\n",
            B$stats$r_latent_lowrange, B$stats$r_latent_highrange, B$stats$r_binned_obs))
cat(sprintf("  C r_target=%.3f  r_total=%.3f  cos(w,diag)=%.3f  loadings(3)=%s\n",
            C$stats$r_target, C$stats$r_total, C$stats$cos_to_total,
            paste(sprintf("%+.2f", C$stats$loadings_shown), collapse = ",")))
