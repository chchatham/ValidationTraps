library(ggplot2)
library(patchwork)

source("figure5_sim.R")

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(color = "#555555", size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e8e8e8"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.text = element_text(size = base_size - 1),
      plot.margin = margin(8, 12, 8, 8)
    )
}

result <- generate_figure5(seed = 42)

n_variants <- length(result$coa_variants)
labels <- sapply(result$coa_variants, `[[`, "label")
lt_values <- c("solid", "longdash", "dashed", "dotted")[seq_len(n_variants)]
names(lt_values) <- labels
coa_palette <- rep("#c0392b", n_variants)
names(coa_palette) <- labels

# ---- Panel A: Time series ----
bio_z <- (result$biomarker - mean(result$biomarker)) / sd(result$biomarker)
ts_df <- data.frame(t = result$t, value = bio_z, series = "Biomarker")
for (v in result$coa_variants) {
  ts_df <- rbind(ts_df, data.frame(
    t = result$t, value = v$coa_z, series = v$label
  ))
}
ts_df$series <- factor(ts_df$series, levels = c("Biomarker", labels))

all_lt <- c("Biomarker" = "solid", lt_values)
all_colors <- c("Biomarker" = "steelblue", coa_palette)
all_lw <- c("Biomarker" = 0.45, setNames(rep(0.7, n_variants), labels))

panel_a <- ggplot(ts_df, aes(x = t, y = value, color = series, linetype = series)) +
  geom_line(aes(linewidth = series), alpha = 0.85) +
  scale_color_manual(values = all_colors, name = NULL) +
  scale_linetype_manual(values = all_lt, name = NULL) +
  scale_linewidth_manual(values = all_lw, name = NULL, guide = "none") +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  labs(
    title = "A",
    x = "Time (days)",
    y = "Standardized value"
  ) +
  theme_report() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.8, "cm")
  )

# ---- Panel B: Recall kernels ----
kernel_df <- do.call(rbind, lapply(result$coa_variants, function(v) {
  data.frame(
    lag = v$kernel_lags,
    weight = v$kernel / max(v$kernel),
    variant = v$label
  )
}))
kernel_df$variant <- factor(kernel_df$variant, levels = labels)

com_df <- data.frame(
  variant = factor(labels, levels = labels),
  com = sapply(result$coa_variants, `[[`, "kernel_com")
)

panel_b <- ggplot(kernel_df, aes(x = lag, y = weight, linetype = variant)) +
  geom_line(color = "#c0392b", linewidth = 0.7) +
  geom_vline(data = com_df, aes(xintercept = com),
             linetype = "dotted", color = "#aaaaaa", linewidth = 0.3) +
  scale_linetype_manual(values = lt_values, name = NULL) +
  labs(
    title = "B",
    x = "Days before report",
    y = "Relative weight"
  ) +
  theme_report() +
  theme(
    legend.position = "none",
    legend.key.width = unit(1.5, "cm")
  )

# ---- Panel C: Lagged cross-correlations ----
ccf_df <- do.call(rbind, lapply(result$coa_variants, function(v) {
  data.frame(lag = result$lags, r = v$ccf, variant = v$label)
}))
ccf_df$variant <- factor(ccf_df$variant, levels = labels)

peak_df <- do.call(rbind, lapply(result$coa_variants, function(v) {
  data.frame(lag = v$peak_lag, r = v$peak_r, variant = v$label)
}))
peak_df$variant <- factor(peak_df$variant, levels = labels)

panel_c <- ggplot(ccf_df, aes(x = lag, y = r, linetype = variant)) +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "#999999", linewidth = 0.4, linetype = "dashed") +
  geom_line(color = "grey30", linewidth = 0.55) +
  geom_point(data = peak_df, aes(x = lag, y = r),
             color = "steelblue", size = 2.5, shape = 16) +
  scale_linetype_manual(values = lt_values, name = NULL) +
  labs(
    title = "C",
    x = expression("Assessment Time Difference (days; COA " * minus * " Biomarker)"),
    y = "Cross-correlation (r)"
  ) +
  theme_report() +
  theme(
    legend.position = "none",
    legend.key.width = unit(1.5, "cm")
  )

# ---- Compose ----
peak_summary <- paste(
  sapply(result$coa_variants, function(v) {
    sprintf("%s: peak r = %.2f at +%d d", v$label, v$peak_r, v$peak_lag)
  }),
  collapse = "   |   "
)

fig <- (panel_a) / (panel_b | panel_c) +
  plot_annotation(
    title = "Figure 5: Temporal Accumulation and the Leading-Indicator Illusion",
    subtitle = peak_summary,
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 9.5, color = "#444444")
    )
  )

ggsave("out/figure5.png", fig, width = 12, height = 9.5, dpi = 200, bg = "white")
cat("Saved out/figure5.png\n")
for (v in result$coa_variants) {
  cat(sprintf("  %s: peak lag = %d d, peak r = %.3f, r at lag 0 = %.3f, kernel COM = %.1f d\n",
    v$label, v$peak_lag, v$peak_r, v$r_at_zero, v$kernel_com))
}
