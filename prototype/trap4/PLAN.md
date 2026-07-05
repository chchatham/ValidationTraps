# Trap 4 Prototype Plan — "The False Alarm: Innocent Attenuation of Change Correlations"

**File:** `prototype/trap4/PLAN.md`
**Status:** APPROVED 2026-07-04. Prototype-only. Nothing in the shipped app/report/deploy is touched until a separate integration phase is approved.

## Purpose
A multipanel figure illustrating conditions under which a **low correlation between change in a
biomarker (ΔX) and change in a clinical outcome assessment (ΔCOA) is not necessarily a validation
problem.** This is the diagnostic companion to Trap 1: where Trap 1 warns against optimism (high
cross-sectional validity ≠ correlated change), this warns against *premature pessimism* (low change
correlation ≠ invalid biomarker). Each panel is a distinct benign mechanism that depresses the
observed Δ–Δ correlation while an honest structural relationship survives.

Rhetorical structure shared by every panel: **navy line = the honest structural relationship (high r);
red line = the naïve pooled/observed correlation (collapsed to ~0).**

## Panels (2×2)

### Panel A — Latent-group heterogeneity (Simpson's paradox in change-space)
Several latent subgroups (subtypes / responder classes). ΔX–ΔCOA is well correlated *within* each
group, but between-group mean structure is oriented orthogonally to the within-group slope, so pooling
cancels the signal.
- Generative model: G groups. Within group g, bivariate-normal (ΔX, ΔY) with within-corr `r_w ≈ 0.78`.
  Group means placed along a line orthogonal to the within-group slope (conservative same-sign design:
  slopes do NOT flip, only offsets differ) so the pooled cloud is flat.
- Figure: muted-colored clouds per group, each with a steelblue within-group fit; one flat red pooled fit.
- Target stats: within-group r ≈ 0.78, pooled r ≈ 0.05.

### Panel B — Nonisotropic decimation (unequal COA resolution)
The COA quantizes a latent continuum into ~10 ordinal bins of *unequal width* — fine resolution in one
region, coarse in another. The biomarker's dynamic range sits in the COA's coarse region, so latent
change is compressed into few/no registered bin crossings, attenuating observed Δ–Δ even though the
biomarker tracks the latent perfectly.
- Generative model: latent L at two timepoints; ΔX tracks ΔL with `r_latent ≈ 0.85`. Observed COA =
  nonuniform binning of L (10 bins). ΔCOA_obs = bin(L2) − bin(L1). Population sits in the coarse region.
- Figure: ΔX vs ΔCOA_obs (quantized → integer lattice), navy latent reference line, red observed fit;
  small staircase inset (observed-COA index vs latent-L, unequal treads) if bands alone read ambiguously.
- Target stats: latent r ≈ 0.85, observed (binned) r ≈ 0.32.

### Panel C — Construct dilution across factors
The biomarker is specific to one latent dimension; the COA **total score** sums several subscales, only
one of which is that dimension. ΔX tracks the on-target subscale strongly, but off-target subscale
variance dominates the total, diluting the correlation.
- Generative model: ΔY_total = Δf1 + Δf2 + … + Δfk. ΔX tracks Δf1 (`r ≈ 0.8`); Δf2…Δfk independent with
  comparable variance so the total is dominated by off-target change.
- Figure: same panel shows ΔX vs on-target subscale (steelblue, strong navy fit) contrasted against
  ΔX vs total score (grey open circles, flat red fit); both z-scored to share the axis.
- Target stats: r vs target subscale ≈ 0.80, r vs total ≈ 0.28.

### Panel D — Temporal lag (leading indicator) [SELECTED as the orthogonal fourth condition]
The biomarker is an upstream/leading indicator: same-window ΔX–ΔCOA is weak, but ΔX predicts *later*
ΔCOA strongly. Orthogonal to A–C because the mechanism is temporal dynamics, not measurement structure;
and it is the case most central to surrogate-endpoint validation.
- Generative model: per-subject driver in window 1 affects X contemporaneously and COA in window 2;
  an independent earlier driver affects COA in window 1. Compute cor(ΔX, ΔCOA) across lags −2…+2.
- Figure: lagged cross-correlation curve (r vs lag, navy line), peak at +1 interval highlighted
  (steelblue), zero-lag point marked red/low. A distinct temporal x-axis reinforces orthogonality.
- Target stats: zero-lag r ≈ 0.10; peak r ≈ 0.80 at +1 interval.

## Visual syntax (matches shipped figures exactly)
- `theme_report`: `theme_minimal(base_size = 13)`, bold left-aligned title, grey (#666) subtitle carrying
  the key r-values, `panel.grid.minor = element_blank()`.
- steelblue open circles (`shape = 1`, alpha 0.3–0.5); navy `#2c3e50` = honest structure; red `#dc3545`
  = attenuated/observed; group palette = restrained muted qualitative set (no saturated maroon/purple/yellow).
- Panel titles "A./B./C./D." bold; subtitle states the two r-values; a caption lists exact params.
- `patchwork` 2×2, ~10×8 in, dpi 150.

## Default design choices (revisit during iteration if needed)
- Panel A: conservative same-sign groups with orthogonal offsets (no sign-flipping slopes).
- Panel B: quantization bands on the scatter; staircase inset only if needed.

## Prototype architecture (forward-compatible, inert)
```
prototype/trap4/
  PLAN.md          # this file
  trap4_sim.R      # pure sim fns → list(data, stats); deterministic seed; contract matches R/trap*_sim.R
  trap4_figure.R   # sources sim + local copy of theme_report; builds patchwork; writes out/trap4.png
  out/trap4.png    # artifact to review
```
- Iteration loop: edit params atop `trap4_figure.R` → `Rscript prototype/trap4/trap4_figure.R` → view PNG.
- Full-sample statistics; plot subsampling only for visual clarity (per project guardrail).
- `theme_report` is replicated locally now (zero risk); extraction to a shared `R/theme_report.R` is a
  later integration step.

## NOT touched by this prototype
`app.R`, `R/trap*_ui.R`, `report/about.Rmd`, `report/validation_traps.Rmd`, `tests/`, `.rscignore`, deploy.

## Later integration phases (scope only after the figure is approved)
1. Promote `trap4_sim.R` → `R/trap4_sim.R`; extract shared `theme_report`.
2. New Shiny tab + `R/trap4_ui.R` module mirroring the established UI conventions.
3. Report section in `validation_traps.Rmd` + narrative section in `about.Rmd` with an
   "Explore this interactively" button.
4. Edge-case tests appended to `tests/test_simulations.R`; redeploy.
