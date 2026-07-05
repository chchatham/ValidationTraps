# ValidationTraps — Task Anchor

## Mission
Create a scholarly report with interactive Shiny simulations illustrating three key failure modes ("traps") in biomarker discovery and development. The report and app together should help biomarker scientists avoid common psychometric and statistical pitfalls when evaluating candidate markers.

## Success Criteria (checkboxes = done)

### Phase 1: Project Scaffolding
- [x] R project structure created (`app.R`, `R/`, `report/`, `tests/`)
- [x] Required R packages identified and documented (shiny, MASS, ggplot2, pROC, patchwork, scales, here)
- [x] Stub `app.R` with three-tab layout (Trap 1, Trap 2, Trap 3) runs without error

### Phase 2: Trap 1 — The Change-Score Paradox
_High cross-sectional convergent validity does NOT guarantee correlated change scores._
- [x] Simulation engine for Trap 1 implemented in `R/trap1_sim.R`
- [x] Shiny UI for Trap 1: sliders for all simulation parameters with sensible defaults
      - Primary params visible: r_T, r_dT, var_S, var_e
      - Advanced params collapsed: var_T, var_dT, mean_ch
      - Simulation size: log-spaced selectInput (100–10000, default 200)
      - Arrow point count: numericInput (1–500, default 50)
- [x] Two-panel plot: arrows plot with convergent validity lines (black T1, grey T2), change scatter (unfilled circles, steelblue regression)
- [x] Summary statistics panel: interactive interpretation dropdowns with inverse solver
- [x] Explanatory text panel describing the trap and its implications
- [x] Cross-plot hover: hovering scatter points highlights corresponding arrow in red
- [x] Intro modal on first tab visit

### Phase 3: Trap 2 — Simpson's Paradox in Reverse
_Weak or zero cross-sectional correlation does NOT rule out strong longitudinal sensitivity to change._
- [x] Simulation engine for Trap 2 implemented in `R/trap2_sim.R`
- [x] Shiny UI for Trap 2: sliders for simulation parameters
- [x] Visualization: cross-sectional scatter + within-person change scatter
- [x] Summary statistics: cross-sectional r, within-person r, ICC decomposition
- [x] Explanatory text connecting Trap 2 to Trap 1 via Simpson's paradox framing
- [x] Intro modal on first tab visit

### Phase 4: Trap 3 — Markers of Good Markers (Portfolio Approach)
_No single biomarker is a unicorn. Assemble portfolios with complementary PPV/NPV profiles._
- [x] Simulation engine for Trap 3 implemented in `R/trap3_sim.R`
- [x] Shiny UI for Trap 3: controls for markers, combination rules
- [x] Visualization: ROC curves + performance metrics bar chart
- [x] Interactive demonstration that complementary markers combine well
- [x] Explanatory text on portfolio construction principles
- [x] Intro modal on first tab visit

### Phase 5: Scholarly Report
- [x] Report drafted in R Markdown (`report/validation_traps.Rmd`)
- [x] Introduction, all 3 trap sections, discussion, references
- [x] Static publication-quality figures from simulation functions

### Phase 6: Polish & Testing
- [x] All three Shiny tabs browser-tested with default parameters ← **Trap 1 tested & fixed (sessions 6-7); Trap 2 & 3 UI aligned (session 8); redeployed session 9; user verified session 12**
- [x] Edge cases handled (n=100, extreme parameter values, degenerate correlations)
- [x] App runs without warnings or errors on fresh R session ← **verified session 4; re-verified sessions 7-9 (HTTP 200, 33/33 tests pass)**
- [x] Report knits to PDF/HTML without errors ← **verified session 4: fixed here::here() → relative paths; renders to HTML**
- [x] README with installation and usage instructions ← **written session 5**
- [x] Unit tests re-verified (72/72 passing) ← **verified session 18**

### Phase 7: Scholarly About Report
_An HTML-rendered narrative report accessible from the Shiny app's About tab, walking through all three traps as a coherent scholarly guide._
- [x] `report/about.Rmd` authored — accessible scholarly narrative covering all three traps sequentially
- [x] Rendered to `www/about.html` (self-contained HTML with figures)
- [x] About tab updated to embed the rendered report via iframe
- [x] Browser-tested on deployed app ← **user verified session 12**

### Phase 7b: About-to-App Navigation
_"Explore this interactively" buttons in the About report that navigate to simulation tabs with matching parameters._
- [x] About tab moved to first position (opens on app launch)
- [x] Explore buttons added after each demonstration figure in `about.Rmd`
- [x] JS postMessage bridge: iframe buttons → parent Shiny app
- [x] Module preset handling: app.R routes params to trap1/2/3 modules
- [x] Trap 3 dynamic UI: two-phase preset (set k_markers, defer sens/spec until sliders exist)
- [x] Intro modals suppressed when navigating via About page (user already read context)
- [x] Re-rendered `www/about.html` with buttons
- [x] Deployed to shinyapps.io (session 11)
- [x] Browser-tested on deployed app ← **user verified session 12**

### Phase 8: Deployment
- [x] App deployed to shinyapps.io ← **session 7; redeployed sessions 10-11: https://chrischatham.shinyapps.io/ValidationTraps/**
- [x] rsconnect configured (account: chrischatham)
- [x] Add `.rscignore` to exclude non-app files from deploy bundle ← **added session 8; trailing-slash bug fixed session 9 (bundle 17→8 files)**

## Environment
- Language: R (≥ 4.1)
- Key packages: shiny, MASS, ggplot2, pROC, patchwork, scales, rsconnect
- External: pandoc ≥ 1.12.3 (for report rendering)
- Test: `Rscript -e "shiny::runApp('app.R', port=3838)"` should launch app
- Report: `Rscript -e "rmarkdown::render('report/validation_traps.Rmd')"`
- Unit tests: `Rscript tests/test_simulations.R` (64 tests, all passing as of session 17)
- Deploy: `Rscript -e "rsconnect::deployApp(appDir='.', appName='ValidationTraps', account='chrischatham')"`

### Phase 9: Trap 4 Prototype — "Expected Attenuation: Innocent Attenuation of Change Correlations"
_A low Δbiomarker–ΔCOA correlation is not necessarily a validation problem. Diagnostic companion to Trap 1._
**Plan of record:** `prototype/trap4/PLAN.md` (approved 2026-07-04). Prototype-only; ships nothing until a later integration phase is approved.
- [x] `prototype/trap4/trap4_sim.R` — pure sim fns (4 mechanisms) → list(data, stats), deterministic seed, contract matches R/trap*_sim.R
- [x] `prototype/trap4/trap4_figure.R` — builds 2×2 patchwork, writes `out/trap4.png`, replicates `theme_report`
- [x] Panel A: latent-group heterogeneity — achieved within r = 0.78, pooled r = 0.00
- [x] Panel B: nonisotropic decimation / unequal 10-bin COA — achieved latent r = 0.85, binned r = 0.43 (+ staircase inset showing coarse-low/fine-high resolution and where the biomarker's range sits)
- [x] Panel C: construct dilution across 6 subscales — achieved r vs target = 0.78, r vs total = 0.31
- [x] Panel D: temporal lag / leading indicator — achieved zero-lag r = 0.00, peak r = 0.80 at lag +1
- [x] Rendered `prototype/trap4/out/trap4.png`
- [x] **User review of `out/trap4.png`** ← approved 2026-07-04
- [x] **DO NOT** touch app.R / R/trap*_ui.R / report/*.Rmd / tests / deploy — integration is a separate approved phase (see PLAN.md)

### Phase 10: Trap 4 Integration — Promote Prototype to Production
_Move approved Trap 4 prototype into the shipped app, report, and test suite._
- [x] `R/trap4_sim.R` — promoted 3 stochastic sim functions + `generate_trap4_temporal()` + `generate_trap4c_visual()` + `plot_trap4c()` for edge-on planes
- [x] `R/trap4_ui.R` — Shiny module: panel selector (A/B/C/D), dynamic sliders per panel, plot, stats table. Panel C uses edge-on planes matching prototype.
- [x] `app.R` — added Trap 4 tab, source files, preset routing, intro modal, visited tracking
- [x] `report/about.Rmd` — Trap 4 narrative with 2x2 figure; Panel C uses edge-on planes matching prototype
- [x] `www/about.html` — re-rendered with edge-on planes Panel C (4.1 MB)
- [x] `tests/test_simulations.R` — 31 Trap 4 tests (64 total, all passing)
- [ ] Browser-tested on deployed app
- [x] Redeployed to shinyapps.io ← session 17
- [x] `.rscignore` updated to exclude `prototype/` from deploy bundle

## Current Focus
Session 17: Panel C visual correspondence fixed, deployed. Awaiting user browser verification at https://chrischatham.shinyapps.io/ValidationTraps/
