# ValidationTraps — Guardrails

## Format
Each guardrail is a "sign" — a short, imperative rule learned from experience or domain knowledge.
Signs are append-only. Never delete a sign. If a sign is wrong, add a new sign that supersedes it.

---

### 🚧 R / Shiny Constraints

🪧 SIGN: Always use `set.seed()` inside reactive expressions so that slider changes produce deterministic results for the same parameter set, but re-randomize when the user clicks a "Resimulate" button.

🪧 SIGN: Never use `req()` on numeric slider inputs — they always have a value. Use `req()` only for text inputs or selectInputs that can be empty.

🪧 SIGN: Use `renderPlot()` with explicit `height` and `width` arguments or the plot will resize unpredictably across browsers.

🪧 SIGN: When using `MASS::mvrnorm()`, validate that the covariance matrix is positive-definite before passing it. Extreme slider values can create non-PD matrices. Wrap in `tryCatch()` and show a user-friendly error.

🪧 SIGN: Do not use `library()` calls inside `R/` module files that get `source()`'d. Put all `library()` calls at the top of `app.R`.

🪧 SIGN: When using `freezeReactiveValue` to prevent circular reactivity, always check `!identical(input[[id]], new_val)` before freezing+updating. Unnecessary updates still consume a freeze cycle and can mask bugs.

🪧 SIGN: [SUPERSEDED — see next sign] When two plots need linked hover behavior, only the RESPONDING plot should depend on the hover reactive. If the SOURCE plot (the one being hovered) also depends on it, you get circular re-renders: hover → re-render source → hover resets → re-render again.

🪧 SIGN: For BIDIRECTIONAL cross-plot hover (both plots highlight simultaneously), create a unified `highlight_idx` reactive that combines both hover sources (`arrow_hover %||% scatter_hover`), then wrap it with `throttle(350)`. The throttle absorbs the brief NULL flashes when a re-rendered plot's hover resets. Both plots depend on this single throttled reactive. This avoids the circular re-render problem because the NULL→value→NULL oscillation is smoothed out within the throttle window.

🪧 SIGN: Use `hoverOpts(delayType = "debounce", delay = 200)` for cross-plot hover — not throttle. Debounce waits for mouse to settle; throttle fires continuously and causes excessive re-renders of the responding plot.

🪧 SIGN: When `optim(L-BFGS-B)` solves for slider parameters, snap results to slider step sizes before calling `updateSliderInput`. Otherwise the slider displays at the rounded position but stores the exact value, creating a mismatch between displayed and actual parameters.

🪧 SIGN: Use `selectInput` (not `sliderInput`) for simulation size when you want log-spaced options. Shiny sliders are linear; faking a log scale via transform+label is fragile. A dropdown with explicit choices (100, 200, 500, 1000, ...) is clearer. Remember to `as.numeric(input$n)` since selectInput returns character.

### 🚧 Statistical / Domain Constraints

🪧 SIGN: The change-score correlation (Trap 1) should be computed on the FULL sample, not the plotting subsample. Display full-sample statistics even when plotting a subset.

🪧 SIGN: For Trap 2, the Simpson's paradox framing requires that the cross-sectional correlation is computed ACROSS individuals at a single timepoint, while the longitudinal coupling is computed WITHIN individuals across timepoints. Do not conflate levels of analysis.

🪧 SIGN: For Trap 3, PPV and NPV depend on prevalence. Always include a prevalence slider. Do not hardcode 50% prevalence — this is unrealistic for most biomarker contexts and hides the base-rate problem.

🪧 SIGN: ROC curves for combined markers must be computed on held-out data (or via cross-validation) to avoid optimistic bias. At minimum, split the simulated data 50/50 for train/test when using logistic combination.

🪧 SIGN: Never say a marker "has no value" based on a single correlation or a single decision threshold. The traps exist precisely because single metrics mislead.

🪧 SIGN: The closed-form theoretical statistics for Trap 1 are: trt_X = var_T / sqrt((var_T+var_S+var_e)(var_T+var_dT+var_S+var_e)), conv_T1 = r_T*var_T / sqrt((var_T+var_S+var_e)(var_T+var_e)), change_corr = r_dT*var_dT / sqrt((var_dT+2*var_S+2*var_e)(var_dT+2*var_e)). These must match the simulation's generative model exactly. If the sim model changes, the solver's calc_stats must be re-derived.

### 🚧 Report / Writing Constraints

🪧 SIGN: The report must frame Trap 1 and Trap 2 as converses of each other with respect to Simpson's paradox. Do not treat them as unrelated phenomena.

🪧 SIGN: Use "sensitivity to change" or "longitudinal sensitivity" instead of "responsiveness" — the latter term is contested in the psychometric literature.

🪧 SIGN: Every simulation figure in the report must include a caption stating the exact parameter values used to generate it.

### 🚧 UX / Slider Design

🪧 SIGN: When a slider controls a latent parameter (e.g., r_dT = true change correlation), the observed statistic it affects is attenuated by error/noise variances. If the default noise-to-signal ratio makes the slider appear "broken" (full range produces <0.30 change in output), increase the default signal variance so the slider has a visible effect. Users expect the slider to "do something."

🪧 SIGN: Label latent-parameter sliders as "True ___" (e.g., "True Change Correlation") to distinguish them from observed statistics. Otherwise users think the slider directly controls the displayed correlation.

🪧 SIGN: When a plot subsamples from the full simulation (e.g., 50 arrows from n=100,000), the plot title/subtitle must state both the subsample size AND the full n. A slider labeled "Sample Size" that doesn't match the visible count is confusing. Label the slider "Simulation Size (n)" and annotate the plot with the subsample info.

🪧 SIGN: If the simulation re-runs reactively on every slider change, a "Resimulate" button that only changes the random seed is confusing — users can't tell what it does differently from moving a slider. Remove it unless there's a clear UX reason to keep it (e.g., the sim is expensive and doesn't auto-update).

🪧 SIGN: Do not use bright/saturated colors (maroon, purple, yellow) for data overlays. User prefers black and grey for reference lines (e.g., T1/T2 convergent validity), steelblue for primary data (arrows, regression), and green/grey/red for qualitative interpretation labels. Keep the palette restrained.

🪧 SIGN: Scatter plot points should be unfilled circles (shape=1) at moderate alpha (~0.4) — filled dots at low alpha (0.08) are too faint to see. Use stroke=0.4 and size=1.5 for good visibility without over-plotting.

🪧 SIGN: Parameters called out in the explanatory text box must be visible in the sidebar — not hidden under "Advanced Parameters." If the prose says "change-score correlation depends on r_dT, state variance, and error variance," all three sliders must be immediately visible.

🪧 SIGN: Intro modals on tab first-visit are effective for orienting users. Use `easyClose = TRUE` and a friendly dismiss button ("Got it — let me explore"). Track visited tabs with `reactiveValues` to show each modal only once per session.

### 🚧 Code Quality

🪧 SIGN: All simulation functions must return a list (not a data.frame) containing both the raw vectors AND the summary statistics. The UI layer picks what to display; the sim layer does not assume a display format.

🪧 SIGN: Test edge cases: n=100 (small sample noise), r_T=0 (no cross-sectional correlation), r_dT=1 (perfect change coupling), var_S=0 (no state variance). These must not crash the app.

### 🚧 Package / Environment Constraints

🪧 SIGN: `pROC` and `here` are not in base R installs. They had to be installed manually. If deploying to a fresh machine, ensure `install.packages(c('pROC', 'here'))` is documented.

🪧 SIGN: `pROC` masks `stats::cov`, `stats::smooth`, and `stats::var` when loaded. This is benign but produces console messages. Use `pROC::roc()` and `pROC::auc()` with namespace qualification in simulation code to avoid ambiguity.

🪧 SIGN: For Trap 3 latent scores, `d' = qnorm(sensitivity) + qnorm(specificity)` gives the signal-detection distance. Threshold at `qnorm(specificity)`. This cleanly produces any desired (sens, spec) pair for a single marker.

🪧 SIGN: For Trap 3 weighted combination, use `pROC::coords(roc_obj, "best", best.method = "youden")` to find the optimal threshold on the combined continuous score. Do not hardcode a threshold.

🪧 SIGN: For Trap 3 logistic combination, always use a 50/50 train/test split (guardrail from above), and wrap `glm()` in `tryCatch()` because rare combinations of prevalence + marker profiles can cause perfect separation warnings or convergence failure.

🪧 SIGN: Trap 2 ICC is computed via one-way ANOVA (MS_between - MS_within) / (MS_between + (n_i - 1) * MS_within), not lme4. This avoids an extra dependency and is exact for balanced designs (which our simulated data always is).

🪧 SIGN: Dynamic UI in Trap 3 (`renderUI` for marker sliders) means marker parameter inputs are NULL on first render. Guard with a `if (any(sapply(..., is.null))) return(NULL)` check in the reactive that reads them.

### 🚧 Report / Build Constraints

🪧 SIGN: Do not use `here::here()` inside R Markdown source() calls. `rmarkdown::render()` changes the working directory to the Rmd's folder, and `here::here()` may not resolve to the project root during knit (depends on .here file, .Rproj, etc.). Use relative paths from the Rmd file instead (e.g., `source("../R/trap1_sim.R")`). This is simpler and always works.

🪧 SIGN: pandoc is required for `rmarkdown::render()` but is NOT bundled with R. On macOS, install via `brew install pandoc`. Document this dependency in the README.

### 🚧 Solver / Reactive Loop Constraints

🪧 SIGN: When an `observe` syncs UI inputs (e.g., dropdowns) to match simulation output, and an `observeEvent` watches those same inputs to trigger a solver, `freezeReactiveValue` alone is NOT sufficient to prevent the solver from firing on programmatic updates. The freeze only suppresses for one flush cycle; the new value coming back from the client still triggers the observeEvent. Solution: track expected values in a `reactiveValues` object; the solver checks if all current inputs match expected values and returns early if so.

🪧 SIGN: For hover detection on `geom_segment` arrows, `nearPoints` (which works on point coordinates) is unreliable — it only checks midpoints or endpoints. Use proper point-to-segment distance: project the cursor onto each segment (clamp t to [0,1]), compute Euclidean distance in axis-normalized coordinates. Always pick the nearest segment (no threshold) for maximum forgiveness.

🪧 SIGN: When scatter points correspond to a subset shown in another plot (e.g., arrows), visually distinguish the linked subset with filled circles (shape=16, steelblue) vs open circles (shape=1, grey) so users know which points are interactive. This is more effective than relying solely on hover discovery.

🪧 SIGN: [SUPERSEDES throttle sign above] `throttle()` does NOT fix hover flicker in bidirectional cross-plot highlighting. The problem: when a plot re-renders (due to highlight change), its hover input resets to NULL permanently until the user moves the mouse again. Throttle only delays the NULL — it still propagates after the window. Fix: use a sticky `reactiveVal` that only updates on non-NULL values. Plot re-renders reset hover to NULL, but the held value persists. Add a `reactiveTimer(2000)` that clears the held value after 1.5s of sustained NULL (mouse truly left). This eliminates flicker entirely.

🪧 SIGN: Do not use `&#9432;` (Unicode ⓘ) for help icons — many fonts/browsers render it as a plain "?" or replacement character. Instead, use a literal `?` inside a CSS-styled circular span (`.param-help` class with `border-radius: 50%`, white text on grey background). Reliable everywhere.

🪧 SIGN: Native HTML `title` attribute tooltips are unreliable for UX — they have a 1-2 second delay, don't appear on mobile, and may not render at all in some contexts (e.g., shinyapps.io deployed apps). Use CSS-only tooltips via `data-tip` attribute + `::after` pseudo-element with `content: attr(data-tip)`. These appear instantly on hover and work everywhere.

### 🚧 Deployment Constraints

🪧 SIGN: shinyapps.io deploys can transiently fail with "Failed to fetch... Mirror sync in progress" errors from Ubuntu package mirrors. This is infrastructure-level and not a code problem. Just retry the deploy.

🪧 SIGN: `rsconnect::deployApp()` bundles ALL files in the app directory by default. Use a `.rscignore` file to exclude non-app files. **Critical**: `.rscignore` uses exact `setdiff()` matching against top-level directory/file names, NOT glob patterns. Use `.ralph` (no trailing slash), not `.ralph/`. Trailing slashes will silently fail to match.

🪧 SIGN: For shinyapps.io deployment, credentials are set via `rsconnect::setAccountInfo(name, token, secret)`. The token/secret come from https://www.shinyapps.io → Account → Tokens. These are distinct from RSCLOUD_CLIENT_ID/SECRET (which are for Posit Cloud OAuth).

### 🚧 Cross-Tab UI Consistency

🪧 SIGN: All tabs must share the same UI conventions established in Trap 1. When adding a new UI pattern to one tab, propagate it to all tabs. The canonical conventions are: (1) collapsible `trap-explainer` div at top spanning full width, using JS toggle with ▼/► arrows; (2) custom HTML stats table with `stats-table stats-table-compact` class and color-coded `<span>` values (green #28a745 for good, grey #666 for moderate, red #dc3545 for poor); (3) CSS tooltips (`param-help` class with `data-tip`) on every slider and stats column header; (4) steelblue for primary data and regression lines, open grey circles (shape=1) for base scatter points; (5) `wellPanel(style = "padding: 10px 15px;")` for stats panels; (6) `tagList(explainer, fluidRow(...))` wrapper structure.

🪧 SIGN: When converting a `renderTable` to a custom HTML stats table (`renderUI`), remember that `renderTable` is self-contained (generates its own `<table>` tag), while custom HTML tables via `renderUI` need their own `tags$table(class=...)` wrapper. Remove the old `tableOutput(ns("stats_table"))` and `renderTable` output, replace with `uiOutput(ns("stats_ui"))` and `renderUI`.

🪧 SIGN: For stats tables with variable row counts (e.g., Trap 3 where marker count is dynamic), use `renderUI` to generate the full `tags$table` including `tags$tbody(lapply(...))` for marker rows. A separator row for the "Combined" result should use `style = "border-top: 2px solid #dee2e6;"` and `tags$strong()` for the label to visually distinguish it from individual markers.

🪧 SIGN: Classification thresholds for color-coding should match the domain context: reliability uses 0.8/0.6 cutoffs (Excellent/Good/Poor); convergent validity uses 0.7/0.4 (Strong/Moderate/Weak); change correlation uses 0.4/0.2 (Substantial/Modest/Negligible); cross-sectional r uses 0.5/0.2 (Strong/Moderate/Negligible); within-person r uses 0.6/0.3 (Strong/Moderate/Weak); ICC uses 0.7/0.4 (High/Moderate/Low); performance metrics (sens/spec/PPV/NPV/AUC) use 0.80/0.60 (good/moderate/poor).

### 🚧 Embedded Report / About Tab Constraints

🪧 SIGN: To embed a pre-rendered HTML report in a Shiny app, place the self-contained HTML in `www/` and use `tags$iframe(src = "about.html", ...)`. Shiny serves `www/` contents as static files — the `src` path omits the `www/` prefix. Use `self_contained: true` in the Rmd YAML so the HTML bundles all CSS/JS/images (no external dependencies). Use `style = "border: none; height: calc(100vh - 60px); display: block;"` for a seamless full-viewport embed.

🪧 SIGN: When the Rmd source lives in `report/` but its rendered output belongs in `www/`, render with `output_file = '../www/about.html'` (relative to the Rmd's directory). The Rmd sources sim functions via `source("../R/trap1_sim.R")` etc. The `report/` directory is excluded from deploy via `.rscignore`, but `www/about.html` is included automatically. Only the artifact ships; the source stays local.

🪧 SIGN: Self-contained HTML reports with `toc_float: true`, embedded figures, and MathJax can reach 3-4 MB. This is acceptable for shinyapps.io deployment — the file is cached by the browser after first load. Do not try to shrink it by removing self-containment (that breaks the single-file deployment model).

### 🚧 Iframe-to-Shiny Communication

🪧 SIGN: To communicate from an iframe (e.g., embedded about.html) to the parent Shiny app, use `parent.postMessage({type: 'navigateToTrap', trap: 'trap1', params: {...}}, '*')` in the iframe, and `window.addEventListener('message', function(e) { Shiny.setInputValue('nav_command', e.data, {priority: 'event'}); })` in the parent. The `{priority: 'event'}` flag ensures each click triggers the observer even with identical data. Always guard iframe-side calls with `if (window.parent !== window)` so the button is inert when the HTML is viewed standalone.

🪧 SIGN: When passing preset parameters to Shiny modules from app.R, pass a `reactiveVal` to the module server function. The module observes it with `observeEvent(preset(), ..., ignoreNULL = TRUE, ignoreInit = TRUE)` and calls `updateSliderInput`/`updateSelectInput` on its own session. This avoids cross-session access issues. For modules with dynamic UI (`renderUI` for sliders), use a two-phase approach: set the parent input (e.g., `k_markers`) immediately, store dependent values in a `pending_*` reactiveVal, then use an `observe` that waits for dynamic inputs to exist (`req(all_exist)`) before applying them.

🪧 SIGN: When navigating to a tab via a programmatic `updateNavbarPage`, suppress intro modals by setting `visited$trapX <- TRUE` BEFORE the tab switch. Otherwise the `observeEvent(input$main_nav, ...)` handler fires and shows the modal — which is redundant when the user is coming from the About page where they already read about the trap.

🪧 SIGN: JavaScript arrays in `postMessage` data (e.g., `sensitivities: [0.9, 0.8, 0.7]`) arrive in R as lists, not vectors. Always `unlist()` them before passing to functions that expect numeric vectors. Handle coercion either in app.R's nav_command observer or in the module's preset handler — but do it somewhere before the values reach simulation functions.

### 🚧 Trap 4 Constraints

🪧 SIGN: Panel D of Trap 4 is DETERMINISTIC (two gamma-PDF sensitivity curves). It does not need a random seed or resimulate button. The other three panels (A, B, C) are stochastic and do use seeds.

🪧 SIGN: When the Edit tool writes R string literals containing text like `"unicorn"` (with Unicode smart quotes as CONTENT), the tool can silently replace ASCII `"` string delimiters with Unicode `"` / `"` (U+201C/U+201D), which R cannot parse. After any edit to R files, run `Rscript -e "parse(file='...')"` to verify. If corrupted, use python to replace Unicode quote characters back to ASCII, but be careful not to replace INTENTIONAL Unicode content quotes — restore those manually.

🪧 SIGN: Panel C's visualization uses a DENSITY-FIELD DGP (`generate_trap4c_visual()`), not the linear model (`simulate_trap4_dilution()`). The density field produces the edge-on planes geometry (tiles, outlines, key); the linear model produces the statistics (r_target, r_total, cos_to_total). Both live inside `generate_trap4c_visual()`, which wraps the linear model call with `noise_sd = 0.62 * mode_spread`. When adding Shiny parameters, ensure they affect BOTH the visual geometry AND the wrapped stats call so the display and numbers stay consistent.

🪧 SIGN: The edge-on planes visualization renders 7 × 16 × 16 = 1,792 tile polygons (7,168 rows in the data frame — 4 corners per tile). This is fast enough for Shiny interactive use (<1s render). Do not increase `nx` or `nv` beyond ~20 without testing interactivity lag. For the static About report figure, the default 16×16 resolution is sufficient.

### 🚧 ggplot2 / patchwork Constraints

🪧 SIGN: `Reduce("+", list_of_annotate_layers)` fails inside a ggplot chain because `Reduce` tries to add two ggproto objects together, which is not allowed. Instead, pre-build a data frame of parameters and use `geom_rect` / `geom_tile` with the data frame. This is the correct pattern for programmatic multi-rect color bars or any variable-count annotation layer.

🪧 SIGN: When building a manual color bar (legend) for an alpha-based opacity scale in ggplot2, use `geom_rect` with a data frame of stacked rectangles at increasing alpha values, plus `annotate("rect")` for the border and `annotate("text")` for labels. This is cleaner than trying to coerce `scale_alpha_identity` into producing a guide.
