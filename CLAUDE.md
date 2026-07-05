# ValidationTraps

## Session Recovery (READ THIS FIRST)
If you're starting a new session, recovering from compaction, or running in a Ralph loop:
1. Read `.ralph/ralph_task.md` — the anchor. Has all checkboxes. Defines "done."
2. Read `.ralph/progress.md` — what exists, what's broken, what to do next.
3. Read `.ralph/guardrails.md` — learned constraints. Follow every sign.
4. Do NOT re-read the full codebase unless progress.md says something is broken. Trust the files.
5. Pick up the "Current Focus" from progress.md and work on it.
6. Before exiting or if context feels heavy: update progress.md with what you did and what's next.

## Compaction Instructions
When compacting this conversation, preserve:
- Current task and its completion state
- Any new guardrails discovered this session
- Any new known issues
- The exact next step to take
Do NOT preserve: file contents already read, full API/command outputs, failed approaches
(log failures to .ralph/errors.log instead).

## Project Purpose
ValidationTraps is a scholarly simulation study illustrating three critical failure modes ("traps") in biomarker discovery and development. It produces (1) an interactive Shiny app with three tabs, each demonstrating one trap via configurable simulations, and (2) an R Markdown report suitable for publication that walks through the theory, simulations, and practical implications.

## Architecture
```
ValidationTraps/
├── CLAUDE.md              # This file — session recovery + architecture
├── app.R                  # Shiny app entry point; sources R/ modules
├── R/
│   ├── trap1_sim.R        # Trap 1 simulation: change-score paradox
│   │                      #   simulate_trap1(params) → list(X1,Y1,X2,Y2,stats)
│   ├── trap2_sim.R        # Trap 2 simulation: reverse Simpson's paradox
│   │                      #   simulate_trap2(params) → list(data,stats)
│   ├── trap3_sim.R        # Trap 3 simulation: portfolio combination
│   │                      #   simulate_trap3(params) → list(markers,combined,roc,stats)
│   ├── trap1_ui.R         # UI + server module for Trap 1 tab (optional split)
│   ├── trap2_ui.R         # UI + server module for Trap 2 tab
│   └── trap3_ui.R         # UI + server module for Trap 3 tab
├── report/
│   └── validation_traps.Rmd  # Scholarly report (knits to HTML/PDF)
├── tests/
│   └── test_simulations.R    # Edge-case tests for sim functions
├── www/                      # Static assets (CSS, images if needed)
└── .ralph/                   # State hygiene (see below)
```

## Key Interfaces

### Trap 1 Simulation Function
```r
simulate_trap1 <- function(
  n = 1e5, var_T = 1.0, r_T = 0.95,
  var_dT = 0.05, r_dT = 0.05,
  var_S = 0.015, var_e = 0.05,
  mean_ch = 1, seed = NULL
) → list(
  X1, Y1, X2, Y2,           # numeric vectors length n
  stats = list(
    trt_X, trt_Y,            # test-retest reliability for each measure
    conv_T1, conv_T2,        # convergent validity at each timepoint
    change_corr              # cor(ΔX, ΔY) — the key output
  )
)
```

### Trap 2 Simulation Function
```r
simulate_trap2 <- function(
  n_subjects = 200, n_timepoints = 5,
  var_between = 1.0,        # between-person variance
  var_within = 0.3,         # within-person change variance
  r_cross = 0.0,            # cross-sectional X-Y correlation
  r_longitudinal = 0.8,     # within-person ΔX-ΔY coupling
  var_e = 0.1, seed = NULL
) → list(
  data,                      # data.frame: subject, time, X, Y
  stats = list(
    r_cross_obs,             # observed cross-sectional correlation
    r_within_obs,            # observed within-person correlation
    icc_x, icc_y             # intra-class correlations
  )
)
```

### Trap 3 Simulation Function
```r
simulate_trap3 <- function(
  n = 5000, prevalence = 0.10,
  k_markers = 4,
  sensitivities, specificities,  # vectors length k
  marker_correlations = NULL,    # optional correlation matrix
  combination_rule = c("and", "or", "majority", "weighted", "logistic"),
  weights = NULL,                # for weighted rule
  seed = NULL
) → list(
  markers,                   # n x k matrix of binary test results
  truth,                     # n-vector of true disease status
  combined,                  # n-vector of combined test result
  individual_rocs,           # list of k ROC objects
  combined_roc,              # ROC object for combined
  stats = list(
    individual_ppv, individual_npv, individual_sens, individual_spec,
    combined_ppv, combined_npv, combined_sens, combined_spec
  )
)
```

## Generative Models (Theory)

### Trap 1: Change-Score Paradox
Each person i has latent traits T_xi, T_yi drawn from MVN with correlation r_T.
At time 2, traits shift by dT_xi, dT_yi drawn from MVN with correlation r_dT.
Observed scores:
  X1_i = T_xi + S1_i + e1_xi          (state + error unique to X)
  Y1_i = T_yi + e1_yi                 (error only, no state for Y)
  X2_i = (T_xi + dT_xi) + S2_i + e2_xi
  Y2_i = (T_yi + dT_yi) + e2_yi

Key insight: even with r_T = 0.95 (high convergent validity), if r_dT is low
and var_S, var_e are non-trivial, cor(ΔX, ΔY) ≈ 0.

### Trap 2: Reverse Simpson's Paradox
Between-person: X_i and Y_i are uncorrelated (r_cross ≈ 0).
Within-person: changes in X track changes in Y (r_longitudinal is high).
This arises when between-person variance in X and Y is dominated by factors
unrelated to each other, but within-person fluctuations are driven by a
shared latent process. Classic example: a biomarker that varies hugely across
people for non-disease reasons but tracks disease progression within a person.

### Trap 3: Portfolio Biomarkers
Single markers with imperfect sensitivity/specificity are evaluated individually
and then combined via different rules. Key demonstrations:
- A high-NPV / low-PPV marker (sensitive but not specific) + a low-NPV / high-PPV
  marker (specific but not sensitive) can yield a combined test with both high
  sensitivity and high specificity under appropriate combination rules.
- The optimal combination rule depends on prevalence and the correlation structure
  among markers.
- "Unicorn" markers (high everything) are rare; portfolios are practical.

## Environment
- Language: R ≥ 4.1
- Key packages: shiny, MASS, ggplot2, patchwork, DT, pROC (for ROC curves), lme4 (for ICC in Trap 2), rmarkdown
- No external APIs or databases
- No environment variables required
- Test command: `Rscript -e "source('tests/test_simulations.R')"`
- App launch: `Rscript -e "shiny::runApp('app.R', port=3838)"`
- Report build: `Rscript -e "rmarkdown::render('report/validation_traps.Rmd')"`

## Design Principles
1. **Simulation logic is pure functions** — no Shiny reactivity inside `R/trap*_sim.R`. They take parameters, return lists. Testable in isolation.
2. **UI modules are thin wrappers** — they wire sliders to simulation functions and simulation outputs to plots. No statistical logic in UI code.
3. **Explanatory text is first-class** — each tab includes a `wellPanel()` or `div()` with prose explaining the trap. This is not an afterthought.
4. **Subsample for visual clarity** — arrow plots and spaghetti plots use ~50 randomly selected observations. Statistics use the full sample. Always.
5. **Deterministic default, stochastic on demand** — default seed produces the same plot every time. "Resimulate" button generates a new seed.
6. **Report reproduces app figures** — the `.Rmd` sources the same `R/trap*_sim.R` functions and calls them with fixed parameters to produce publication-quality static figures.
7. **Traps are narratively connected** — the report and app text frame Trap 1 → Trap 2 as converses (cross-sectional ≠ longitudinal, both directions), and Trap 3 as the constructive response.
