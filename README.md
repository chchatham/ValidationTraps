# ValidationTraps

A scholarly simulation study illustrating three critical failure modes ("traps") in biomarker discovery and development. Includes an interactive Shiny app and a publication-ready R Markdown report.

## The Three Traps

1. **The Change-Score Paradox** — High cross-sectional convergent validity does NOT guarantee correlated change scores. Two measures can agree at every timepoint yet show zero correlation in their changes over time.

2. **Simpson's Paradox in Reverse** — Weak or zero cross-sectional correlation does NOT rule out strong longitudinal sensitivity to change. A biomarker that looks useless across people can track disease progression within a person.

3. **Portfolio Biomarkers** — No single biomarker is a unicorn. Markers with complementary sensitivity/specificity profiles can be combined into portfolios that outperform any individual marker.

## Requirements

- R >= 4.1
- R packages: `shiny`, `MASS`, `ggplot2`, `patchwork`, `scales`, `pROC`, `DT`
- [pandoc](https://pandoc.org/) >= 1.12.3 (for report rendering only)

### Install R packages

```r
install.packages(c("shiny", "MASS", "ggplot2", "patchwork", "scales", "pROC", "DT"))
```

### Install pandoc (macOS)

```bash
brew install pandoc
```

## Usage

### Run the Shiny app

```r
shiny::runApp("app.R", port = 3838)
```

Then open http://127.0.0.1:3838 in your browser.

### Render the scholarly report

```r
rmarkdown::render("report/validation_traps.Rmd")
```

Produces `report/validation_traps.html`.

### Run unit tests

```bash
Rscript tests/test_simulations.R
```

All 33 tests should pass.

## Project Structure

```
ValidationTraps/
├── app.R                  # Shiny app entry point
├── R/
│   ├── trap1_sim.R        # Trap 1 simulation engine
│   ├── trap2_sim.R        # Trap 2 simulation engine
│   ├── trap3_sim.R        # Trap 3 simulation engine
│   ├── trap1_ui.R         # Trap 1 Shiny module (UI + server)
│   ├── trap2_ui.R         # Trap 2 Shiny module
│   └── trap3_ui.R         # Trap 3 Shiny module
├── report/
│   └── validation_traps.Rmd  # Scholarly report (R Markdown)
├── tests/
│   └── test_simulations.R    # Unit tests for simulation engines
└── www/
    └── styles.css            # Custom app styling
```

## Design

- Simulation logic lives in pure functions (`R/trap*_sim.R`) — no Shiny reactivity, fully testable in isolation.
- UI modules are thin wrappers that wire parameters to simulations and outputs to plots.
- The report sources the same simulation functions with fixed parameters for reproducible figures.
- Each app tab includes explanatory prose framing the trap and its practical implications.
