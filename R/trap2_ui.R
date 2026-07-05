trap2_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Collapsible explanation box spanning full width at top
    tags$div(class = "trap-explainer", id = ns("explainer_box"),
      tags$div(class = "trap-explainer-header",
        tags$h4("Trap 2: Simpson's Paradox in Reverse", style = "margin:0; display:inline;"),
        tags$span(class = "trap-explainer-toggle", onclick = paste0(
          "var b=document.getElementById('", ns("explainer_body"), "');",
          "var t=this;",
          "if(b.style.display==='none'){b.style.display='block';t.innerHTML='&#9660;';}",
          "else{b.style.display='none';t.innerHTML='&#9654;';}"
        ), HTML("&#9660;"))
      ),
      tags$div(id = ns("explainer_body"), class = "trap-explainer-body",
        p("A weak or zero cross-sectional correlation between two measures does ",
          strong("not"), " rule out strong longitudinal coupling."),
        p("When between-person variance in X and Y is dominated by factors ",
          "unrelated to each other (e.g., genetics, demographics, comorbidities) ",
          "but within-person fluctuations are driven by a shared latent process ",
          "(e.g., disease progression), the cross-sectional correlation can be ",
          "near zero even though within-person changes are tightly coupled."),
        p("This is the ", em("converse"), " of Trap 1:"),
        tags$ul(
          tags$li(strong("Trap 1:"), " High cross-sectional validity masked weak change coupling."),
          tags$li(strong("Trap 2:"), " Weak cross-sectional validity masks strong change coupling.")
        ),
        p(strong("Implication:"), " A biomarker that appears ",
          "unrelated to an outcome across individuals may still be a sensitive ",
          "indicator of within-person change — precisely the property needed for ",
          "monitoring treatment response.")
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          h4("Simulation Parameters"),
          sliderInput(ns("n_subjects"), HTML("Number of Subjects <span class='param-help' data-tip='Total number of individuals in the simulated longitudinal study. More subjects give more stable estimates of cross-sectional and within-person correlations.'>?</span>"),
                      min = 20, max = 500, value = 200, step = 10),
          sliderInput(ns("n_timepoints"), HTML("Number of Timepoints <span class='param-help' data-tip='How many repeated measurements per subject. More timepoints improve estimation of within-person slopes and ICC decomposition.'>?</span>"),
                      min = 3, max = 20, value = 5, step = 1),
          hr(),
          sliderInput(ns("var_between"), HTML("Between-Person Variance (&sigma;²<sub>B</sub>) <span class='param-help' data-tip='Variance of stable individual differences (random intercepts). High values mean people differ greatly from each other at baseline — this drives the cross-sectional correlation toward r_cross.'>?</span>"),
                      min = 0.1, max = 3.0, value = 1.0, step = 0.1),
          sliderInput(ns("var_within"), HTML("Within-Person Slope Variance (&sigma;²<sub>W</sub>) <span class='param-help' data-tip='Variance of individual change trajectories (random slopes). High values mean people change at different rates — this is the signal that longitudinal coupling captures.'>?</span>"),
                      min = 0.01, max = 2.0, value = 0.3, step = 0.05),
          hr(),
          sliderInput(ns("r_cross"), HTML("Cross-Sectional r(X,Y) <span class='param-help' data-tip='Correlation between X and Y across individuals at a single timepoint. Set near zero to demonstrate that cross-sectional independence does NOT rule out longitudinal coupling.'>?</span>"),
                      min = -0.5, max = 0.5, value = 0.0, step = 0.05),
          sliderInput(ns("r_longitudinal"), HTML("Longitudinal r(&Delta;X, &Delta;Y) <span class='param-help' data-tip='Correlation between within-person changes in X and Y over time. High values mean that when X increases within a person, Y does too — the key property for treatment monitoring.'>?</span>"),
                      min = 0, max = 1, value = 0.8, step = 0.05),
          hr(),
          sliderInput(ns("var_e"), HTML("Residual Variance (&sigma;²<sub>e</sub>) <span class='param-help' data-tip='Random measurement noise at each observation. Higher values attenuate both cross-sectional and longitudinal correlations and reduce ICC.'>?</span>"),
                      min = 0.01, max = 1.0, value = 0.1, step = 0.05),
          actionButton(ns("resimulate"), "Resimulate",
                       class = "btn-primary", style = "width:100%;")
        )
      ),
      column(9,
        fluidRow(
          column(6, plotOutput(ns("cross_scatter"), height = "420px")),
          column(6, plotOutput(ns("within_scatter"), height = "420px"))
        ),
        wellPanel(style = "padding: 10px 15px;",
          h4("Summary Statistics", style = "margin-bottom:6px;"),
          uiOutput(ns("stats_ui"))
        )
      )
    )
  )
}

trap2_server <- function(id, preset = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    seed <- reactiveVal(42L)

    observeEvent(preset(), {
      p <- preset()
      updateSliderInput(session, "n_subjects", value = p$n_subjects)
      updateSliderInput(session, "n_timepoints", value = p$n_timepoints)
      updateSliderInput(session, "var_between", value = p$var_between)
      updateSliderInput(session, "var_within", value = p$var_within)
      updateSliderInput(session, "r_cross", value = p$r_cross)
      updateSliderInput(session, "r_longitudinal", value = p$r_longitudinal)
      updateSliderInput(session, "var_e", value = p$var_e)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$resimulate, {
      seed(sample.int(1e6, 1))
    })

    # --- Classification and color helpers (matching Trap 1 conventions) ---
    classify_cross  <- function(v) ifelse(abs(v) < 0.2, "Negligible", ifelse(abs(v) < 0.5, "Moderate", "Strong"))
    classify_within <- function(v) ifelse(abs(v) > 0.6, "Strong", ifelse(abs(v) > 0.3, "Moderate", "Weak"))
    classify_icc    <- function(v) ifelse(v > 0.7, "High", ifelse(v > 0.4, "Moderate", "Low"))

    interp_color <- function(label) {
      switch(label,
        "Strong" = "#28a745", "High" = "#28a745",
        "Moderate" = "#666666",
        "Weak" = "#dc3545", "Negligible" = "#dc3545", "Low" = "#dc3545", "#333")
    }

    render_val <- function(val, label) {
      tags$span(style = paste0("font-weight:bold; color:", interp_color(label)),
                sprintf("%.3f", val))
    }

    sim <- reactive({
      tryCatch(
        simulate_trap2(
          n_subjects = input$n_subjects,
          n_timepoints = input$n_timepoints,
          var_between = input$var_between,
          var_within = input$var_within,
          r_cross = input$r_cross,
          r_longitudinal = input$r_longitudinal,
          var_e = input$var_e,
          seed = seed()
        ),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
    })

    output$cross_scatter <- renderPlot({
      d <- sim()
      if (is.null(d)) return(NULL)

      mid_t <- ceiling(input$n_timepoints / 2)
      mid_df <- d$data[d$data$time == mid_t, ]
      r_obs <- d$stats$r_cross_obs

      ggplot(mid_df, aes(x = X, y = Y)) +
        geom_point(shape = 1, alpha = 0.4, color = "grey50", size = 1.5, stroke = 0.4) +
        geom_smooth(method = "lm", color = "steelblue", se = FALSE,
                    linewidth = 1) +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("r = %.3f", r_obs),
                 hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold",
                 color = "steelblue") +
        labs(title = sprintf("Cross-Sectional (timepoint %d)", mid_t),
             subtitle = sprintf("%d individuals", nrow(mid_df)),
             x = "X", y = "Y") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", size = 13))
    }, height = 420, width = function() session$clientData$output_cross_scatter_width %||% 500)

    output$within_scatter <- renderPlot({
      d <- sim()
      if (is.null(d)) return(NULL)

      r_within <- d$stats$r_within_obs
      df_change <- data.frame(dX = d$delta_X, dY = d$delta_Y)

      n_highlight <- min(50, nrow(df_change))
      set.seed(seed())
      idx <- sample(nrow(df_change), n_highlight)

      df_change$role <- "base"
      df_change$role[idx] <- "highlight"
      df_change$role <- factor(df_change$role, levels = c("base", "highlight"))

      ggplot(df_change, aes(x = dX, y = dY)) +
        geom_point(data = df_change[df_change$role == "base", ],
                   shape = 1, alpha = 0.3, color = "grey50", size = 1.5, stroke = 0.4) +
        geom_point(data = df_change[df_change$role == "highlight", ],
                   shape = 16, alpha = 0.7, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", color = "steelblue", se = FALSE,
                    linewidth = 1) +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("r(ΔX, ΔY) = %.3f", r_within),
                 hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold",
                 color = "steelblue") +
        labs(title = "Within-Person Changes (first → last)",
             subtitle = sprintf("%d individuals (50 highlighted)", nrow(df_change)),
             x = expression(Delta * X), y = expression(Delta * Y)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", size = 13))
    }, height = 420, width = function() session$clientData$output_within_scatter_width %||% 500)

    output$stats_ui <- renderUI({
      d <- sim(); req(d); s <- d$stats

      cross_label  <- classify_cross(s$r_cross_obs)
      within_label <- classify_within(s$r_within_obs)
      icc_x_label  <- classify_icc(s$icc_x)
      icc_y_label  <- classify_icc(s$icc_y)

      tags$table(class = "table table-hover stats-table stats-table-compact",
                 style = "width:100%; margin-bottom:0;",
        tags$thead(tags$tr(
          tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Cross-Sectional r(X,Y)"),
            tags$td(render_val(s$r_cross_obs, cross_label)),
            tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(cross_label)), cross_label))
          ),
          tags$tr(
            tags$td(HTML("Within-Person r(&Delta;X, &Delta;Y)")),
            tags$td(render_val(s$r_within_obs, within_label)),
            tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(within_label)), within_label))
          ),
          tags$tr(
            tags$td("ICC(X)"),
            tags$td(render_val(s$icc_x, icc_x_label)),
            tags$td(
              tags$span(style = paste0("font-weight:bold; color:", interp_color(icc_x_label)), icc_x_label),
              tags$span(style = "color:#888; font-size:0.9em;",
                        sprintf(" — %.0f%% of variance is between-person", s$icc_x * 100))
            )
          ),
          tags$tr(
            tags$td("ICC(Y)"),
            tags$td(render_val(s$icc_y, icc_y_label)),
            tags$td(
              tags$span(style = paste0("font-weight:bold; color:", interp_color(icc_y_label)), icc_y_label),
              tags$span(style = "color:#888; font-size:0.9em;",
                        sprintf(" — %.0f%% of variance is between-person", s$icc_y * 100))
            )
          )
        )
      )
    })
  })
}
