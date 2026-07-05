trap3_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Collapsible explanation box spanning full width at top
    tags$div(class = "trap-explainer", id = ns("explainer_box"),
      tags$div(class = "trap-explainer-header",
        tags$h4("Trap 3: Markers of Good Markers (The Portfolio Approach)", style = "margin:0; display:inline;"),
        tags$span(class = "trap-explainer-toggle", onclick = paste0(
          "var b=document.getElementById('", ns("explainer_body"), "');",
          "var t=this;",
          "if(b.style.display==='none'){b.style.display='block';t.innerHTML='&#9660;';}",
          "else{b.style.display='none';t.innerHTML='&#9654;';}"
        ), HTML("&#9660;"))
      ),
      tags$div(id = ns("explainer_body"), class = "trap-explainer-body",
        p("No single biomarker needs to be a ", em("unicorn"), " with both high ",
          "sensitivity and high specificity. By combining complementary markers ",
          "— some with high sensitivity (good NPV, catching most cases) and ",
          "others with high specificity (good PPV, ruling in disease) — a ",
          "portfolio can achieve overall performance exceeding any individual marker."),
        p("Key insights:"),
        tags$ol(
          tags$li("At low prevalence, PPV suffers severely even with high specificity ",
                  "(the base-rate problem). Combining markers can mitigate this."),
          tags$li("The AND rule maximizes specificity (and PPV) but sacrifices sensitivity."),
          tags$li("The OR rule maximizes sensitivity (and NPV) but sacrifices specificity."),
          tags$li("Logistic regression and weighted combinations can find optimal trade-offs ",
                  "by leveraging continuous underlying scores."),
          tags$li("The optimal rule depends on the clinical context: screening favors OR; ",
                  "confirmatory diagnosis favors AND; general use favors majority or logistic.")
        ),
        p(strong("Implication:"), " Rather than searching for a single perfect biomarker, ",
          "invest in characterizing the complementary strengths of available markers ",
          "and develop principled combination strategies.")
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          h4("Disease Parameters"),
          sliderInput(ns("n"), HTML("Sample Size <span class='param-help' data-tip='Total number of individuals in the simulated cohort. Larger samples give more stable ROC curves and performance metric estimates.'>?</span>"),
                      min = 500, max = 20000, value = 5000, step = 500),
          sliderInput(ns("prevalence"), HTML("Disease Prevalence <span class='param-help' data-tip='Proportion of the population with the disease. Low prevalence severely reduces PPV even with high specificity — this is the base-rate problem.'>?</span>"),
                      min = 0.01, max = 0.50, value = 0.10, step = 0.01),
          hr(),
          h4("Marker Profiles"),
          sliderInput(ns("k_markers"), HTML("Number of Markers <span class='param-help' data-tip='How many biomarkers to include in the portfolio. More markers provide more opportunities for complementary strengths.'>?</span>"),
                      min = 2, max = 6, value = 4, step = 1),
          uiOutput(ns("marker_sliders")),
          hr(),
          h4("Combination Rule"),
          selectInput(ns("rule"), HTML("Rule <span class='param-help' data-tip='How individual marker results are combined. AND maximizes specificity; OR maximizes sensitivity; Majority balances both; Weighted/Logistic optimize using continuous scores.'>?</span>"),
                      choices = c("AND (all positive)" = "and",
                                  "OR (any positive)" = "or",
                                  "Majority vote" = "majority",
                                  "Weighted sum" = "weighted",
                                  "Logistic regression" = "logistic"),
                      selected = "majority"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'logistic'", ns("rule")),
            p(em("Note: logistic regression uses a 50/50 train/test split ",
                 "to avoid optimistic bias."), style = "color: #888; font-size: 0.85em;")
          ),
          actionButton(ns("resimulate"), "Resimulate",
                       class = "btn-primary", style = "width:100%;")
        )
      ),
      column(9,
        fluidRow(
          column(6, plotOutput(ns("roc_plot"), height = "450px")),
          column(6, plotOutput(ns("metrics_plot"), height = "450px"))
        ),
        wellPanel(style = "padding: 10px 15px;",
          h4("Performance Comparison", style = "margin-bottom:6px;"),
          uiOutput(ns("stats_ui"))
        )
      )
    )
  )
}

trap3_server <- function(id, preset = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seed <- reactiveVal(42L)

    pending_marker_params <- reactiveVal(NULL)

    observeEvent(preset(), {
      p <- preset()
      updateSliderInput(session, "n", value = p$n)
      updateSliderInput(session, "prevalence", value = p$prevalence)
      updateSliderInput(session, "k_markers", value = p$k_markers)
      updateSelectInput(session, "rule", selected = p$rule)
      pending_marker_params(list(
        sens = if (is.list(p$sensitivities)) unlist(p$sensitivities) else p$sensitivities,
        spec = if (is.list(p$specificities)) unlist(p$specificities) else p$specificities
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observe({
      pend <- pending_marker_params()
      req(pend)
      k <- input$k_markers
      req(k)
      all_exist <- all(sapply(1:k, function(j) {
        !is.null(input[[paste0("sens_", j)]]) && !is.null(input[[paste0("spec_", j)]])
      }))
      req(all_exist)
      for (j in seq_along(pend$sens)) {
        updateSliderInput(session, paste0("sens_", j), value = pend$sens[j])
        updateSliderInput(session, paste0("spec_", j), value = pend$spec[j])
      }
      pending_marker_params(NULL)
    })

    observeEvent(input$resimulate, {
      seed(sample.int(1e6, 1))
    })

    # --- Classification and color helpers (matching Trap 1 conventions) ---
    classify_pct <- function(v) ifelse(v >= 0.80, "good", ifelse(v >= 0.60, "moderate", "poor"))
    pct_color <- function(label) switch(label, "good" = "#28a745", "moderate" = "#666666", "poor" = "#dc3545", "#333")

    render_pct <- function(val) {
      cl <- classify_pct(val)
      tags$span(style = paste0("font-weight:bold; color:", pct_color(cl)),
                sprintf("%.1f%%", val * 100))
    }

    render_auc <- function(val) {
      cl <- classify_pct(val)
      tags$span(style = paste0("font-weight:bold; color:", pct_color(cl)),
                sprintf("%.3f", val))
    }

    output$marker_sliders <- renderUI({
      k <- input$k_markers
      default_sens <- c(0.90, 0.80, 0.70, 0.95, 0.85, 0.75)[1:k]
      default_spec <- c(0.60, 0.85, 0.90, 0.50, 0.70, 0.80)[1:k]
      labels <- c("A (High Sens)", "B (High Spec)", "C (Balanced)",
                   "D (Screening)", "E", "F")[1:k]

      tagList(lapply(1:k, function(j) {
        tagList(
          strong(sprintf("Marker %s", labels[j]), style = "font-size:0.9em;"),
          fluidRow(
            column(6, sliderInput(ns(paste0("sens_", j)),
                                  HTML(paste0("Sens <span class='param-help' data-tip='Sensitivity of marker ", LETTERS[j], ": probability of a positive test given disease is present. Higher values catch more true cases.'>?</span>")),
                                  min = 0.30, max = 0.99, value = default_sens[j],
                                  step = 0.01, width = "100%")),
            column(6, sliderInput(ns(paste0("spec_", j)),
                                  HTML(paste0("Spec <span class='param-help' data-tip='Specificity of marker ", LETTERS[j], ": probability of a negative test given no disease. Higher values reduce false positives and improve PPV.'>?</span>")),
                                  min = 0.30, max = 0.99, value = default_spec[j],
                                  step = 0.01, width = "100%"))
          )
        )
      }))
    })

    get_marker_params <- reactive({
      k <- input$k_markers
      sens <- sapply(1:k, function(j) input[[paste0("sens_", j)]])
      spec <- sapply(1:k, function(j) input[[paste0("spec_", j)]])
      if (any(sapply(c(sens, spec), is.null))) return(NULL)
      list(sens = sens, spec = spec)
    })

    sim <- reactive({
      params <- get_marker_params()
      if (is.null(params)) return(NULL)

      tryCatch(
        simulate_trap3(
          n = input$n,
          prevalence = input$prevalence,
          k_markers = input$k_markers,
          sensitivities = params$sens,
          specificities = params$spec,
          combination_rule = input$rule,
          seed = seed()
        ),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
    })

    marker_colors <- reactive({
      k <- input$k_markers
      palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
      palette[1:k]
    })

    output$roc_plot <- renderPlot({
      d <- sim()
      if (is.null(d)) return(NULL)

      k <- input$k_markers
      colors <- marker_colors()
      labels <- c("A", "B", "C", "D", "E", "F")[1:k]

      roc_dfs <- lapply(1:k, function(j) {
        roc_obj <- d$individual_rocs[[j]]
        data.frame(
          fpr = 1 - as.numeric(roc_obj$specificities),
          tpr = as.numeric(roc_obj$sensitivities),
          marker = sprintf("Marker %s (AUC=%.2f)", labels[j], pROC::auc(roc_obj)),
          stringsAsFactors = FALSE
        )
      })

      comb_roc <- d$combined_roc
      comb_df <- data.frame(
        fpr = 1 - as.numeric(comb_roc$specificities),
        tpr = as.numeric(comb_roc$sensitivities),
        marker = sprintf("Combined (AUC=%.2f)", pROC::auc(comb_roc)),
        stringsAsFactors = FALSE
      )

      all_df <- do.call(rbind, c(roc_dfs, list(comb_df)))
      all_colors <- c(colors, "black")
      names(all_colors) <- c(
        sapply(1:k, function(j) sprintf("Marker %s (AUC=%.2f)", labels[j],
                                         pROC::auc(d$individual_rocs[[j]]))),
        sprintf("Combined (AUC=%.2f)", pROC::auc(comb_roc))
      )

      ggplot(all_df, aes(x = fpr, y = tpr, color = marker)) +
        geom_line(linewidth = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
        scale_color_manual(values = all_colors) +
        coord_equal() +
        labs(title = "ROC Curves: Individual vs. Combined",
             x = "False Positive Rate (1 - Specificity)",
             y = "True Positive Rate (Sensitivity)",
             color = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          legend.position = "bottom",
          legend.text = element_text(size = 9)
        ) +
        guides(color = guide_legend(ncol = 2))
    }, height = 450, width = function() session$clientData$output_roc_plot_width %||% 500)

    output$metrics_plot <- renderPlot({
      d <- sim()
      if (is.null(d)) return(NULL)

      k <- input$k_markers
      labels <- c("A", "B", "C", "D", "E", "F")[1:k]
      s <- d$stats

      df <- data.frame(
        Marker = c(rep(paste0("Marker ", labels), 4), rep("Combined", 4)),
        Metric = rep(c("Sensitivity", "Specificity", "PPV", "NPV"),
                     each = k + 1),
        Value = c(
          c(s$individual_sens, s$combined_sens),
          c(s$individual_spec, s$combined_spec),
          c(s$individual_ppv, s$combined_ppv),
          c(s$individual_npv, s$combined_npv)
        ),
        stringsAsFactors = FALSE
      )
      df$Marker <- factor(df$Marker, levels = c(paste0("Marker ", labels), "Combined"))
      df$Metric <- factor(df$Metric, levels = c("Sensitivity", "Specificity", "PPV", "NPV"))

      colors <- c(marker_colors(), "grey20")
      names(colors) <- levels(df$Marker)

      ggplot(df, aes(x = Metric, y = Value, fill = Marker)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        scale_fill_manual(values = colors) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        labs(title = sprintf("Performance Metrics (prevalence = %.0f%%)",
                             input$prevalence * 100),
             x = NULL, y = NULL, fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          axis.text.x = element_text(face = "bold")
        ) +
        guides(fill = guide_legend(nrow = 1))
    }, height = 450, width = function() session$clientData$output_metrics_plot_width %||% 500)

    output$stats_ui <- renderUI({
      d <- sim(); req(d)
      k <- input$k_markers
      s <- d$stats
      labels <- c("A", "B", "C", "D", "E", "F")[1:k]

      marker_rows <- lapply(1:k, function(j) {
        tags$tr(
          tags$td(paste0("Marker ", labels[j])),
          tags$td(render_pct(s$individual_sens[j])),
          tags$td(render_pct(s$individual_spec[j])),
          tags$td(render_pct(s$individual_ppv[j])),
          tags$td(render_pct(s$individual_npv[j])),
          tags$td(render_auc(as.numeric(pROC::auc(d$individual_rocs[[j]]))))
        )
      })

      combined_row <- tags$tr(style = "border-top: 2px solid #dee2e6;",
        tags$td(tags$strong(paste0("Combined (", input$rule, ")"))),
        tags$td(render_pct(s$combined_sens)),
        tags$td(render_pct(s$combined_spec)),
        tags$td(render_pct(s$combined_ppv)),
        tags$td(render_pct(s$combined_npv)),
        tags$td(render_auc(as.numeric(pROC::auc(d$combined_roc))))
      )

      tags$table(class = "table table-hover stats-table stats-table-compact",
                 style = "width:100%; margin-bottom:0;",
        tags$thead(tags$tr(
          tags$th("Marker"),
          tags$th(HTML("Sensitivity <span class='param-help' data-tip='Proportion of true disease cases correctly identified as positive. Higher is better for screening.'>?</span>")),
          tags$th(HTML("Specificity <span class='param-help' data-tip='Proportion of true non-disease cases correctly identified as negative. Higher is better for confirmatory testing.'>?</span>")),
          tags$th(HTML("PPV <span class='param-help' data-tip='Positive Predictive Value: probability that a positive test result is a true case. Heavily affected by prevalence.'>?</span>")),
          tags$th(HTML("NPV <span class='param-help' data-tip='Negative Predictive Value: probability that a negative test result is truly disease-free.'>?</span>")),
          tags$th(HTML("AUC <span class='param-help' data-tip='Area Under the ROC Curve: overall discriminative ability across all thresholds. 0.5 = chance, 1.0 = perfect.'>?</span>"))
        )),
        tags$tbody(
          marker_rows,
          combined_row
        )
      )
    })
  })
}
