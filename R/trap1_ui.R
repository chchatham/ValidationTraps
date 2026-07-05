trap1_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Collapsible explanation box spanning full width at top
    tags$div(class = "trap-explainer", id = ns("explainer_box"),
      tags$div(class = "trap-explainer-header",
        tags$h4("Trap 1: The Change-Score Paradox", style = "margin:0; display:inline;"),
        tags$span(class = "trap-explainer-toggle", onclick = paste0(
          "var b=document.getElementById('", ns("explainer_body"), "');",
          "var t=this;",
          "if(b.style.display==='none'){b.style.display='block';t.innerHTML='&#9660;';}",
          "else{b.style.display='none';t.innerHTML='&#9654;';}"
        ), HTML("&#9660;"))
      ),
      tags$div(id = ns("explainer_body"), class = "trap-explainer-body",
        p("High cross-sectional convergent validity between two measures does ",
          strong("not"), " guarantee that their change scores will be correlated."),
        p("Even when X and Y are highly correlated at any single timepoint ",
          "(convergent validity r ~ 0.95), the correlation between ",
          HTML("&Delta;X and &Delta;Y"), " can be near zero when:"),
        tags$ol(
          tags$li("True change in the underlying constructs has low correlation ",
                  HTML("(r<sub>&Delta;T</sub> &approx; 0)")),
          tags$li("State-specific variance affects one measure but not the other"),
          tags$li("Measurement error is non-trivial relative to true change variance")
        ),
        p(strong("Implication:"), " Demonstrating that a candidate biomarker tracks a ",
          "gold standard at baseline does not ensure it will track treatment-related ",
          "changes. Longitudinal sensitivity to change must be validated separately ",
          "from cross-sectional convergent validity.")
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          h4("Simulation Parameters"),
          sliderInput(ns("r_T"), HTML("Trait Correlation (r<sub>T</sub>) <span class='param-help' data-tip='Correlation between true latent traits X and Y. High values mean both measures track the same construct cross-sectionally.'>?</span>"),
                      min = 0, max = 1, value = 0.95, step = 0.01),
          sliderInput(ns("r_dT"), HTML("True Change Correlation (r<sub>&Delta;T</sub>) <span class='param-help' data-tip='Correlation between true changes in X and Y over time. This is what change-score correlation tries to estimate — often near zero even when cross-sectional validity is high.'>?</span>"),
                      min = 0, max = 1, value = 0.05, step = 0.01),
          sliderInput(ns("var_S"), HTML("State Variance (&sigma;²<sub>S</sub>) <span class='param-help' data-tip='Variance due to transient states (e.g., mood, fatigue) that affect measure X but not Y. Adds noise to change scores in X only.'>?</span>"),
                      min = 0, max = 0.5, value = 0.015, step = 0.005),
          sliderInput(ns("var_e"), HTML("Error Variance (&sigma;²<sub>e</sub>) <span class='param-help' data-tip='Random measurement error variance, affecting both X and Y independently at each timepoint. Attenuates all correlations.'>?</span>"),
                      min = 0, max = 0.5, value = 0.05, step = 0.005),
          selectInput(ns("n"), HTML("Simulation Size (n) <span class='param-help' data-tip='Total number of simulated individuals. Statistics use the full sample; plots show a subset.'>?</span>"),
                      choices = c(100, 200, 500, 1000, 2000, 5000, 10000),
                      selected = 200),
          numericInput(ns("n_arrows"), HTML("Arrow Points to Display <span class='param-help' data-tip='Number of randomly selected individuals shown as arrows on the trajectory plot. More arrows reveal the pattern but can be harder to read.'>?</span>"),
                       value = 50, min = 1, max = 500, step = 1),
          tags$details(
            tags$summary(
              HTML("&#9881; "),
              tags$strong("Advanced Parameters"),
              tags$span(class = "details-arrow")
            ),
            sliderInput(ns("var_T"), HTML("Trait Variance (&sigma;²<sub>T</sub>) <span class='param-help' data-tip='Variance of the stable latent trait underlying both measures. Larger values make cross-sectional correlations dominate over noise.'>?</span>"),
                        min = 0.1, max = 3.0, value = 1.0, step = 0.1),
            sliderInput(ns("var_dT"), HTML("Change Variance (&sigma;²<sub>&Delta;T</sub>) <span class='param-help' data-tip='Variance of true latent change from pre to post. Larger values give change scores more signal relative to noise.'>?</span>"),
                        min = 0.001, max = 2, value = 0.25, step = 0.01),
            sliderInput(ns("mean_ch"), HTML("Mean Change <span class='param-help' data-tip='Average shift in the latent trait from pre to post. Affects the mean direction of arrows but not the correlation structure.'>?</span>"),
                        min = 0, max = 3, value = 1, step = 0.1)
          )
        )
      ),
      column(9,
        fluidRow(
          column(6, plotOutput(ns("arrow_plot"), height = "420px",
                               hover = hoverOpts(id = ns("arrow_hover"),
                                                 delay = 200,
                                                 delayType = "debounce"))),
          column(6, plotOutput(ns("change_scatter"), height = "420px",
                               hover = hoverOpts(id = ns("scatter_hover"),
                                                 delay = 200,
                                                 delayType = "debounce")))
        ),
        tags$p(class = "text-muted", style = "font-size: 0.85em; font-style: italic; margin: 6px 0 12px 0; text-align: center;",
               "Hover over points in either plot to highlight the same individual across both."),
        wellPanel(style = "padding: 10px 15px;",
          h4("Summary Statistics", style = "margin-bottom:6px;"),
          tags$table(id = ns("stats_tbl"),
                     class = "table table-hover stats-table stats-table-compact",
                     style = "width:100%; margin-bottom:0;",
            tags$thead(tags$tr(
              tags$th("Statistic"), tags$th("Value"),
              tags$th(HTML("Interpretation <span class='param-help' data-tip='Select a target interpretation for any statistic and the simulator will find the closest parameter values that produce it. Use this to ask &ldquo;what would it take to get Excellent reliability?&rdquo;'>?</span>"))
            )),
            tags$tbody(
              tags$tr(
                tags$td("Test-Retest Reliability (X)"),
                tags$td(uiOutput(ns("val_trt_x"), inline = TRUE)),
                tags$td(selectInput(ns("interp_trt_x"), NULL,
                                    choices = c("Excellent", "Good", "Poor"),
                                    selected = "Excellent", width = "140px"))
              ),
              tags$tr(
                tags$td("Test-Retest Reliability (Y)"),
                tags$td(uiOutput(ns("val_trt_y"), inline = TRUE)),
                tags$td(selectInput(ns("interp_trt_y"), NULL,
                                    choices = c("Excellent", "Good", "Poor"),
                                    selected = "Excellent", width = "140px"))
              ),
              tags$tr(
                tags$td("Convergent Validity (T1)"),
                tags$td(uiOutput(ns("val_conv_t1"), inline = TRUE)),
                tags$td(selectInput(ns("interp_conv_t1"), NULL,
                                    choices = c("Strong", "Moderate", "Weak"),
                                    selected = "Strong", width = "140px"))
              ),
              tags$tr(
                tags$td("Convergent Validity (T2)"),
                tags$td(uiOutput(ns("val_conv_t2"), inline = TRUE)),
                tags$td(selectInput(ns("interp_conv_t2"), NULL,
                                    choices = c("Strong", "Moderate", "Weak"),
                                    selected = "Strong", width = "140px"))
              ),
              tags$tr(
                tags$td("Change-Score Correlation"),
                tags$td(uiOutput(ns("val_change"), inline = TRUE)),
                tags$td(selectInput(ns("interp_change"), NULL,
                                    choices = c("Substantial", "Modest", "Negligible"),
                                    selected = "Negligible", width = "140px"))
              )
            )
          )
        )
      )
    ),
    tags$script(HTML(paste0(
      "function colorStatsSelects(){",
        "var m={'Excellent':'#28a745','Strong':'#28a745','Substantial':'#28a745',",
              "'Good':'#666','Moderate':'#666','Modest':'#666',",
              "'Poor':'#dc3545','Weak':'#dc3545','Negligible':'#dc3545'};",
        "$('#", ns("stats_tbl"), " select').each(function(){",
          "var c=m[$(this).val()]||'#333';",
          "$(this).css({color:c,'font-weight':'bold'});",
        "});",
      "}",
      "$(document).on('shiny:inputchanged shiny:value shiny:recalculated',",
        "function(){setTimeout(colorStatsSelects,50);});",
      "$(document).ready(function(){setTimeout(colorStatsSelects,500);});"
    )))
  )
}

trap1_server <- function(id, preset = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    seed <- reactiveVal(42L)

    observeEvent(preset(), {
      p <- preset()
      updateSliderInput(session, "r_T", value = p$r_T)
      updateSliderInput(session, "r_dT", value = p$r_dT)
      updateSliderInput(session, "var_S", value = p$var_S)
      updateSliderInput(session, "var_e", value = p$var_e)
      updateSliderInput(session, "var_T", value = p$var_T)
      updateSliderInput(session, "var_dT", value = p$var_dT)
      updateSliderInput(session, "mean_ch", value = p$mean_ch)
      updateSelectInput(session, "n", selected = as.character(p$n))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Theoretical statistics from parameters (closed-form) ---
    calc_stats <- function(var_T, r_T, var_dT, r_dT, var_S, var_e) {
      c(
        trt_X = var_T / sqrt((var_T + var_S + var_e) * (var_T + var_dT + var_S + var_e)),
        trt_Y = var_T / sqrt((var_T + var_e) * (var_T + var_dT + var_e)),
        conv_T1 = r_T * var_T / sqrt((var_T + var_S + var_e) * (var_T + var_e)),
        conv_T2 = (r_T * var_T + r_dT * var_dT) /
                   sqrt((var_T + var_dT + var_S + var_e) * (var_T + var_dT + var_e)),
        change_corr = r_dT * var_dT /
                       sqrt((var_dT + 2 * var_S + 2 * var_e) * (var_dT + 2 * var_e))
      )
    }

    interp_to_target <- function(label) {
      switch(label,
        "Excellent" = 0.90, "Good" = 0.70, "Poor" = 0.40,
        "Strong" = 0.85, "Moderate" = 0.55, "Weak" = 0.20,
        "Substantial" = 0.55, "Modest" = 0.30, "Negligible" = 0.05, 0.5)
    }

    classify_rel  <- function(v) ifelse(v > 0.8, "Excellent", ifelse(v > 0.6, "Good", "Poor"))
    classify_conv <- function(v) ifelse(abs(v) > 0.7, "Strong", ifelse(abs(v) > 0.4, "Moderate", "Weak"))
    classify_ch   <- function(v) ifelse(abs(v) > 0.4, "Substantial", ifelse(abs(v) > 0.2, "Modest", "Negligible"))

    interp_color <- function(label) {
      switch(label,
        "Excellent" = "#28a745", "Strong" = "#28a745", "Substantial" = "#28a745",
        "Good" = "#666666", "Moderate" = "#666666", "Modest" = "#666666",
        "Poor" = "#dc3545", "Weak" = "#dc3545", "Negligible" = "#dc3545", "#333")
    }

    snap <- function(x, step, lo, hi) max(lo, min(hi, round(x / step) * step))

    solve_params <- function(targets, start) {
      obj <- function(par) {
        pred <- calc_stats(par[1], par[2], par[3], par[4], par[5], par[6])
        sum((pred - targets)^2)
      }
      lo <- c(0.1, 0.0, 0.001, 0.0, 0.0, 0.0)
      hi <- c(3.0, 1.0, 2.0, 1.0, 0.5, 0.5)

      best <- NULL; best_val <- Inf
      starts <- list(start, runif(6, lo, hi))
      for (s in starts) {
        res <- tryCatch(
          optim(s, obj, method = "L-BFGS-B", lower = lo, upper = hi),
          error = function(e) NULL)
        if (!is.null(res) && res$value < best_val) {
          best <- res; best_val <- res$value
        }
      }
      if (is.null(best)) return(NULL)
      list(var_T = best$par[1], r_T = best$par[2], var_dT = best$par[3],
           r_dT = best$par[4], var_S = best$par[5], var_e = best$par[6])
    }

    # --- Simulation ---
    sim <- reactive({
      tryCatch(
        simulate_trap1(
          n = as.numeric(input$n),
          var_T = input$var_T, r_T = input$r_T,
          var_dT = input$var_dT, r_dT = input$r_dT,
          var_S = input$var_S, var_e = input$var_e,
          mean_ch = input$mean_ch, seed = seed()
        ),
        error = function(e) { showNotification(e$message, type = "error"); NULL }
      )
    })

    # --- Stable arrow indices (shared between both plots) ---
    arrow_indices <- reactive({
      d <- sim(); req(d)
      n_show <- min(input$n_arrows %||% 50, length(d$X1))
      set.seed(seed() + 1L)
      sample(length(d$X1), n_show)
    })

    # --- Precomputed scatter data (stable across hover) ---
    scatter_df <- reactive({
      d <- sim(); req(d)
      n_plot <- min(5000, length(d$X1))
      set.seed(seed())
      idx <- sample(length(d$X1), n_plot)
      ai <- arrow_indices()
      data.frame(dX = d$X2[idx] - d$X1[idx],
                 dY = d$Y2[idx] - d$Y1[idx],
                 orig_idx = idx,
                 in_arrows = idx %in% ai)
    })

    # --- Scatter hover → original index ---
    hovered_idx_raw <- reactive({
      hover <- input$scatter_hover
      if (is.null(hover)) return(NULL)
      df <- scatter_df()
      if (is.null(df)) return(NULL)
      near <- nearPoints(df, hover, xvar = "dX", yvar = "dY",
                         maxpoints = 1, threshold = 10)
      if (nrow(near) == 0) return(NULL)
      near$orig_idx[1]
    })

    # --- Arrow hover → original index ---
    # Uses point-to-segment distance; always picks the nearest arrow
    hovered_arrow_idx_raw <- reactive({
      hover <- input$arrow_hover
      if (is.null(hover)) return(NULL)
      d <- sim()
      if (is.null(d)) return(NULL)
      idx <- arrow_indices()
      px <- hover$x; py <- hover$y
      x1 <- d$X1[idx]; y1 <- d$Y1[idx]
      x2 <- d$X2[idx]; y2 <- d$Y2[idx]
      if (is.null(hover$domain)) {
        rx <- diff(range(c(x1, x2))); ry <- diff(range(c(y1, y2)))
      } else {
        rx <- hover$domain$right - hover$domain$left
        ry <- hover$domain$top - hover$domain$bottom
      }
      if (rx == 0) rx <- 1; if (ry == 0) ry <- 1
      npx <- px / rx; npy <- py / ry
      nx1 <- x1 / rx; ny1 <- y1 / ry
      nx2 <- x2 / rx; ny2 <- y2 / ry
      dx <- nx2 - nx1; dy <- ny2 - ny1
      len2 <- dx^2 + dy^2
      len2[len2 == 0] <- 1e-10
      t_val <- pmax(0, pmin(1, ((npx - nx1) * dx + (npy - ny1) * dy) / len2))
      closest_x <- nx1 + t_val * dx
      closest_y <- ny1 + t_val * dy
      dist2 <- (npx - closest_x)^2 + (npy - closest_y)^2
      best <- which.min(dist2)
      idx[best]
    })

    # --- Unified highlight index (sticky latch — ignores NULL to prevent re-render flicker) ---
    highlight_held <- reactiveVal(NULL)
    last_hover_time <- reactiveVal(0)

    observe({
      raw <- hovered_arrow_idx_raw() %||% hovered_idx_raw()
      if (!is.null(raw)) {
        last_hover_time(as.numeric(Sys.time()))
        if (!identical(raw, isolate(highlight_held()))) {
          highlight_held(raw)
        }
      }
    })

    # Clear highlight after 2s of no hover activity
    auto_clear <- reactiveTimer(2000)
    observe({
      auto_clear()
      held <- isolate(highlight_held())
      if (!is.null(held)) {
        elapsed <- as.numeric(Sys.time()) - isolate(last_hover_time())
        if (elapsed > 1.5) {
          highlight_held(NULL)
        }
      }
    })

    highlight_idx <- reactive({ highlight_held() })

    # --- Value cells (colored numeric display) ---
    render_val <- function(val, label) {
      tags$span(style = paste0("font-weight:bold; color:", interp_color(label)),
                sprintf("%.3f", val))
    }
    output$val_trt_x   <- renderUI({ d <- sim(); req(d); render_val(d$stats$trt_X,      classify_rel(d$stats$trt_X)) })
    output$val_trt_y   <- renderUI({ d <- sim(); req(d); render_val(d$stats$trt_Y,      classify_rel(d$stats$trt_Y)) })
    output$val_conv_t1 <- renderUI({ d <- sim(); req(d); render_val(d$stats$conv_T1,    classify_conv(d$stats$conv_T1)) })
    output$val_conv_t2 <- renderUI({ d <- sim(); req(d); render_val(d$stats$conv_T2,    classify_conv(d$stats$conv_T2)) })
    output$val_change  <- renderUI({ d <- sim(); req(d); render_val(d$stats$change_corr, classify_ch(d$stats$change_corr)) })

    # --- Track what the sync observer last set (to distinguish user vs programmatic dropdown changes) ---
    expected_interps <- reactiveValues(
      trt_x = "Excellent", trt_y = "Excellent",
      conv_t1 = "Strong", conv_t2 = "Strong",
      change = "Negligible"
    )

    # --- Sync dropdowns to actual interpretations after sim runs ---
    observe({
      d <- sim(); req(d); s <- d$stats
      new_trt_x   <- classify_rel(s$trt_X)
      new_trt_y   <- classify_rel(s$trt_Y)
      new_conv_t1 <- classify_conv(s$conv_T1)
      new_conv_t2 <- classify_conv(s$conv_T2)
      new_change  <- classify_ch(s$change_corr)

      expected_interps$trt_x   <- new_trt_x
      expected_interps$trt_y   <- new_trt_y
      expected_interps$conv_t1 <- new_conv_t1
      expected_interps$conv_t2 <- new_conv_t2
      expected_interps$change  <- new_change

      sync <- function(id, new_val) {
        if (!identical(input[[id]], new_val)) {
          freezeReactiveValue(input, id)
          updateSelectInput(session, id, selected = new_val)
        }
      }
      sync("interp_trt_x",   new_trt_x)
      sync("interp_trt_y",   new_trt_y)
      sync("interp_conv_t1", new_conv_t1)
      sync("interp_conv_t2", new_conv_t2)
      sync("interp_change",  new_change)
    })

    # --- Solve for params when user manually changes an interpretation dropdown ---
    observeEvent(
      list(input$interp_trt_x, input$interp_trt_y, input$interp_conv_t1,
           input$interp_conv_t2, input$interp_change),
      {
        req(input$interp_trt_x, input$interp_trt_y, input$interp_conv_t1,
            input$interp_conv_t2, input$interp_change)

        # Skip if all dropdowns match what the sync observer set — this is a programmatic update
        if (identical(input$interp_trt_x,   expected_interps$trt_x) &&
            identical(input$interp_trt_y,   expected_interps$trt_y) &&
            identical(input$interp_conv_t1, expected_interps$conv_t1) &&
            identical(input$interp_conv_t2, expected_interps$conv_t2) &&
            identical(input$interp_change,  expected_interps$change)) {
          return()
        }

        targets <- c(
          trt_X       = interp_to_target(input$interp_trt_x),
          trt_Y       = interp_to_target(input$interp_trt_y),
          conv_T1     = interp_to_target(input$interp_conv_t1),
          conv_T2     = interp_to_target(input$interp_conv_t2),
          change_corr = interp_to_target(input$interp_change)
        )
        current <- c(input$var_T %||% 1, input$r_T, input$var_dT %||% 0.25,
                      input$r_dT, input$var_S, input$var_e)

        result <- solve_params(targets, current)
        if (is.null(result)) return()

        updateSliderInput(session, "var_T",  value = snap(result$var_T,  0.1,   0.1,   3.0))
        updateSliderInput(session, "r_T",    value = snap(result$r_T,    0.01,  0,     1))
        updateSliderInput(session, "var_dT", value = snap(result$var_dT, 0.01,  0.001, 2))
        updateSliderInput(session, "r_dT",   value = snap(result$r_dT,   0.01,  0,     1))
        updateSliderInput(session, "var_S",  value = snap(result$var_S,  0.005, 0,     0.5))
        updateSliderInput(session, "var_e",  value = snap(result$var_e,  0.005, 0,     0.5))
      },
      ignoreInit = TRUE
    )

    # --- Arrow plot (reacts to unified highlight) ---
    output$arrow_plot <- renderPlot({
      d <- sim(); if (is.null(d)) return(NULL)

      arrow_idx <- arrow_indices()
      n_show <- length(arrow_idx)

      df_arrows <- data.frame(
        x_start = d$X1[arrow_idx], y_start = d$Y1[arrow_idx],
        x_end   = d$X2[arrow_idx], y_end   = d$Y2[arrow_idx],
        role    = "base",
        stringsAsFactors = FALSE
      )

      hi <- highlight_idx()
      if (!is.null(hi)) {
        if (hi %in% arrow_idx) {
          df_arrows$role[which(arrow_idx == hi)[1]] <- "hover"
        } else {
          df_arrows <- rbind(df_arrows, data.frame(
            x_start = d$X1[hi], y_start = d$Y1[hi],
            x_end   = d$X2[hi], y_end   = d$Y2[hi],
            role    = "hover",
            stringsAsFactors = FALSE
          ))
        }
      }

      df_arrows$role <- factor(df_arrows$role, levels = c("base", "hover"))

      fit_t1 <- lm(d$Y1 ~ d$X1)
      fit_t2 <- lm(d$Y2 ~ d$X2)

      ggplot(df_arrows) +
        geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                         color = role, linewidth = role, alpha = role),
                     arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
        scale_color_manual(values = c(base = "steelblue", hover = "#dc3545"), guide = "none") +
        scale_alpha_manual(values = c(base = 0.7, hover = 1), guide = "none") +
        scale_linewidth_manual(values = c(base = 0.7, hover = 1.4), guide = "none") +
        geom_abline(intercept = coef(fit_t1)[1], slope = coef(fit_t1)[2],
                    color = "black", linewidth = 1.1, linetype = "solid") +
        geom_abline(intercept = coef(fit_t2)[1], slope = coef(fit_t2)[2],
                    color = "grey50", linewidth = 1.1, linetype = "dashed") +
        annotate("text", x = Inf, y = -Inf,
                 label = sprintf("T1: r = %.3f", d$stats$conv_T1),
                 hjust = 1.1, vjust = -2.5, size = 4, fontface = "bold",
                 color = "black") +
        annotate("text", x = Inf, y = -Inf,
                 label = sprintf("T2: r = %.3f", d$stats$conv_T2),
                 hjust = 1.1, vjust = -1, size = 4, fontface = "bold",
                 color = "grey50") +
        labs(title = "Pre → Post Trajectories",
             subtitle = paste0(n_show, " randomly selected from n = ",
                               format(length(d$X1), big.mark = ",")),
             x = "Measure X", y = "Measure Y") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", size = 13))
    }, height = 420, width = function() session$clientData$output_arrow_plot_width %||% 500)

    # --- Scatter plot (reacts to unified highlight) ---
    output$change_scatter <- renderPlot({
      df <- scatter_df(); req(df)
      d <- sim()
      r_ch <- d$stats$change_corr

      df$role <- ifelse(df$in_arrows, "linked", "base")
      hi <- highlight_idx()
      if (!is.null(hi)) {
        hit <- which(df$orig_idx == hi)
        if (length(hit) > 0) df$role[hit[1]] <- "hover"
      }
      df$role <- factor(df$role, levels = c("base", "linked", "hover"))

      ggplot(df, aes(x = dX, y = dY)) +
        geom_point(data = df[df$role == "base", ],
                   shape = 1, alpha = 0.3, color = "grey50", size = 1.5, stroke = 0.4) +
        geom_point(data = df[df$role == "linked", ],
                   shape = 16, alpha = 0.7, color = "steelblue", size = 2) +
        geom_point(data = df[df$role == "hover", ],
                   shape = 16, alpha = 1, color = "#dc3545", size = 3.5) +
        geom_smooth(method = "lm", color = "steelblue", se = FALSE, linewidth = 1) +
        annotate("text", x = Inf, y = Inf,
                 label = sprintf("r(ΔX, ΔY) = %.3f", r_ch),
                 hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold",
                 color = "steelblue") +
        labs(title = "Change-Score Scatter (hover either plot to cross-highlight)",
             x = expression(Delta * X), y = expression(Delta * Y)) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", size = 13))
    }, height = 420, width = function() session$clientData$output_change_scatter_width %||% 500)
  })
}
