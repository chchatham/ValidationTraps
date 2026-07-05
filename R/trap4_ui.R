trap4_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(class = "trap-explainer", id = ns("explainer_box"),
      tags$div(class = "trap-explainer-header",
        tags$h4("Trap 4: Expected Attenuation", style = "margin:0; display:inline;"),
        tags$span(class = "trap-explainer-toggle", onclick = paste0(
          "var b=document.getElementById('", ns("explainer_body"), "');",
          "var t=this;",
          "if(b.style.display==='none'){b.style.display='block';t.innerHTML='&#9660;';}",
          "else{b.style.display='none';t.innerHTML='&#9654;';}"
        ), HTML("&#9660;"))
      ),
      tags$div(id = ns("explainer_body"), class = "trap-explainer-body",
        p("A low correlation between change in a biomarker and change in a clinical ",
          "outcome assessment is ", strong("not necessarily"), " a validation problem. ",
          "This is the diagnostic companion to Trap 1: where Trap 1 warns against ",
          "optimism (high cross-sectional validity ≠ correlated change), this warns ",
          "against ", em("premature pessimism"), " (low change correlation ≠ invalid biomarker)."),
        p("Four distinct mechanisms can depress the observed Δbiomarker–ΔCOA ",
          "correlation while an honest structural relationship survives:"),
        tags$ol(
          tags$li(strong("Latent-group heterogeneity:"), " Within each subtype, changes ",
                  "correlate well, but pooling across subtypes cancels the signal (Simpson’s ",
                  "paradox in change-space)."),
          tags$li(strong("Nonisotropic decimation:"), " The COA has coarse resolution exactly ",
                  "where the biomarker is most sensitive, compressing real change into few bin ",
                  "crossings."),
          tags$li(strong("Construct dilution:"), " The biomarker tracks one latent dimension; ",
                  "the COA total score sums many, diluting the on-target correlation."),
          tags$li(strong("Temporal mismatch:"), " The biomarker is prognostic (predicts future ",
                  "state) while the COA is retrospective (reflects past state). Same-window ",
                  "correlation is weak despite both tracking the same process.")
        ),
        p(strong("Implication:"), " Before concluding that a biomarker fails to track clinical ",
          "change, investigate whether the apparent attenuation is an artifact of measurement ",
          "structure, scoring, construct breadth, or temporal alignment.")
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          h4("Panel"),
          radioButtons(ns("panel"), NULL,
            choices = c(
              "A: Group Heterogeneity" = "A",
              "B: COA Decimation" = "B",
              "C: Construct Dilution" = "C",
              "D: Temporal Mismatch" = "D"
            ),
            selected = "A"
          ),
          hr(),
          uiOutput(ns("panel_sliders")),
          conditionalPanel(
            condition = sprintf("input['%s'] !== 'D'", ns("panel")),
            actionButton(ns("resimulate"), "Resimulate",
                         class = "btn-primary", style = "width:100%;")
          )
        )
      ),
      column(9,
        conditionalPanel(
          condition = sprintf("input['%s'] !== 'C'", ns("panel")),
          plotOutput(ns("panel_plot"), height = "480px")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'C'", ns("panel")),
          plotlyOutput(ns("panel_c_3d"), height = "520px")
        ),
        wellPanel(style = "padding: 10px 15px;",
          h4("Summary Statistics", style = "margin-bottom:6px;"),
          uiOutput(ns("stats_ui"))
        )
      )
    )
  )
}

trap4_server <- function(id, preset = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seed <- reactiveVal(42L)

    observeEvent(preset(), {
      p <- preset()
      if (!is.null(p$panel)) updateRadioButtons(session, "panel", selected = p$panel)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$resimulate, {
      seed(sample.int(1e6, 1))
    })

    classify_r <- function(v) ifelse(abs(v) > 0.6, "Strong", ifelse(abs(v) > 0.3, "Moderate", "Weak"))

    interp_color <- function(label) {
      switch(label,
        "Strong" = "#28a745", "High" = "#28a745",
        "Moderate" = "#666666",
        "Weak" = "#dc3545", "Low" = "#dc3545", "#333")
    }

    render_val <- function(val, label) {
      tags$span(style = paste0("font-weight:bold; color:", interp_color(label)),
                sprintf("%.3f", val))
    }

    # --- Dynamic sliders per panel ---
    output$panel_sliders <- renderUI({
      panel <- input$panel
      if (panel == "A") {
        tagList(
          h4("Panel A Parameters"),
          sliderInput(ns("a_within_r"), HTML("Within-Group r <span class='param-help' data-tip='Correlation between ΔBiomarker and ΔCOA within each latent subtype. High values mean the biomarker tracks change within homogeneous groups.'>?</span>"),
                      min = 0.3, max = 0.95, value = 0.78, step = 0.01),
          sliderInput(ns("a_n_per_group"), HTML("Subjects per Group <span class='param-help' data-tip='Number of individuals in each latent subtype. More subjects stabilize the within-group and pooled correlation estimates.'>?</span>"),
                      min = 50, max = 500, value = 150, step = 10),
          sliderInput(ns("a_x_sd"), HTML("Within-Group Spread <span class='param-help' data-tip='Standard deviation of ΔBiomarker within each group. Larger spreads make the within-group slopes more visible.'>?</span>"),
                      min = 0.3, max = 2.0, value = 0.85, step = 0.05)
        )
      } else if (panel == "B") {
        tagList(
          h4("Panel B Parameters"),
          sliderInput(ns("b_gain_center"), HTML("Biomarker Sensitivity Center <span class='param-help' data-tip='Latent trait value where the biomarker has peak sensitivity. The biomarker responds most to change below this point and is nearly flat above it.'>?</span>"),
                      min = 1, max = 8, value = 3.0, step = 0.5),
          sliderInput(ns("b_warp_exp"), HTML("COA Bin Non-uniformity <span class='param-help' data-tip='Controls how unequal the COA bins are. Values < 1 make low-end bins coarse (wide) and high-end bins fine. Higher values make bins more uniform.'>?</span>"),
                      min = 0.2, max = 1.5, value = 0.5, step = 0.1),
          sliderInput(ns("b_n_bins"), HTML("Number of COA Bins <span class='param-help' data-tip='Total ordinal bins in the COA scale. More bins generally improve resolution but the non-uniformity effect persists.'>?</span>"),
                      min = 4, max = 20, value = 10, step = 1),
          sliderInput(ns("b_n"), HTML("Sample Size <span class='param-help' data-tip='Number of simulated individuals. Larger samples give more stable correlation estimates.'>?</span>"),
                      min = 200, max = 2000, value = 700, step = 100)
        )
      } else if (panel == "C") {
        tagList(
          h4("Panel C Parameters"),
          sliderInput(ns("c_k_traits"), HTML("Number of Latent Traits <span class='param-help' data-tip='Dimensions contributing to the COA total score. More traits dilute the biomarker-target correlation because more off-target variance enters the total. The key below the plot shows each trait axis.'>?</span>"),
                      min = 3, max = 15, value = 5, step = 1),
          sliderInput(ns("c_mode_spread"), HTML("Biomarker Concentration <span class='param-help' data-tip='How concentrated the biomarker sensitivity pattern is in latent-trait space. Lower values = sharper density peaks (more specific). Higher values = more diffuse (more noise).'>?</span>"),
                      min = 0.3, max = 3.0, value = 1.0, step = 0.1)
        )
      } else {
        tagList(
          h4("Panel D Parameters"),
          sliderInput(ns("d_shape_bio"), HTML("Biomarker Peak Shape <span class='param-help' data-tip='Gamma shape parameter for the biomarker sensitivity curve. Higher values produce a later, sharper peak.'>?</span>"),
                      min = 1.5, max = 8, value = 4, step = 0.5),
          sliderInput(ns("d_rate_bio"), HTML("Biomarker Peak Rate <span class='param-help' data-tip='Gamma rate parameter for the biomarker. Higher rates compress the curve (earlier, narrower peak).'>?</span>"),
                      min = 0.3, max = 3, value = 1.1, step = 0.1),
          sliderInput(ns("d_shape_coa"), HTML("COA Peak Shape <span class='param-help' data-tip='Gamma shape parameter for the COA recall sensitivity curve. Higher values produce a later (more negative), sharper peak.'>?</span>"),
                      min = 1.5, max = 8, value = 3.5, step = 0.5),
          sliderInput(ns("d_rate_coa"), HTML("COA Peak Rate <span class='param-help' data-tip='Gamma rate parameter for the COA. Higher rates compress the curve toward the assessment point.'>?</span>"),
                      min = 0.3, max = 3, value = 1.0, step = 0.1)
        )
      }
    })

    # --- Simulations ---
    sim_a <- reactive({
      req(input$panel == "A", input$a_within_r, input$a_n_per_group)
      tryCatch(
        simulate_trap4_groups(
          n_per_group = input$a_n_per_group,
          within_r = input$a_within_r,
          x_sd = input$a_x_sd %||% 0.85,
          seed = seed()
        ),
        error = function(e) { showNotification(e$message, type = "error"); NULL }
      )
    })

    sim_b <- reactive({
      req(input$panel == "B", input$b_gain_center, input$b_warp_exp)
      tryCatch(
        simulate_trap4_decimation(
          n = input$b_n %||% 700,
          gain_center = input$b_gain_center,
          warp_exp = input$b_warp_exp,
          n_bins = input$b_n_bins %||% 10,
          seed = seed()
        ),
        error = function(e) { showNotification(e$message, type = "error"); NULL }
      )
    })

    sim_c <- reactive({
      req(input$panel == "C", input$c_k_traits)
      tryCatch(
        generate_trap4c_surfaces(
          k_traits = input$c_k_traits,
          mode_spread = input$c_mode_spread %||% 1.0,
          seed = seed()
        ),
        error = function(e) { showNotification(e$message, type = "error"); NULL }
      )
    })

    sim_d <- reactive({
      req(input$panel == "D", input$d_shape_bio, input$d_rate_bio)
      generate_trap4_temporal(
        shape_bio = input$d_shape_bio,
        rate_bio = input$d_rate_bio,
        shape_coa = input$d_shape_coa %||% 3.5,
        rate_coa = input$d_rate_coa %||% 1.0
      )
    })

    NAVY  <- "#2c3e50"
    RED   <- "#dc3545"
    BLUE  <- "steelblue"
    GREY  <- "grey60"
    GROUP_PAL <- c("steelblue", "#5b6b73", "#3f7d78", "#8a6d52")

    # --- Plot ---
    output$panel_plot <- renderPlot({
      panel <- input$panel

      if (panel == "A") {
        d <- sim_a(); if (is.null(d)) return(NULL)
        set.seed(seed())
        n_sub <- min(300, nrow(d$data))
        idx <- sample(nrow(d$data), n_sub)

        ggplot(d$data[idx, ], aes(dX, dY, color = group)) +
          geom_point(shape = 1, alpha = 0.5, size = 1.5, stroke = 0.5) +
          geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, aes(group = group)) +
          geom_smooth(data = d$data[idx, ], method = "lm", se = FALSE, linewidth = 1.1,
                      color = RED, aes(group = 1)) +
          scale_color_manual(values = GROUP_PAL, guide = "none") +
          annotate("text", x = Inf, y = Inf,
                   label = sprintf("within r = %.2f\npooled r = %.2f",
                                   d$stats$within_r_mean, d$stats$pooled_r),
                   hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold", color = NAVY) +
          labs(title = "A. Latent-Group Heterogeneity",
               subtitle = sprintf("%d per group × %d groups (%d shown)",
                                  d$stats$n_groups * (nrow(d$data) %/% d$stats$n_groups %/% d$stats$n_groups),
                                  d$stats$n_groups, n_sub),
               x = expression(Delta * "Biomarker"), y = expression(Delta * "COA")) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", size = 13))

      } else if (panel == "B") {
        d <- sim_b(); if (is.null(d)) return(NULL)
        gcurve <- data.frame(L = seq(0, 10, length.out = 200))
        gcurve$sens <- d$gain_fun(gcurve$L)
        gcurve$sens <- gcurve$sens / max(gcurve$sens)
        res <- d$mapping
        res$res_n <- res$resolution / max(res$resolution)

        ggplot() +
          geom_line(data = gcurve, aes(L, sens), color = NAVY, linewidth = 1.1) +
          geom_line(data = res, aes(mid, res_n), color = RED, linewidth = 0.8) +
          geom_point(data = res, aes(mid, res_n), color = RED, size = 1.5) +
          annotate("text", x = 0.5, y = 0.65, label = "biomarker\nsensitivity", color = NAVY,
                   size = 4, hjust = 0, lineheight = 0.85, fontface = "bold") +
          annotate("text", x = 9.5, y = 0.55, label = "COA\nresolution", color = RED,
                   size = 4, hjust = 1, lineheight = 0.85, fontface = "bold") +
          scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.08)) +
          annotate("text", x = Inf, y = Inf,
                   label = sprintf("latent r = %.2f\nbinned r = %.2f",
                                   d$stats$r_latent_overall, d$stats$r_binned_obs),
                   hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold", color = NAVY) +
          labs(title = "B. Range-Localized Biomarker × Coarse COA Bins",
               subtitle = sprintf("%d bins, warp = %.1f, gain center = %.1f",
                                  d$stats$n_bins, input$b_warp_exp, d$stats$gain_center),
               x = "latent trait", y = "relative sensitivity / resolution") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", size = 13))

      } else if (panel == "C") {
        return(NULL)

      } else {
        d <- sim_d(); if (is.null(d)) return(NULL)
        curves <- d$curves

        ggplot(curves) +
          annotate("rect", xmin = -6, xmax = 0, ymin = -Inf, ymax = Inf,
                   fill = GREY, alpha = 0.10) +
          geom_vline(xintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.4) +
          geom_line(aes(t, biomarker), color = NAVY, linewidth = 1.1) +
          geom_line(aes(t, coa), color = RED, linewidth = 0.8) +
          annotate("text", x = 5.5, y = 0.65, label = "causal markers\nbetter predict\nfuture states",
                   color = NAVY, size = 3.5, hjust = 0, lineheight = 0.85, fontface = "bold") +
          annotate("text", x = -5.5, y = 0.65, label = "COA not uniformly\nsensitive across\nrecall period",
                   color = RED, size = 3.5, hjust = 1, lineheight = 0.85, fontface = "bold") +
          annotate("text", x = Inf, y = Inf,
                   label = sprintf("bio peak = t+%.1f\nCOA peak = t%.1f\ngap = %.1f",
                                   d$stats$bio_peak_t, d$stats$coa_peak_t, d$stats$temporal_gap),
                   hjust = 1.1, vjust = 1.5, size = 4.5, fontface = "bold", color = NAVY) +
          scale_x_continuous(expand = c(0, 0), breaks = seq(-9, 9, 3)) +
          scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.08)) +
          labs(title = "D. Temporal Mismatch",
               subtitle = "Biomarker (navy) is prognostic; COA (red) is retrospective",
               x = "time from assessment",
               y = "variance explained in clinical state") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(face = "bold", size = 13))
      }
    }, height = 480, width = function() session$clientData$output_panel_plot_width %||% 700)

    # --- Panel C: interactive 3D plotly ---
    output$panel_c_3d <- renderPlotly({
      d <- sim_c(); req(d)

      cs <- list(
        list(0, "rgb(245, 248, 252)"),
        list(0.25, "rgb(200, 218, 235)"),
        list(0.5, "rgb(140, 180, 210)"),
        list(0.75, "rgb(100, 155, 195)"),
        list(1, "rgb(70, 130, 180)")
      )

      p <- plot_ly()
      n_surf <- length(d$surfaces)
      for (i in seq_len(n_surf)) {
        surf <- d$surfaces[[i]]
        p <- p %>% add_surface(
          x = surf$x, y = surf$z, z = surf$y,
          surfacecolor = surf$density,
          colorscale = cs,
          opacity = 0.65,
          showscale = (i == n_surf),
          colorbar = if (i == n_surf) list(
            title = list(text = "biomarker", font = list(size = 11)),
            len = 0.5, thickness = 15, x = 1.02,
            tickvals = list(0, 1), ticktext = list("low", "high")
          ) else NULL,
          lighting = list(ambient = 0.85, diffuse = 0.3, specular = 0.05, roughness = 0.9),
          hoverinfo = "skip",
          name = ""
        )
      }

      for (j in seq_len(nrow(d$key_arrows))) {
        ka <- d$key_arrows[j, ]
        p <- p %>%
          add_trace(type = "scatter3d", mode = "lines",
                    x = c(ka$x0, ka$x1), y = c(ka$z0, ka$z1), z = c(ka$y0, ka$y1),
                    line = list(color = "grey45", width = 8),
                    showlegend = FALSE, hoverinfo = "skip") %>%
          add_trace(type = "scatter3d", mode = "markers+text",
                    x = ka$x1, y = ka$z1, z = ka$y1,
                    marker = list(size = 5, color = "grey45"),
                    text = ka$lab, textposition = "top center",
                    textfont = list(color = "#444", size = 14),
                    showlegend = FALSE, hoverinfo = "skip")
      }

      ca <- d$coa_arrow
      p <- p %>%
        add_trace(type = "scatter3d", mode = "lines",
                  x = c(ca$x0, ca$x1), y = c(ca$z0, ca$z1), z = c(ca$y0, ca$y1),
                  line = list(color = "#dc3545", width = 12),
                  showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(type = "scatter3d", mode = "markers+text",
                  x = ca$x1, y = ca$z1, z = ca$y1,
                  marker = list(size = 7, color = "#dc3545"),
                  text = "COA total", textposition = "top center",
                  textfont = list(color = "#dc3545", size = 15),
                  showlegend = FALSE, hoverinfo = "skip")

      kc <- d$key_center
      p <- p %>%
        add_trace(type = "scatter3d", mode = "markers+text",
                  x = kc[1], y = kc[3], z = kc[2] - 0.3,
                  marker = list(size = 1, color = "rgba(0,0,0,0)"),
                  text = "latent-trait axes (length ~ COA weight)",
                  textposition = "bottom center",
                  textfont = list(color = "#888", size = 11),
                  showlegend = FALSE, hoverinfo = "skip")

      p %>% layout(
        scene = list(
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE,
                       zeroline = FALSE, showbackground = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE,
                       zeroline = FALSE, showbackground = FALSE),
          zaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE,
                       zeroline = FALSE, showbackground = FALSE),
          camera = list(eye = list(x = 0.2, y = -2.5, z = 1.0)),
          aspectmode = "data"
        ),
        showlegend = FALSE,
        margin = list(l = 0, r = 0, t = 40, b = 0),
        title = list(
          text = sprintf(
            "C. Construct Dilution - %d traits - r(target)=%.2f  r(total)=%.2f",
            d$stats$k_factors, d$stats$r_target, d$stats$r_total
          ),
          font = list(size = 13),
          x = 0.02
        )
      )
    })

    # --- Stats table ---
    output$stats_ui <- renderUI({
      panel <- input$panel

      if (panel == "A") {
        d <- sim_a(); req(d); s <- d$stats
        pooled_l <- classify_r(s$pooled_r)
        within_l <- classify_r(s$within_r_mean)
        tags$table(class = "table table-hover stats-table stats-table-compact",
                   style = "width:100%; margin-bottom:0;",
          tags$thead(tags$tr(
            tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
          )),
          tags$tbody(
            tags$tr(
              tags$td("Within-Group r (mean)"),
              tags$td(render_val(s$within_r_mean, within_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(within_l)),
                                within_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                " — honest tracking within subtypes"))
            ),
            tags$tr(
              tags$td("Pooled r (all groups)"),
              tags$td(render_val(s$pooled_r, pooled_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(pooled_l)),
                                pooled_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                " — attenuated by between-group structure"))
            ),
            do.call(tagList, lapply(seq_along(s$within_r), function(g) {
              gl <- classify_r(s$within_r[g])
              tags$tr(
                tags$td(sprintf("Group %d r", g)),
                tags$td(render_val(s$within_r[g], gl)),
                tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(gl)),
                                  sprintf("slope = %.2f", s$slopes[g])))
              )
            }))
          )
        )

      } else if (panel == "B") {
        d <- sim_b(); req(d); s <- d$stats
        lat_l <- classify_r(s$r_latent_overall)
        bin_l <- classify_r(s$r_binned_obs)
        low_l <- classify_r(s$r_latent_lowrange)
        tags$table(class = "table table-hover stats-table stats-table-compact",
                   style = "width:100%; margin-bottom:0;",
          tags$thead(tags$tr(
            tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
          )),
          tags$tbody(
            tags$tr(
              tags$td("Latent r (overall)"),
              tags$td(render_val(s$r_latent_overall, lat_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(lat_l)), lat_l))
            ),
            tags$tr(
              tags$td("Latent r (active range)"),
              tags$td(render_val(s$r_latent_lowrange, low_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(low_l)), low_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                sprintf(" — %.0f%% of subjects in active range", s$pct_low * 100)))
            ),
            tags$tr(style = "border-top: 2px solid #dee2e6;",
              tags$td(tags$strong("Observed r (binned COA)")),
              tags$td(render_val(s$r_binned_obs, bin_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(bin_l)), bin_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                " — what you’d observe"))
            )
          )
        )

      } else if (panel == "C") {
        d <- sim_c(); req(d); s <- d$stats
        tar_l <- classify_r(s$r_target)
        tot_l <- classify_r(s$r_total)
        tags$table(class = "table table-hover stats-table stats-table-compact",
                   style = "width:100%; margin-bottom:0;",
          tags$thead(tags$tr(
            tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
          )),
          tags$tbody(
            tags$tr(
              tags$td("r(ΔBiomarker, target subscale)"),
              tags$td(render_val(s$r_target, tar_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(tar_l)), tar_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                " — on-target tracking"))
            ),
            tags$tr(style = "border-top: 2px solid #dee2e6;",
              tags$td(tags$strong("r(ΔBiomarker, total score)")),
              tags$td(render_val(s$r_total, tot_l)),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", interp_color(tot_l)), tot_l),
                      tags$span(style = "color:#888; font-size:0.9em;",
                                sprintf(" — diluted across %d factors", s$k_factors)))
            ),
            tags$tr(
              tags$td("cos(loading, diagonal)"),
              tags$td(tags$span(style = "font-weight:bold;", sprintf("%.3f", s$cos_to_total))),
              tags$td(tags$span(style = "color:#888; font-size:0.9em;",
                                "theoretical dilution factor"))
            )
          )
        )

      } else {
        d <- sim_d(); req(d); s <- d$stats
        tags$table(class = "table table-hover stats-table stats-table-compact",
                   style = "width:100%; margin-bottom:0;",
          tags$thead(tags$tr(
            tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
          )),
          tags$tbody(
            tags$tr(
              tags$td("Biomarker peak time"),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", NAVY),
                                sprintf("t + %.1f", s$bio_peak_t))),
              tags$td(tags$span(style = "color:#888; font-size:0.9em;",
                                "prognostic — peaks after assessment"))
            ),
            tags$tr(
              tags$td("COA peak time"),
              tags$td(tags$span(style = paste0("font-weight:bold; color:", RED),
                                sprintf("t %.1f", s$coa_peak_t))),
              tags$td(tags$span(style = "color:#888; font-size:0.9em;",
                                "retrospective — peaks before assessment"))
            ),
            tags$tr(style = "border-top: 2px solid #dee2e6;",
              tags$td(tags$strong("Temporal gap")),
              tags$td(tags$span(style = "font-weight:bold;",
                                sprintf("%.1f time units", s$temporal_gap))),
              tags$td(tags$span(style = "color:#888; font-size:0.9em;",
                                "separation between peak sensitivities"))
            )
          )
        )
      }
    })
  })
}
