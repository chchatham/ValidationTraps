trap5_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(class = "trap-explainer", id = ns("explainer_box"),
      tags$div(class = "trap-explainer-header",
        tags$h4("Trap 5: Temporal Accumulation", style = "margin:0; display:inline;"),
        tags$span(class = "trap-explainer-toggle", onclick = paste0(
          "var b=document.getElementById('", ns("explainer_body"), "');",
          "var t=this;",
          "if(b.style.display==='none'){b.style.display='block';t.innerHTML='&#9660;';}",
          "else{b.style.display='none';t.innerHTML='&#9654;';}"
        ), HTML("&#9660;"))
      ),
      tags$div(id = ns("explainer_body"), class = "trap-explainer-body",
        p("A biomarker that responds within hours to physiological change may appear to ",
          strong("lead"), " a clinical outcome assessment (COA) by days or weeks. ",
          "This is not because the biomarker is prognostic; it is because the COA ",
          "reflects a ", strong("recency-weighted accumulation"), " of the patient's ",
          "experience over a recall period."),
        p("When a patient reports how they have been feeling over the past week or month, ",
          "their response integrates recent experience through a perceptual filter: very ",
          "recent events may not yet have registered, and distant events are down-weighted. ",
          "The shape of this recall kernel determines how much the COA lags behind the ",
          "biomarker and how strongly the two correlate."),
        p(strong("Implication:"), " A non-zero peak lag in the cross-correlation between ",
          "biomarker and COA does not indicate that the biomarker is a leading indicator ",
          "of future clinical state. It may simply reflect the temporal integration inherent ",
          "in patient-reported outcomes. The optimal analysis window should be matched to ",
          "the recall period of the COA instrument."),
        p("Use the ", strong("Parametric"), " mode to adjust the recall kernel via ",
          "standard parameters, or switch to ", strong("Draw Custom"), " mode to sketch ",
          "any kernel shape directly.")
      )
    ),
    fluidRow(
      column(3,
        wellPanel(
          radioButtons(ns("kernel_mode"), "Kernel Mode",
            choices = c("Parametric" = "parametric", "Draw Custom" = "custom"),
            selected = "parametric", inline = TRUE
          ),
          hr(style = "margin: 8px 0;"),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'parametric'", ns("kernel_mode")),
            sliderInput(ns("recall_period"),
              HTML("Recall Period (days) <span class='param-help' data-tip='How far back the patient recalls when reporting the COA'>?</span>"),
              min = 7, max = 56, value = 28, step = 1),
            sliderInput(ns("recall_shape"),
              HTML("Kernel Shape <span class='param-help' data-tip='Controls where the kernel peaks: higher = peak further from today'>?</span>"),
              min = 1.2, max = 5.0, value = 2.5, step = 0.1),
            sliderInput(ns("recall_rate"),
              HTML("Kernel Rate <span class='param-help' data-tip='Controls kernel width: lower = broader, more spread out'>?</span>"),
              min = 0.04, max = 0.50, value = 0.12, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'custom'", ns("kernel_mode")),
            tags$p(class = "text-muted", style = "font-size: 12px; margin-bottom: 6px;",
              "Draw the recall kernel on Panel B (click & drag). The curve defines how ",
              "the patient weights past experience."),
            actionButton(ns("clear_canvas"), "Clear Drawing",
              class = "btn-sm btn-default", style = "width:100%; margin-bottom: 8px;")
          ),
          hr(style = "margin: 8px 0;"),
          tags$details(
            tags$summary(HTML("&#9881; "), tags$strong("Advanced Parameters"),
              style = "cursor:pointer; font-size: 13px;"),
            sliderInput(ns("ar_coef"),
              HTML("Drive Persistence <span class='param-help' data-tip='AR(1) coefficient of the latent process: higher = slower fluctuations'>?</span>"),
              min = 0.3, max = 0.95, value = 0.75, step = 0.05),
            sliderInput(ns("noise_bio"),
              HTML("Biomarker Noise <span class='param-help' data-tip='Measurement error SD added to the biomarker (signal SD ~ 1.0)'>?</span>"),
              min = 0.0, max = 2.0, value = 0.08, step = 0.05),
            sliderInput(ns("noise_coa"),
              HTML("COA Noise <span class='param-help' data-tip='Measurement error SD added to the COA report'>?</span>"),
              min = 0.0, max = 2.0, value = 0.02, step = 0.05),
            sliderInput(ns("coa_unique_sd"),
              HTML("COA-Unique Variance <span class='param-help' data-tip='SD of latent COA-specific drift unrelated to biomarker (mood, context)'>?</span>"),
              min = 0.0, max = 2.0, value = 0.08, step = 0.05)
          ),
          actionButton(ns("resimulate"), "Resimulate",
            class = "btn-primary", style = "width:100%; margin-top: 8px;")
        )
      ),
      column(9,
        tags$h5(tags$strong("A"), style = "margin: 0 0 2px 4px;"),
        plotOutput(ns("ts_plot"), height = "300px"),
        fluidRow(
          column(6,
            tags$h5(tags$strong("B"), style = "margin: 8px 0 2px 4px;"),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'parametric'", ns("kernel_mode")),
              plotOutput(ns("kernel_plot"), height = "280px")
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'custom'", ns("kernel_mode")),
              tags$canvas(
                id = ns("kernel_canvas"),
                class = "kernel-draw-canvas",
                `data-input-id` = ns("drawn_kernel"),
                `data-max-days` = "42",
                width = "460",
                height = "220",
                style = "border: 1px solid #ddd; cursor: crosshair; width: 100%; border-radius: 4px; background: white;"
              )
            )
          ),
          column(6,
            tags$h5(tags$strong("C"), style = "margin: 8px 0 2px 4px;"),
            plotOutput(ns("ccf_plot"), height = "280px")
          )
        ),
        wellPanel(style = "padding: 10px 15px;",
          h4("Summary Statistics", style = "margin-bottom: 6px;"),
          uiOutput(ns("stats_ui"))
        )
      )
    ),
    # --- Canvas drawing JS ---
    tags$script(HTML("
(function() {
  function initCanvas(canvas) {
    if (canvas._initialized) return;
    canvas._initialized = true;
    var inputId = canvas.getAttribute('data-input-id');
    var maxDays = parseInt(canvas.getAttribute('data-max-days')) || 42;
    var ctx = canvas.getContext('2d');
    var PAD_L = 38, PAD_R = 12, PAD_T = 8, PAD_B = 28;
    var W = canvas.width, H = canvas.height;
    var PW = W - PAD_L - PAD_R, PH = H - PAD_T - PAD_B;

    function d2p(day, wt) {
      return { x: PAD_L + (day / maxDays) * PW, y: PAD_T + (1 - wt) * PH };
    }
    function p2d(px, py) {
      return {
        day: Math.max(0, Math.min(maxDays, ((px - PAD_L) / PW) * maxDays)),
        wt:  Math.max(0, Math.min(1, 1 - (py - PAD_T) / PH))
      };
    }

    var drawing = false, pts = [];

    function drawBg() {
      ctx.clearRect(0, 0, W, H);
      ctx.strokeStyle = '#e8e8e8'; ctx.lineWidth = 1;
      for (var d = 0; d <= maxDays; d += 7) {
        var p = d2p(d, 0);
        ctx.beginPath(); ctx.moveTo(p.x, PAD_T); ctx.lineTo(p.x, H - PAD_B); ctx.stroke();
        ctx.fillStyle = '#888'; ctx.font = '10px sans-serif'; ctx.textAlign = 'center';
        ctx.fillText(d + '', p.x, H - PAD_B + 14);
      }
      for (var w = 0; w <= 1.001; w += 0.25) {
        var p = d2p(0, w);
        ctx.beginPath(); ctx.moveTo(PAD_L, p.y); ctx.lineTo(W - PAD_R, p.y); ctx.stroke();
        ctx.fillStyle = '#888'; ctx.font = '10px sans-serif'; ctx.textAlign = 'right';
        ctx.fillText(w.toFixed(2), PAD_L - 4, p.y + 4);
      }
      ctx.strokeStyle = '#999'; ctx.lineWidth = 1;
      ctx.beginPath(); ctx.moveTo(PAD_L, PAD_T); ctx.lineTo(PAD_L, H - PAD_B);
      ctx.lineTo(W - PAD_R, H - PAD_B); ctx.stroke();
      ctx.fillStyle = '#888'; ctx.font = '10px sans-serif'; ctx.textAlign = 'center';
      ctx.fillText('Days before report', W / 2, H - 2);
    }
    function drawCurve() {
      if (pts.length < 2) return;
      ctx.strokeStyle = '#c0392b'; ctx.lineWidth = 2.5; ctx.beginPath();
      var f = d2p(pts[0].day, pts[0].wt);
      ctx.moveTo(f.x, f.y);
      for (var i = 1; i < pts.length; i++) { var p = d2p(pts[i].day, pts[i].wt); ctx.lineTo(p.x, p.y); }
      ctx.stroke();
    }
    function redraw() {
      drawBg(); drawCurve();
      if (pts.length === 0) {
        ctx.fillStyle = '#aaa'; ctx.font = '13px sans-serif'; ctx.textAlign = 'center';
        ctx.fillText('Click and drag to draw kernel', W / 2, H / 2);
      }
    }
    function mpos(e) {
      var r = canvas.getBoundingClientRect();
      return { x: (e.clientX - r.left) * (W / r.width), y: (e.clientY - r.top) * (H / r.height) };
    }
    function sendKernel() {
      if (pts.length < 2) return;
      var sorted = pts.slice().sort(function(a, b) { return a.day - b.day; });
      var kernel = [];
      for (var d = 0; d < maxDays; d++) {
        var lo = null, hi = null;
        for (var i = 0; i < sorted.length; i++) {
          if (sorted[i].day <= d) lo = sorted[i];
          if (sorted[i].day >= d && hi === null) hi = sorted[i];
        }
        if (lo && hi) {
          if (Math.abs(lo.day - hi.day) < 0.01) kernel.push(lo.wt);
          else { var t = (d - lo.day) / (hi.day - lo.day); kernel.push(lo.wt * (1 - t) + hi.wt * t); }
        } else if (lo) kernel.push(lo.wt);
        else kernel.push(0);
      }
      var mx = Math.max.apply(null, kernel);
      if (mx > 0) kernel = kernel.map(function(v) { return v / mx; });
      Shiny.setInputValue(inputId, kernel, {priority: 'event'});
    }

    canvas.addEventListener('mousedown', function(e) { e.preventDefault(); drawing = true; pts = []; var d = p2d(mpos(e).x, mpos(e).y); pts.push(d); });
    canvas.addEventListener('mousemove', function(e) { if (!drawing) return; pts.push(p2d(mpos(e).x, mpos(e).y)); redraw(); });
    canvas.addEventListener('mouseup', function(e) { drawing = false; sendKernel(); });
    canvas.addEventListener('mouseleave', function(e) { if (drawing) { drawing = false; sendKernel(); } });
    canvas.addEventListener('touchstart', function(e) { e.preventDefault(); drawing = true; pts = [];
      var r = canvas.getBoundingClientRect(); var t = e.touches[0];
      pts.push(p2d((t.clientX - r.left) * (W / r.width), (t.clientY - r.top) * (H / r.height)));
    });
    canvas.addEventListener('touchmove', function(e) { e.preventDefault(); if (!drawing) return;
      var r = canvas.getBoundingClientRect(); var t = e.touches[0];
      pts.push(p2d((t.clientX - r.left) * (W / r.width), (t.clientY - r.top) * (H / r.height)));
      redraw();
    });
    canvas.addEventListener('touchend', function(e) { drawing = false; sendKernel(); });

    canvas._clearDrawing = function() { pts = []; redraw(); Shiny.setInputValue(inputId, null, {priority: 'event'}); };
    redraw();
  }

  Shiny.addCustomMessageHandler('clearKernelCanvas', function(id) {
    var c = document.getElementById(id);
    if (c && c._clearDrawing) c._clearDrawing();
  });

  function initAll() {
    document.querySelectorAll('.kernel-draw-canvas').forEach(function(c) { initCanvas(c); });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initAll);
  else initAll();
  $(document).on('shiny:idle', function() { setTimeout(initAll, 200); });
})();
"))  # JS uses data-input-id attribute; no R templating needed
  )
}

trap5_server <- function(id, preset = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seed <- reactiveVal(42L)

    observeEvent(preset(), {
      p <- preset()
      if (!is.null(p)) {
        if (!is.null(p$recall_period)) updateSliderInput(session, "recall_period", value = p$recall_period)
        if (!is.null(p$recall_shape)) updateSliderInput(session, "recall_shape", value = p$recall_shape)
        if (!is.null(p$recall_rate)) updateSliderInput(session, "recall_rate", value = p$recall_rate)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$resimulate, { seed(sample.int(1e6, 1)) })

    observeEvent(input$clear_canvas, {
      session$sendCustomMessage("clearKernelCanvas", ns("kernel_canvas"))
    })

    sim <- reactive({
      s <- seed()
      mode <- input$kernel_mode
      custom_k <- NULL
      rp <- 42
      rs <- 2.5
      rr <- 0.12

      if (mode == "parametric") {
        rp <- input$recall_period
        rs <- input$recall_shape
        rr <- input$recall_rate
      } else {
        dk <- input$drawn_kernel
        if (!is.null(dk)) {
          custom_k <- as.numeric(dk)
        }
      }

      simulate_trap5(
        n_display = 300,
        n_ccf = 10000,
        ar_coef = input$ar_coef,
        drive_sd = 0.7,
        tau_bio = 0.2,
        recall_period = rp,
        recall_shape = rs,
        recall_rate = rr,
        custom_kernel = custom_k,
        noise_bio = input$noise_bio,
        noise_coa = input$noise_coa,
        coa_unique_sd = input$coa_unique_sd,
        max_lag = 60,
        seed = s
      )
    })

    # --- Panel A: Time series ---
    output$ts_plot <- renderPlot({
      r <- sim()
      df <- data.frame(
        t = rep(r$t, 2),
        value = c(r$bio_z, r$coa_z),
        series = rep(c("Biomarker", "COA"), each = length(r$t))
      )
      df$series <- factor(df$series, levels = c("Biomarker", "COA"))
      ggplot(df, aes(x = t, y = value, color = series)) +
        geom_line(linewidth = 0.55, alpha = 0.85) +
        scale_color_manual(values = c("Biomarker" = "steelblue", "COA" = "#c0392b"), name = NULL) +
        labs(x = "Time (days)", y = "Standardized value") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#e8e8e8"),
          legend.position = c(0.90, 0.92),
          plot.margin = margin(4, 8, 4, 4)
        )
    }, height = 300)

    # --- Panel B: Kernel (parametric mode only) ---
    output$kernel_plot <- renderPlot({
      r <- sim()
      kdf <- data.frame(lag = r$kernel_lags, weight = r$kernel / max(r$kernel))
      com <- r$stats$kernel_com
      ggplot(kdf, aes(x = lag, y = weight)) +
        geom_area(fill = "#c0392b", alpha = 0.20) +
        geom_line(color = "#c0392b", linewidth = 0.8) +
        geom_vline(xintercept = com, linetype = "dashed", color = "#888888", linewidth = 0.4) +
        annotate("text", x = com + 1, y = 0.85,
          label = paste0("COM = ", round(com, 1), " d"),
          hjust = 0, size = 3.2, color = "#555555") +
        labs(x = "Days before report", y = "Relative weight") +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#e8e8e8"),
          plot.margin = margin(4, 8, 4, 4)
        )
    }, height = 280)

    # --- Panel C: CCF (x-axis limited to [-15, max]) ---
    output$ccf_plot <- renderPlot({
      r <- sim()
      ccf_df <- data.frame(lag = r$lags, r = r$ccf)
      ccf_df <- ccf_df[ccf_df$lag >= -15, ]
      pk <- r$stats
      pk_df <- data.frame(lag = pk$peak_lag, r = pk$peak_r)
      z_df <- data.frame(lag = 0, r = pk$r_at_zero)

      p <- ggplot(ccf_df, aes(x = lag, y = r)) +
        geom_hline(yintercept = 0, color = "#999999", linewidth = 0.4) +
        geom_vline(xintercept = 0, color = "#999999", linewidth = 0.4, linetype = "dashed") +
        geom_line(color = "grey30", linewidth = 0.6) +
        geom_point(data = pk_df, color = "steelblue", size = 3, shape = 16) +
        geom_point(data = z_df, color = "#c0392b", size = 3, shape = 17) +
        labs(
          x = expression("Assessment Time Difference (days; COA " * minus * " Biomarker)"),
          y = "Cross-correlation (r)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#e8e8e8"),
          plot.margin = margin(4, 8, 4, 4)
        )

      if (pk$peak_lag >= -15) {
        p <- p +
          annotate("text", x = pk$peak_lag + 2, y = pk$peak_r + 0.06,
            label = sprintf("peak r = %.2f at +%d d", pk$peak_r, pk$peak_lag),
            color = "steelblue", size = 3.2, fontface = "bold", hjust = 0)
      }
      p
    }, height = 280)

    # --- Stats table ---
    interp_color <- function(label) {
      switch(label,
        "Strong" = "#28a745", "Good" = "#28a745",
        "Moderate" = "#666666",
        "Weak" = "#dc3545", "Poor" = "#dc3545", "#333")
    }
    render_val <- function(val, label) {
      tags$span(style = paste0("font-weight:bold; color:", interp_color(label)),
                sprintf("%.3f", val))
    }

    output$stats_ui <- renderUI({
      r <- sim()
      s <- r$stats

      cls_peak <- if (s$peak_r > 0.5) "Strong" else if (s$peak_r > 0.25) "Moderate" else "Weak"
      cls_zero <- if (abs(s$r_at_zero) > 0.5) "Strong" else if (abs(s$r_at_zero) > 0.25) "Moderate" else "Weak"

      tags$table(class = "table table-hover stats-table stats-table-compact",
        tags$thead(tags$tr(
          tags$th("Statistic"), tags$th("Value"), tags$th("Interpretation")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Peak Cross-Correlation"),
            tags$td(render_val(s$peak_r, cls_peak)),
            tags$td(tags$span(style = paste0("color:", interp_color(cls_peak)), cls_peak))
          ),
          tags$tr(
            tags$td("Peak Lag (days)"),
            tags$td(tags$strong(sprintf("+%d", s$peak_lag))),
            tags$td("Biomarker leads COA by this many days")
          ),
          tags$tr(
            tags$td("Cross-Correlation at Lag 0"),
            tags$td(render_val(s$r_at_zero, cls_zero)),
            tags$td(tags$span(style = paste0("color:", interp_color(cls_zero)), cls_zero))
          ),
          tags$tr(
            tags$td("Kernel Center of Mass"),
            tags$td(tags$strong(sprintf("%.1f days", s$kernel_com))),
            tags$td("Average recall lookback")
          ),
          tags$tr(
            tags$td("Recall Period"),
            tags$td(tags$strong(sprintf("%d days", s$recall_period))),
            tags$td("Total recall window")
          )
        )
      )
    })
  })
}
