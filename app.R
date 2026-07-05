library(shiny)
library(MASS)
library(ggplot2)
library(pROC)
library(plotly)
library(scales)

source("R/trap1_sim.R")
source("R/trap2_sim.R")
source("R/trap3_sim.R")
source("R/trap4_sim.R")
source("R/trap1_ui.R")
source("R/trap2_ui.R")
source("R/trap3_ui.R")
source("R/trap4_ui.R")

ui <- navbarPage(
  title = "Validation Traps",
  id = "main_nav",
  selected = "about",
  theme = NULL,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      window.addEventListener('message', function(e) {
        if (e.data && e.data.type === 'navigateToTrap') {
          Shiny.setInputValue('nav_command', e.data, {priority: 'event'});
        }
      });
    "))
  ),

  tabPanel("About", value = "about",
    tags$iframe(
      src = "about.html",
      width = "100%",
      style = "border: none; height: calc(100vh - 60px); display: block;"
    )
  ),

  tabPanel("Trap 1: Change-Score Paradox", value = "trap1",
    trap1_ui("trap1")
  ),

  tabPanel("Trap 2: Reverse Simpson's", value = "trap2",
    trap2_ui("trap2")
  ),

  tabPanel("Trap 3: Portfolio Approach", value = "trap3",
    trap3_ui("trap3")
  ),

  tabPanel("Trap 4: Expected Attenuation", value = "trap4",
    trap4_ui("trap4")
  )
)

server <- function(input, output, session) {
  trap1_preset <- reactiveVal(NULL)
  trap2_preset <- reactiveVal(NULL)
  trap3_preset <- reactiveVal(NULL)
  trap4_preset <- reactiveVal(NULL)

  trap1_server("trap1", preset = trap1_preset)
  trap2_server("trap2", preset = trap2_preset)
  trap3_server("trap3", preset = trap3_preset)
  trap4_server("trap4", preset = trap4_preset)

  # --- Handle navigation commands from About page iframe ---
  observeEvent(input$nav_command, {
    cmd <- input$nav_command
    trap <- cmd$trap
    params <- cmd$params

    # Suppress intro modal — user already read the About page
    if (trap == "trap1") visited$trap1 <- TRUE
    if (trap == "trap2") visited$trap2 <- TRUE
    if (trap == "trap3") visited$trap3 <- TRUE
    if (trap == "trap4") visited$trap4 <- TRUE

    updateNavbarPage(session, "main_nav", selected = trap)

    if (trap == "trap1") {
      trap1_preset(params)
    } else if (trap == "trap2") {
      trap2_preset(params)
    } else if (trap == "trap3") {
      if (is.list(params$sensitivities)) params$sensitivities <- unlist(params$sensitivities)
      if (is.list(params$specificities)) params$specificities <- unlist(params$specificities)
      trap3_preset(params)
    } else if (trap == "trap4") {
      trap4_preset(params)
    }
  })

  # --- Intro modals (once per tab, per session) ---
  visited <- reactiveValues(trap1 = FALSE, trap2 = FALSE, trap3 = FALSE, trap4 = FALSE)

  show_intro <- function(title, ...) {
    showModal(modalDialog(
      title = title,
      ...,
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Got it — let me explore")
    ))
  }

  observeEvent(input$main_nav, {
    if (input$main_nav == "trap1" && !visited$trap1) {
      visited$trap1 <- TRUE
      show_intro(
        "Trap 1: The Change-Score Paradox",
        p("Two measures can correlate ", strong("r = 0.95"), " at any single timepoint ",
          "yet show ", strong("near-zero correlation"), " in their change scores."),
        p("Use the sliders on the left to adjust parameters, or ",
          strong("click the interpretation labels"), " in the table below the plots ",
          "to set targets and see which parameter combinations produce them."),
        p("Hover over points in the right-hand scatter plot to see the corresponding ",
          "trajectory highlighted in red on the left-hand arrow diagram.")
      )
    }
    if (input$main_nav == "trap2" && !visited$trap2) {
      visited$trap2 <- TRUE
      show_intro(
        "Trap 2: Simpson's Paradox in Reverse",
        p("A biomarker that appears ", strong("unrelated"), " to an outcome across ",
          "individuals may track within-person changes ", strong("beautifully"), "."),
        p("Compare the cross-sectional scatter (left) with the within-person change ",
          "scatter (right) to see how population-level and individual-level ",
          "relationships can tell opposite stories."),
        p("This is the ", em("converse"), " of Trap 1: there, high cross-sectional ",
          "validity masked weak change coupling. Here, weak cross-sectional validity ",
          "masks strong change coupling.")
      )
    }
    if (input$main_nav == "trap3" && !visited$trap3) {
      visited$trap3 <- TRUE
      show_intro(
        "Trap 3: The Portfolio Approach",
        p("No single biomarker needs to be a ", strong("“unicorn”"), " with both ",
          "high sensitivity and high specificity."),
        p("Combine complementary markers — some sensitive (good at catching cases), ",
          "others specific (good at ruling in disease) — and try different ",
          "combination rules to see how a portfolio can ", strong("outperform any ",
          "individual marker"), "."),
        p("Adjust marker profiles and prevalence to see how the optimal strategy changes ",
          "with the clinical context.")
      )
    }
    if (input$main_nav == "trap4" && !visited$trap4) {
      visited$trap4 <- TRUE
      show_intro(
        "Trap 4: Expected Attenuation",
        p("A low Δbiomarker–ΔCOA correlation is ", strong("not necessarily"),
          " a validation problem. This is the diagnostic companion to Trap 1."),
        p("Select a panel (A–D) to explore four distinct mechanisms that can ",
          "depress the observed change correlation while an honest structural ",
          "relationship survives: group heterogeneity, COA resolution mismatch, ",
          "construct dilution, and temporal mismatch."),
        p("Adjust parameters with the sliders to see how each mechanism ",
          "attenuates the observed correlation.")
      )
    }
  })
}

shinyApp(ui, server)
