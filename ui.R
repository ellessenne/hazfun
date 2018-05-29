ui <- shiny::fluidPage(

  # Application title
  shiny::titlePanel("HazFun: plotting hazard functions"),

  # Sidebar with inputs
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput(inputId = "maxt", label = "Max time t:", min = 1, max = 10, value = 5, step = 1),
      shiny::selectInput(inputId = "distribution",
                         label = "Distribution of the baseline hazard:",
                         choices = c("Exponential" = "exp",
                                     "Weibull" = "wei",
                                     "Inverse Weibull" = "iwei",
                                     "Gompertz" = "gom",
                                     "Log-Normal" = "logn",
                                     "Log-logistic" = "logl")),
      shiny::uiOutput(outputId = "pars")
    ),

    # Show a plot of the desired hazard
    shiny::mainPanel(
      shiny::plotOutput("hazPlot")
    )
  )
)
