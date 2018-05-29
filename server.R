source("global.R")

server <- function(input, output, session) {

  # Make inputs
  output$pars <- shiny::renderUI({
    switch(
      input$distribution,
      "exp" = shiny::tagList(
        shiny::sliderInput(inputId = "lambda", label = "Lambda", min = 0.01, max = 5, step = 0.01, value = 1)
      ),
      "wei" = shiny::tagList(
        shiny::sliderInput(inputId = "lambda", label = "Lambda", min = 0.01, max = 5, step = 0.01, value = 1.2),
        shiny::sliderInput(inputId = "p", label = "P", min = 0.01, max = 5, step = 0.01, value = 1.2)
      ),
      "iwei" = shiny::tagList(
        shiny::sliderInput(inputId = "lambda", label = "Lambda", min = 0.01, max = 5, step = 0.01, value = 1.2),
        shiny::sliderInput(inputId = "p", label = "P", min = 0.01, max = 5, step = 0.01, value = 1.2)
      ),
      "gom" = shiny::tagList(
        shiny::sliderInput(inputId = "lambda", label = "Lambda", min = 0.01, max = 5, step = 0.01, value = 0.1),
        shiny::sliderInput(inputId = "gamma", label = "Gamma", min = 0.01, max = 5, step = 0.01, value = 0.5)
      ),
      "logn" = shiny::tagList(
        shiny::sliderInput(inputId = "mu", label = "Mu", min = -5, max = 5, step = 0.01, value = 0),
        shiny::sliderInput(inputId = "sigma", label = "Sigma", min = 0.01, max = 5, step = 0.01, value = 1)
      ),
      "logl" = shiny::tagList(
        shiny::sliderInput(inputId = "alpha", label = "Alpha", min = -5, max = 5, step = 0.01, value = 0),
        shiny::sliderInput(inputId = "kappa", label = "Kappa", min = 0.01, max = 5, step = 0.01, value = 1)
      ),
      "mww" = shiny::tagList(
        shiny::sliderInput(inputId = "lambda1", label = "Lambda 1", min = 0.01, max = 5, step = 0.01, value = 0.1),
        shiny::sliderInput(inputId = "p1", label = "P 1", min = 0.01, max = 5, step = 0.01, value = 3),
        shiny::sliderInput(inputId = "lambda2", label = "Lambda 2", min = 0.01, max = 5, step = 0.01, value = 0.1),
        shiny::sliderInput(inputId = "p2", label = "P 2", min = 0.01, max = 5, step = 0.01, value = 1.6),
        shiny::sliderInput(inputId = "mixture", label = "Mixture", min = 0.01, max = 1, step = 0.01, value = 0.8)
      )
    )
  })

  # Make dataset to plot
  data <- shiny::reactive({
    data <- data.frame(t = seq(0, input$maxt, length.out = 1000))
    data$h <- switch(
      input$distribution,
      "exp" = input$lambda,
      "wei" = input$lambda * input$p * data$t^(input$p - 1),
      "iwei" = (input$lambda * input$p * data$t^(-(input$p - 1))) / (exp(input$lambda * data$t^(-input$p)) - 1),
      "gom" = input$lambda * exp(input$gamma * data$t),
      "logn" = dnorm((log(data$t) - input$mu) / input$sigma) / (input$sigma * data$t * (1 - pnorm((log(data$t) - input$mu) / input$sigma))),
      "logl" = (exp(input$alpha) * input$kappa * data$t^(input$kappa - 1)) / (1 + exp(input$alpha) * data$t^input$kappa),
      "mww" = (input$lambda1 * input$p1 * data$t^(input$p1 - 1) * input$mixture * exp(-input$lambda1 * data$t^input$p1) + input$lambda2 * input$p2 * data$t^(input$p2 - 1) * (1 - input$mixture) * exp(-input$lambda2 * data$t^input$p2)) / (input$mixture * exp(-input$lambda1 * data$t^input$p1) + (1 - input$mixture) * exp(-input$lambda2 * data$t^input$p2))
    )
    data
  })

  # Make plot with hazard function
  output$hazPlot <- shiny::renderPlot({
    ggplot2::ggplot(data(), aes(x = t, y = h)) +
      ggplot2::geom_line()
  })
}
