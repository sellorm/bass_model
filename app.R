# Small shiny app to visualise the Bass diffusion model

library(shiny)
library(diffusion)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Bass Diffusion Model Visualisation"),

  sidebarLayout(
    sidebarPanel(
      h2("Baseline model (blue)"),
      sliderInput(
        "innovation_base",
        "Innovation effect:",
        min = 0,
        max = 1,
        value = 0.03
      ),
      sliderInput(
        "imitation_base",
        "Imitation effect:",
        min = 0,
        max = 1,
        value = 0.38
      ),
      h2("Challenger model (red)"),
      sliderInput(
        "innovation",
        "Innovation effect:",
        min = 0,
        max = 1,
        value = 0.02
      ),
      sliderInput(
        "imitation",
        "Imitation effect:",
        min = 0,
        max = 1,
        value = 0.42
      )
    ),


    # Show the plots
    mainPanel(
      h2("Rate of adoption"),
      plotOutput("adoptionPlot"),
      h2("Cumulative adoption"),
      plotOutput("cumAdoptionPlot"))
  ))

# Define server logic
server <- function(input, output) {
  output$adoptionPlot <- renderPlot({
    bass_base <-
      difcurve(
        n = 20,
        w = c(input$innovation_base, input$imitation_base, 100),
        type = "bass"
      )
    bass_challenger <-
      difcurve(
        n = 20,
        w = c(input$innovation, input$imitation, 100),
        type = "bass"
      )

    plot_adoption <- ggplot(as.data.frame(bass_base)) +
      aes(y = Adoption, x = 1:nrow(bass_base)) +
      geom_line(colour = "blue") +
      geom_line(data = as.data.frame(bass_challenger), colour = 'red') +
      labs(x = "years")

    plot_adoption
  })
  output$cumAdoptionPlot <- renderPlot({
    bass_base <-
      difcurve(
        n = 20,
        w = c(input$innovation_base, input$imitation_base, 100),
        type = "bass"
      )

    bass_challenger <-
      difcurve(
        n = 20,
        w = c(input$innovation, input$imitation, 100),
        type = "bass"
      )

    plot_cumAdoption <- ggplot(as.data.frame(bass_base)) +
      aes(y = `Cumulative Adoption`, x = 1:nrow(bass_base)) +
      geom_line(colour = "blue") +
      geom_line(data = as.data.frame(bass_challenger), colour = 'red') +
      labs(x = "years")

    plot_cumAdoption
  })
}

# Run the application
shinyApp(ui = ui, server = server)
