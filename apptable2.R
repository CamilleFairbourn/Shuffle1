library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(titlePanel("Make a Contingency Table"),
                sidebarLayout(
                  sidebarPanel(
                    tags$div(
                             tags$table(class = "table",
                                        # tags$thead(
                                        #   tags$tr(
                                        #     tags$th("Condition 1"),
                                        #     tags$th("Condition 2")
                                        #   )
                                        # ),
                                        tags$tbody(
                                          tags$tr(
                                            tags$td(numericInput("r1c1", " ", value = "NA")),
                                            tags$td(numericInput("r1c2", " ", value = "NA"))
                                          ),
                                          tags$tr(
                                            tags$td(numericInput("r2c1", " ", value = "NA")),
                                            tags$td(numericInput("r2c2", " ", value = "NA"))
                                          )
                                        )))
                    
                  ),
                
                mainPanel(
                  plotOutput("randomPlot")
                )))

server <- function(input, output) {
  randomPlot <- hist(rnorm(100))
}

shinyApp(ui = ui, server = server)