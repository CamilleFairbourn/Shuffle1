library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(titlePanel("Making a Contingency Table"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("RowName", "Name your row variable"),
                    textInput("ColName", "Name your column variable")
                    numericInput("r1c1", "Observations in Row 1, Column 1:", value = 0, min = 0),
                    numericInput("r1c2", "Observations in Row 1, Column 2:", value = 0, min = 0),
                    numericInput("r2c1", "Observations in Row 2, Column 1:", value = 0, min = 0),
                    numericInput("r2c2", "Observations in Row 2, Column 2:", value = 0, min = 0),
                    numericInput("r1t", "Total observations for Row 1:", value = 0, min = 0),
                    numericInput("r2t", "Total observations for Row 2:", value = 0, min = 0),
                    numericInput("c1t", "Total observations for Column 1:", value = 0, min = 0),
                    numericInput("c2t", "Total observations for Column 2:", value = 0, min = 0),
                    numericInput("numsamp", "Shuffle how many times?", value = 100, min = 1),
                    hr(),
                    tags$div(class="header", checked = NA,
                             tags$p("Explanatory Text Here"),
                             tags$p("And Some More Stuff Here")
                    ),
                    actionButton("UseData", "Use This Data")
                  ),
                  
                  mainPanel(              
                    plotOutput("tableplot")
                  )
                )
  
)




server <- function(input, output) {
  mytable <- reactiveValues(r1c1 = 0, r1c2 = 0, r2c1 = 0, r2c2 = 0, r1t = 0, r2t = 0, 
                            c1t = 0, c2t = 0, reps = 0)
  
  #these will update each time the user clicks the Replicate button
  observeEvent(input$UseData, {
    mytable$r1c1 = input$r1c1
    mytable$r1c2 = input$r1c2
    mytable$r2c1 = input$r2c1
    mytable$r2c2 = input$r2c2
    mytable$r1t = input$r1t
    mytable$r2t = input$r2t
    mytable$c1t = input$c1t
    mytable$c2t = input$c2t
    mytable$reps = input$numsamp
  })
  tableplot <- qplot(1:10, 1:10, geom = "blank") + theme(line = element_blank(), text = element_blank()) +
    annotation_custom(grob = tableGrob(mytable))
  
}