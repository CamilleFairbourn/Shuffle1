library(shiny)
library(ggplot2)
library(gridExtra)
library(rhandsontable)

ui <- fluidRow(
  textInput("colnames", "Enter Column Names (separated by comma)",
            value = "A, B", placeholder = "A, B"),
  textInput("rownames", "Enter Row Names (separated by comma)",
            value = "C, D", placeholder = "C, D"),
  rHandsontableOutput("hot"),
  rHandsontableOutput("prob")
)

server <- shinyServer(function(input, output, session) {
  values = reactiveValues()
  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]])){
        DF = data.frame("X1" = c(0, 0, 0), "X2" = c(0, 0, 0))
        
      }
      else{
        DF = values[["DF"]]
      }
    }
    DF[3, ] <- apply(DF[-3, ], 2, sum)
    DF[, 3] <- apply(DF[, -3], 1, sum)
    values[["DF"]] = DF
    DF
  })
  

  
  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      
      rhandsontable(DF, colHeaders = c(unlist(strsplit(input$colnames, ",")), "Total"),
                    rowHeaders = c(unlist(strsplit(input$rownames, ",")), "Total"))
  })
  
  output$prob <- renderRHandsontable({
    DF.prob = data()
    DF.prob <- DF.prob/DF.prob[3, 3]
    if (!is.null(DF.prob))
      
      rhandsontable(DF.prob, colHeaders = paste("P(",c(unlist(strsplit(input$colnames, ",")),
                                                       "Total"),")", sep = ""),
                    rowHeaders = paste("P(",c(unlist(strsplit(input$rownames, ",")),
                                              "Total"),")", sep = ""),
                    readOnly = TRUE)
  })
})


shinyApp(ui = ui, server = server)