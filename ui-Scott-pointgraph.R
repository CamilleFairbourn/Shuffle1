library(shiny)
library(ggplot2)
library(dplyr)
library(BHH2)


ui <- fluidPage(titlePanel("Two Proportion Resampling Test"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("cat1", "Total observations in Category 1:", value = 24, min = 0),
                    numericInput("cat2", "Total observations in Category 2:", value = 24, min = 0),
                    numericInput("cond1", "Total observations for Condition 1:", value = 35, min = 0),
                    numericInput("numsamp", "Shuffle how many times?", value = 100, min = 1),
                    hr(),
                    tags$div(class="header", checked = NA,
                             tags$p("Explanatory Text Here"),
                             tags$p("And Some More Stuff Here")
                    ),
                    actionButton("Replicate", "Replicate!"),
                    actionButton("Reset", "Reset")
                  ),
                  
                  mainPanel(              
                    plotOutput("RandomPlot1")
                  )
                )
)



server <- function(input, output) {
  values <- reactiveValues()
  values$props <- vector()
  
  #This function will do the shuffling
  shuffle <- function(total1, total2, totyes){
    vec1<- c(rep("C1", total1),rep("C2", total2))
    samp1 <- sample(vec1, totyes, replace = FALSE)
    res1 <- table(samp1)
    prop1 <- c(res1[1]/total1, res1[2]/total2)
    diff1 <- prop1[1]-prop1[2]
    return(diff1)
  }
  #create dotplot locations from data x
  dotplot_locs <- function(x){
    counts <- table(x)
    x.locs <- as.numeric(names(counts))
    point_dist <- min(diff(as.numeric(names(counts))))/6
    
    x.coord <- sapply(x.locs, function(x) x + ((1:4)-2.5)*point_dist)
    
    x.coords <- vector()
    y.coords <- vector()
    for (i in 1:length(counts)){
      x.coords <- c(x.coords, rep(x.coord[, i], counts[i]/4), x.coord[0:(counts[i] %% 4), i])
      if (counts[i] > 4){
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/4), 4)),
                      rep(ceiling(counts[i]/4), counts[i] %% 4))
      } else {
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/4), counts[i])))
      }
    }
    return(data.frame("x" = x.coords, "y" = y.coords*4))
  }
  
  #set up starting values for the app
  mylist <- reactiveValues(cat1 = 24, cat2 = 24, cond1 = 35, numsamp = 1)
  
  #these will update each time the user clicks the Replicate button
  observeEvent(input$Replicate, {
    mylist$cat1 = input$cat1
    mylist$cat2 = input$cat2
    mylist$cond1 = input$cond1
    mylist$numsamp = input$numsamp
  })
  
  observeEvent(input$Reset, {
    values$props <- vector()
  })
  
  update_vals <- eventReactive(input$Replicate, {
    values$props <- c(values$props, 
                      replicate(n = mylist$numsamp, shuffle(mylist$cat1,mylist$cat2,mylist$cond1)))
  })
  
  
  output$RandomPlot1 <- renderPlot({
    #props <- c(props, update_vals())
    update_vals()
    
    df <- dotplot_locs(values$props)
    ggplot(df)  +
      geom_point(aes(x ,y), size=125/length(values$props))
    
  })
  
  
}


shinyApp(ui = ui, server = server)