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
                    actionButton("Replicate", "Replicate!")
                  ),
                
                mainPanel(              
                  plotOutput("RandomPlot1")
                  )
                )
)



server <- function(input, output) {
  #This function will do the shuffling
  shuffle <- function(total1, total2, totyes){
    vec1<- c(rep("C1", total1),rep("C2", total2))
    samp1 <- sample(vec1, totyes, replace = FALSE)
    res1 <- table(samp1)
    prop1 <- c(res1[1]/total1, res1[2]/total2)
    diff1 <- prop1[1]-prop1[2]
    return(diff1)
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
  

  output$RandomPlot1 <- renderPlot({
    props <- replicate(n = mylist$numsamp, shuffle(mylist$cat1,mylist$cat2,mylist$cond1))
    df <- data.frame(props)
    #dotPlot(props)
    ggplot(df, aes(x=props)) +
      scale_x_continuous("Difference in Proportions",
                         limits = c(-1,1))+
      ggtitle("The Awesome Graph") +
      theme(axis.text=element_text(size=rel(2)),
            axis.title=element_text(size=rel(2.25)),
            plot.title = element_text(size = rel(3))) +
      geom_dotplot(na.rm = TRUE, fill="#F8766D", binwidth = .05, method="histodot", dotsize = .25) +
      scale_y_continuous(name="", limits = c(0, mylist$numsamp/4),breaks = NULL)
      # geom_vline(xintercept = 4.29, color = "blue", size = 2)
  })
  
  
}


shinyApp(ui = ui, server = server)