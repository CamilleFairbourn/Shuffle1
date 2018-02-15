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
                    hr(),
                    tags$div(class="header", checked = NA,
                             tags$p("Explanatory Text Here"),
                             tags$p("And Some More Stuff Here")
                             )
                  ),
                
                mainPanel(              
                  plotOutput("RandomPlot1")
                  )
                )
)



server <- function(input, output) {
  shuffle <- function(total1, total2, totyes){
    vec1<- c(rep("C1", total1),rep("C2", total2))
    samp1 <- sample(vec1, totyes, replace = FALSE)
    res1 <- table(samp1)
    prop1 <- c(res1[1]/total1, res1[2]/total2)
    diff1 <- prop1[1]-prop1[2]
    return(diff1)
  }
  props <- replicate(n = 100, shuffle(24,24,35))
  dotPlot(props)
  output$RandomPlot1 <- renderPlot({
    dotPlot(props)
    # ggplot(diff1, aes(x=average)) + 
    #   scale_x_continuous("Average Word Length in First Samples", 
    #                      limits = c(1,11), breaks=c(1:11))+
    #   ggtitle("First Samples") +
    #   theme(axis.text=element_text(size=rel(2)),
    #         axis.title=element_text(size=rel(2.25)),
    #         plot.title = element_text(size = rel(3))) +
    #   geom_dotplot(na.rm = TRUE, fill="#F8766D", binwidth = .25, method="histodot", dotsize = 1) + 
    #   coord_fixed(ratio=0.05)+
    #   scale_y_continuous(name="", limits = c(0,200), breaks = NULL) +
    #   geom_vline(xintercept = 4.29, color = "blue", size = 2)
  })
  
  
}


shinyApp(ui = ui, server = server)