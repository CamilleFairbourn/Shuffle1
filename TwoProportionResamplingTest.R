library(shiny)
library(ggplot2)
library(dplyr)
library(BHH2)
library(gridExtra)
library(rhandsontable)


ui <- fluidPage(titlePanel("Two Proportion Resampling Test"),
                sidebarLayout(
                  sidebarPanel(
                    tabsetPanel(
                      tabPanel("Shuffle",
                    tags$div(class="header", checked = NA,
                             tags$p("Explanatory Text Here"),
                             tags$p("And Some More Stuff Here")
                    ),
                    hr(),
                    switchInput("plot", value = TRUE, onLabel = "Dotplot", offLabel = "Histogram"),
                    selectInput("presets", "Presets", c("Gender Discrimination",
                                                        "Opportunity Cost",
                                                        "Avandia")),
                    textInput("colnames", "Enter Column Names (separated by comma)",
                              value = "Pro, No Pro", placeholder = "Pro, No Pro"),
                    textInput("rownames", "Enter Row Names (separated by comma)",
                              value = "Male, Female", placeholder = "Male, Female"),
                    rHandsontableOutput("hot"),
                    actionButton("Reset", "Reset"),
                    numericInput("numsamp", "Shuffle how many times?", value = 100, min = 1),
                    actionButton("Replicate", "Shuffle")
                  ),
                  tabPanel("Instructions", "Some Instructions")
                  )),
                  
                  mainPanel(              
                    plotOutput("RandomPlot1"),
                    checkboxInput("Show.Observed", "Show observed difference", FALSE),
                    textOutput("Observed.Diff"),
                    fluidRow(
                    column(textOutput("count.samples"), width = 3),
                    column(selectInput("inequality", NULL, c("greater than", "less than")), width = 3),
                    column(textInput("cutoff", NULL), width = 4),
                    textOutput("counts"))
                  )
                )
)



server <- function(input, output) {
  values <- reactiveValues()
  values$props <- vector()
  values$table.names <- c("Pro", "No Pro", "Male", "Female")
  
  #This function will do the shuffling
  shuffle <- function(total1, total2, totyes){
    vec1<- c(rep("C1", total1),rep("C2", total2))
    samp1 <- sample(vec1, totyes, replace = FALSE)
    res1 <- table(factor(samp1, levels = c("C1", "C2")))
    prop1 <- c(res1[1]/total1, res1[2]/total2)
    diff1 <- prop1[1]-prop1[2]
    return(diff1)
  }
  
  #determines the number of decimal places of a number
  decimalcount<-function(x){stopifnot(class(x)=="character")
    x<-gsub("(.*)(\\.)|([0]*$)","",x)
    as.numeric(nchar(x))
  }
 
  #create dotplot locations from data x
  dotplot_locs <- function(x, n){
    counts <- table(x)
    x.locs <- as.numeric(names(counts))
    if (length(names(counts)) == 1){
      point_dist <- min(diff(c(0, as.numeric(names(counts)))))/(n+2)
    } else {
      point_dist <- min(diff(as.numeric(names(counts))))/(n+2)
    }

    x.coord <- sapply(x.locs, function(x) x + ((1:n)-(n+1)/2)*point_dist)
    
    x.coords <- vector()
    y.coords <- vector()
    to.red <- vector()
    names.counts <- as.numeric(names(counts))
    for (i in 1:length(counts)){
      if (n == 1){
        x.coords <- c(x.coords, rep(x.coord[i], counts[i]/n))
      } else {
        x.coords <- c(x.coords, rep(x.coord[, i], counts[i]/n), x.coord[0:(counts[i] %% n), i])
      }
      
      if (counts[i] > n){
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/n), n)),
                      rep(ceiling(counts[i]/n), counts[i] %% n))
      } else {
        y.coords <- c(y.coords, sort(rep(1:(counts[i]/n), counts[i])))
      }
      if(!is.na(as.numeric(input$cutoff))){
        num.decimals <- decimalcount(as.character(input$cutoff))
        error <- ifelse(num.decimals == 0, 0, 0.1^num.decimals/2)
        if (input$inequality == "greater than"){
          if (names.counts[i] >= as.numeric(input$cutoff)-error){
            to.red <- c(to.red, rep("red", counts[i]))
          } else {
            to.red <- c(to.red, rep("black", counts[i]))
          }
        } else {
          if (names.counts[i] <= as.numeric(input$cutoff)+error){
            to.red <- c(to.red, rep("red", counts[i]))
          } else {
            to.red <- c(to.red, rep("black", counts[i]))
          }
        }
      } else {
        to.red <- c(to.red, rep("black", counts[i]))
      }
    }
    return(data.frame("x" = x.coords, "y" = y.coords*n, "red" = to.red))
  }
  
  
  #set up starting values for the app
  mylist <- reactiveValues(cat1 = NA, cat2 = NA,
                           cond1 = NA, numsamp = 1)
  
  #these will update each time the user clicks the Replicate button
  observeEvent(input$Replicate || input$Show.Observed, {
    mylist$cat1 = values[['DF']][1, 3]
    mylist$cat2 = values[['DF']][2, 3]
    mylist$cond1 = values[['DF']][3, 1]
    mylist$numsamp = input$numsamp
    mylist$observed = values[['DF']][1 ,1]/values[['DF']][1, 3] - values[['DF']][2 ,1]/values[['DF']][2, 3]
  })
  
  
  observeEvent(c(input$Reset, input$hot), {
    values$props <- vector()
  })
  
  update_vals <- eventReactive(input$Replicate, {
    values$props <- c(values$props, 
                      replicate(n = mylist$numsamp, shuffle(mylist$cat1,mylist$cat2,mylist$cond1)))
  })
  
  update_counts <- eventReactive(c(input$cutoff, input$Replicate, input$Reset, input$inequality), {
    if (!is.na(as.numeric(input$cutoff))){
      num.decimals <- decimalcount(as.character(input$cutoff))
      error <- ifelse(num.decimals == 0, 0, 0.1^num.decimals/2)
      if (input$inequality == "greater than"){
      values$prob <- sum(values$props >= as.numeric(input$cutoff)-error)/length(values$props)
      values$count <- sum(values$props >= as.numeric(input$cutoff)-error)
      } else {
        values$prob <- sum(values$props <= as.numeric(input$cutoff)+error)/length(values$props)
        values$count <- sum(values$props <= as.numeric(input$cutoff)+error)
      }
    }
  })
  
  output$RandomPlot1 <- renderPlot({
    update_vals()
    if (length(values$props) != 0){ # after reset, values$props is empty
      DF <- values[['DF']]
      possible_x <- max(0, DF[3, 1]-DF[2, 3]):min(DF[3, 1],DF[1, 3])/DF[1, 3]-
        min(DF[3, 1],DF[1, 3]):max(0, DF[3, 1]-DF[2, 3])/DF[1, 3]
      if (input$plot == TRUE){
        if (DF[3, 1] > 1000){
          n <- 1
        } else {
          n <- 4
        }
        df <- dotplot_locs(values$props, n)
        myplot <- ggplot(df)  +
          geom_point(aes(x ,y), size=min(n, 50/length(values$props)^0.5)) + 
          theme(legend.position="none")
        if (!is.na(as.numeric(input$cutoff))){
          myplot <- ggplot(df)  +
            geom_point(aes(x ,y, colour = red), size=min(n, 50/length(values$props)^0.5)) +
            scale_colour_manual(name = "red",values = c("black", "red")) + 
            theme(legend.position="none")
          myplot <- myplot + geom_vline(xintercept = as.numeric(input$cutoff), color = "red")
        }
        myplot + scale_y_continuous(limits = c(0, max(n*7.5,max(df$y)))) +
          scale_x_continuous(limits = c(-max(abs(max(df$x)), abs(min(df$x))),
                                        max(abs(max(df$x)), abs(min(df$x))))) #breaks = round(possible_x,3)
      } else {
        df <- data.frame("x" = values$props)
        myplot <- ggplot(df, aes(x=x)) + geom_histogram() 
        if (!is.na(as.numeric(input$cutoff))){
          names.counts <- ggplot_build(myplot)$data[[1]]$x
          num.decimals <- decimalcount(as.character(input$cutoff))
          error <- ifelse(num.decimals == 0, 0, 0.1^num.decimals/2)
          if (input$inequality == "greater than"){
            to.red <- which(names.counts >= as.numeric(input$cutoff)-error)
            red <- rep("black", length(names.counts))
            red[to.red] <- "red"
          } else {
            to.red <- which(names.counts <= as.numeric(input$cutoff)+error)
            red <- rep("black", length(names.counts))
            red[to.red] <- "red"
          }
          myplot <- ggplot(df, aes(x=x)) + geom_histogram(fill = red) + geom_vline(xintercept = as.numeric(input$cutoff), color = "red")
        }
      }
      myplot
    }
  })
  
  
  
  output$Observed.Diff <- renderText({
    if (input$Show.Observed){
      DF <- data()
      mylist$observed = values[['DF']][1 ,1]/values[['DF']][1, 3] - values[['DF']][2 ,1]/values[['DF']][2, 3]
        paste("Observed Difference:", round(mylist$observed, 3))
    }
  })
  
  output$count.samples <- renderText({
    "Count Samples"
  })
  
  output$counts <- renderText({
    update_counts()
    if (!is.null(values$prob)){
      if (is.na(values$prob)){
        " "
      } else if (!is.na(as.numeric(input$cutoff))){
        paste(values$count, "/", length(values$props), " (", round(values$prob, 4), ")", sep = "")
      } else if (nchar(input$cutoff)!=0){
        "Invalid Cutoff!"
      } else {
        " "
      }
    }
  })
  
  
  observeEvent(input$presets, {
    if (input$presets == "Gender Discrimination"){
      DF <- data.frame("X1" = c(21, 14, 35), "X2" = c(3, 10, 13))
      DF[3, ] <- apply(DF[-3, ], 2, sum)
      DF[, 3] <- apply(DF[, -3], 1, sum)
      values[["DF"]] <- DF
      values$table.names <- c("Pro", "No Pro", "Male", "Female")
    } else if (input$presets == "Opportunity Cost"){
      DF <- data.frame("X1" = c(46, 51, 97), "X2" = c(29, 24, 53))
      DF[3, ] <- apply(DF[-3, ], 2, sum)
      DF[, 3] <- apply(DF[, -3], 1, sum)
      values[["DF"]] <- DF
      values$table.names <- c("buy DVD", "not buy DVD", "control", "treatment")
    } else if (input$presets == "Avandia"){
      DF = data.frame("X1" = c(2593, 5386, 7979), "X2" = c(65000, 154592, 219592))
      DF[3, ] <- apply(DF[-3, ], 2, sum)
      DF[, 3] <- apply(DF[, -3], 1, sum)
      values[["DF"]] <- DF
      values$table.names <- c("Yes", "No", "Rosiglitazone", "Pioglitazone")
    }
  })
  
  observeEvent(c(input$colnames, input$rownames), {
    values$table.names <- c(unlist(strsplit(input$colnames, ",")), unlist(strsplit(input$rownames, ",")))
  })
  
  
  observeEvent(input$hot, {
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
        DF = values[["DF"]]
    }
    DF[3, ] <- apply(DF[-3, ], 2, sum)
    DF[, 3] <- apply(DF[, -3], 1, sum)
    values[["DF"]] = DF
  })
  
  output$hot <- renderRHandsontable({
    DF = values[['DF']]
    if (!is.null(DF)){
      rhandsontable(DF, colHeaders = c(values$table.names[1:2], "Total"),
                    rowHeaders = c(values$table.names[3:4], "Total"))
    }
  })
  
  
}


shinyApp(ui = ui, server = server, options = list(height = 1080))