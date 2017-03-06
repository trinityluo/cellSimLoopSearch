#NAME: server.R
#FUNC: Display RMarkdown documents and allow upload
#USES: ui.R, server.R
#VER.: 1.0
#HIST: XL initial 02/27/2017


library(shiny)
library(plotly)

source('R/functions.R')

GetLoopData <- function(path){
  loopData <- read.csv(path)
  
  return(loopData)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #
  LoopInfo <- reactive({
    input$refresh # Refresh if button clicked
    
    interval <- as.numeric(input$interval)
    
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    
    GetLoopData('data/processed/results.csv') %>% arrange(desc(count))
    
  })
  
  # Get time that vehicles locations were updated
  lastUpdateTime <- reactive({
    LoopInfo() # Trigger this reactive when vehicles locations are updated
    Sys.time()
  }) 
  
  # Number of seconds since last update
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(1000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })
  
  #
  output$loopTable <- renderTable({
    df <- LoopInfo() 
    df$flux <- as.character(format(df$flux, scientific = T, digits = 3))
    
    return(head(df, 10))
  })
  
  output$rr <- renderValueBox({
    df <- LoopInfo()
    rrValue <- df[, input$rrtype] %>% RobustRatio()
    valueBox(
      round(rrValue, 2), "RR", icon = icon("list"), color = "light-blue" 
    )
  })
  
  # output$rrPlot <- renderPlotly({
  #   df <- LoopInfo()
  #   rrValue <- df[, input$rrtype] %>% RobustRatio()
  #   
  #   now <- as.POSIXlt(Sys.time())
  #   tm <- seq(0, 600, length.out = nrow())
  #   
  #   rrdf <- data.frame(x = now - tm)
  #   
  #   x <- now - tm
  #   y <- rrValue <- df[, input$rrtype] %>% RobustRatio()
  #   p <- plot_ly(df, x ~x, y = )
  # })
  
  # output$countPlot <- renderPlotly({
  #   df <- LoopInfo()
  #   df <- head(df, 10)
  #   p <- plot_ly(df, x = loop, y = count, type = 'bar')
  #   
  #   p
  # })
  
  
})
