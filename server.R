#NAME: server.R
#FUNC: Display RMarkdown documents and allow upload
#USES: ui.R, server.R
#VER.: 1.0
#HIST: XL initial 02/27/2017


library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)


source('R/functions.R')
source("runAll.R")




shinyServer(function(input, output, session) {
  
  # observe if any parameters changed, if so, re-create results.Rds
  observe({
    
    input$alpha
    input$beta
    input$gamma
    unlink('data/processed/results.Rds')
    # initialize results.Rds
    results <- data.table(count = numeric(), loops = character(), steps = numeric(),
                          flux = numeric(), weight = numeric())
    saveRDS(results, 'data/processed/results.Rds')
  })
  
  #run simulation
  simRuner <- observe({

    # run simulation based on input parameters
    results <- readRDS('data/processed/results.Rds')
    results <- DecomposeTrj(input$alpha, input$beta, input$gamma/100, matrixA, 
                            1024, results) %>% 
      arrange(desc(count))
    saveRDS(results, 'data/processed/results.Rds')
    invalidateLater(100, session)
    })
  
    # When the client ends the session, suspend the observer and
    session$onSessionEnded(function() {
      simRuner$suspend()
      unlink('data/processed/results.Rds')
    })
  
  # every 2secs, re-read the results.Rds
  fileReaderData <- reactiveFileReader(2000, session,
                                     'data/processed/results.Rds', readRDS)
  
  # output parameters value to text
  output$paramInfoText <- renderText({
    paste('Alpha=', input$alpha, ', Beta=', input$beta, ', Gamma=', input$gamma, '%')
  })
  
  # output rr calculation
  output$rrInfoTable <- renderTable({
    df <- fileReaderData()
    value <- df[, input$rrtype]
    logx <- -log(value) %>% round(2)
    avg <- mean(logx) %>% round(2)
    minValue <- min(logx) %>% round(2)
    std <- sd(logx) %>% round(2)
    rr <- abs(mean(logx) - min(logx))/sd(logx)
    rr <- round(rr, 2)
    
    formula <- c('rr', '=', 'average', '-', 'minimum', '/', 'standard deviation')
    value <- c(rr, '=', avg, '-', minValue, '/', std)
    
    outputDf <- as.data.frame(rbind(value)) 
    colnames(outputDf) <- formula
    
    return(outputDf)
  })
  
  # output loop table
  output$loopResultsTable <- renderTable({
    df <- fileReaderData()
    head(df)
  })
  
  # output RR
  output$rr <- renderValueBox({
    df <- fileReaderData()
    rrValue <- df[, input$rrtype] %>% RobustRatio()
    valueBox(
      round(rrValue, 2), "RR")
  })
  
  # output bio-loop weight
  output$weight <- renderValueBox({
    df <- fileReaderData()
    weightValue <- df %>% 
      filter(loops == '4-132-130-138-154-19-101-100-612-356') %>% 
      select(weight)
    weightValue <- paste(round(weightValue, 2) * 100, '%')  
    valueBox(
      weightValue, "Bio-Loop Weight")
  })
  
  # output sim steps
  output$simSteps <- renderValueBox({
    df <- fileReaderData()
    stepsValue <- df[, 'steps'] %>% sum()
    valueBox(
      stepsValue, "Total Sim Steps")
  })
  # output number of loops
  output$numLoops <- renderValueBox({
    df <- fileReaderData()
    numOfLoops <- nrow(df)
    valueBox(
      numOfLoops, "Num of Unique Loops")
  })
  
  # output bubble chart
  output$bubblePlot <- renderPlotly({
    data <- fileReaderData()
      
    pMargin <- list(b = 80, l = 80, r = 80, t = 90, pad = 4)
    # yy <- as.formula(paste("~", input$selectedGroupby))
    # sizeValue <- sapply(data$opportunity, function(value){
    #   if (value > 0) {3*sqrt(value)}#modify the size
    #   else {value <- 0}
    # })
    p <- plot_ly(data, x = data$weight, y = data$steps, name = 'bubble', type = 'scatter',
                 hoverinfo = "text", source = 'bubbleclick',
                 text = paste("Loop Detail:", data$loop,
                              "<br>Loop Appears", data$count),
                 mode = "markers", marker = list(color = data$count, size = data$weight * 500, colorscale = 'Portland',
                                                 colorbar = list(title = 'Count'))) %>%
      layout(
        title = paste('Loop Bubble Chart'),
        xaxis = list(title =  "Weight of Loops"),
        yaxis = list(title = 'States of Loops', zeroline = T),
        margin = list(t = 80, b = 80)
      ) %>%
      # add_annotations(text = "(Click to view detail information below)", 
      #                 xref = 'paper', yref = 'paper', x = 0.5, y = -0.3, showarrow = F) %>%
      add_annotations(text = paste("Size represent the Weight and color represent the Count"),
                      xref = 'paper', yref = 'paper', x = 0.5, y = 1.1, showarrow = F) %>%
      config(showLink = F, displayModeBar = F, displaylogo = F)
    return(p)
  })
  
  # 3d surface
  output$surface <- renderPlotly({
    df <- fileReaderData()
    dataLength <- ceiling(sqrt(nrow(df)))
    xVec <- c(seq(1, dataLength))
    xMatrix <- matrix(xVec, nrow = dataLength, ncol = 1)
    yMatrix <- matrix(xVec, nrow = 1, ncol = dataLength)
    
    diff <- dataLength^2 - nrow(df)
    loopMatrix <- matrix(c(sample(df$count), rep(0, diff)), nrow = dataLength, ncol = dataLength)
    
    p <- plot_ly(z = loopMatrix) %>% add_surface() %>% 
      config(showLink = F, displayModeBar = F, displaylogo = F)
    
    return(p)
  })
  
  # change parameters
  output$multiParamPlot <- renderPlotly({
    data <- fileReaderData()
    
    pMargin <- list(b = 80, l = 80, r = 80, t = 90, pad = 4)
    # yy <- as.formula(paste("~", input$selectedGroupby))
    # sizeValue <- sapply(data$opportunity, function(value){
    #   if (value > 0) {3*sqrt(value)}#modify the size
    #   else {value <- 0}
    # })
    p <- plot_ly(data, x = data$weight, y = data$steps, name = 'bubble', type = 'scatter',
                 hoverinfo = "text", source = 'bubbleclick',
                 text = paste("Loop Detail:", data$loop,
                              "<br>Loop Appears", data$count),
                 mode = "markers", marker = list(color = data$count, size = data$weight * 500, colorscale = 'Portland',
                                                 colorbar = list(title = 'Count'))) %>%
      layout(
        title = paste('Loop Bubble Chart'),
        xaxis = list(title =  "Weight of Loops"),
        yaxis = list(title = 'States of Loops', zeroline = T),
        margin = list(t = 80, b = 80)
      ) %>%
      # add_annotations(text = "(Click to view detail information below)", 
      #                 xref = 'paper', yref = 'paper', x = 0.5, y = -0.3, showarrow = F) %>%
      add_annotations(text = paste("Size represent the Weight and color represent the Count"),
                      xref = 'paper', yref = 'paper', x = 0.5, y = 1.1, showarrow = F) %>%
      config(showLink = F, displayModeBar = F, displaylogo = F)
    return(p)
  })
  
  
  
  
})






