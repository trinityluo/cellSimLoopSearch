library(shinydashboard)
library(plotly)


header <- dashboardHeader(
  title = 'Decomposition by Simulated Trajectories', titleWidth = 450
)

body <- dashboardBody(
  fluidRow(
    
    column(6,
      tableOutput('loopTable'),
      plotlyOutput('countPlot')
    ),
    column(3,
           valueBoxOutput("rr", width = NULL),
           p(class = "text-muted",
             br(),
             "RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)"
           )
    ),
    column(3,
           box(width = NULL, status = "warning",
               radioButtons('rrtype', "Type of RR",
                            choices = c('weight', 'flux'),
                            selected = 'weight'),
               selectInput("interval", "Refresh interval",
                           choices = c(
                             "2 seconds" = 2,
                             "5 seconds" = 5,
                             "30 seconds" = 30,
                             "1 minute" = 60,
                             "2 minutes" = 120,
                             "10 minutes" = 600
                           ),
                           selected = "5"
               ),
               uiOutput("timeSinceLastUpdate"),
               
               actionButton("refresh", "Refresh now")
               
               )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = T),
  body
)