
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(shinythemes)
library(htmltools)
library(tidyr)
library(dplyr)
library(DT)

shinyUI(
  fluidPage(theme = shinytheme('readable'),
    fluidRow(
      column(3,
        wellPanel(
          sliderInput('alpha', 'Input Alpha', 
                      min = 0.1, max = 9, value = 7, step = 0.1),
          sliderInput('beta', 'Input Beta',
                       min = 0.1, max = 9, value = 5, step = 0.1),
          sliderInput('gamma', 'Input Gamma',
                       min = 1, max = 90, value = 60, step = 10, post = '%'),
          radioButtons('rrtype', "Type of RR",
                        choices = c('weight', 'flux'),
                        selected = 'weight'),

           p('Formulas:'),
           p('RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)'),
           p('Weight = count of one loop / total counts of loops')
          

          
        )
      ),
      column(9,
        tabsetPanel(id = 'tabs',
          tabPanel('Single Parameter',
            fluidRow(
              column(3,
                     valueBoxOutput('rr')
              ),
              column(3,
                     valueBoxOutput('weight', width = 'auto')
              ),
              column(3, 
                     valueBoxOutput('simSteps', width = 'auto')
              ),
              column(3, 
                     valueBoxOutput('numLoops', width = 'auto')
              )     
            ),
            fluidRow(
              tableOutput('rrInfoTable')
            ),
            fluidRow(
              div(style = 'font-size:150%;text-align:center',
                  textOutput('paramInfoText')
              ),
              div(style = 'text-align:center',
                  tableOutput('loopResultsTable'),
                  plotlyOutput('bubblePlot'),
                  plotlyOutput('surface')
              )
            )
            
          ),
          tabPanel('Multi-Parameters'
                 
          )
        )
      )
    )
  )
)


















# library(shinydashboard)
# library(plotly)
# 
# 
# header <- dashboardHeader(
#   title = 'Decomposition by Simulated Trajectories', titleWidth = 450
# )
# 
# body <- dashboardBody(
#   # row 1 - value box and parameter selection
#   fluidRow(
#     column(3,
#            valueBoxOutput("rr", width = NULL),
#            p(class = "text-muted",
#              br(),
#              "RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)"
#            )
#     ),
#     column(3,
#            valueBoxOutput("rr", width = NULL),
#            p(class = "text-muted",
#              br(),
#              "RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)"
#            )
#     ),
#     column(3,
#            valueBoxOutput("rr", width = NULL),
#            p(class = "text-muted",
#              br(),
#              "RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)"
#            )
#     ),
#     column(3,
#            valueBoxOutput("rr", width = NULL),
#            p(class = "text-muted",
#              br(),
#              "RR = abs(average(-log(flux)) - min(-log(flux)))/std(flux)"
#            )
#     )
#   ),
#   fluidRow(
#     
#     column(6,
#       tableOutput('loopTable'),
#       verbatimTextOutput('testText')
#       
#     ),
#     column(3
#            
#     ),
#     column(3,
#            box(width = NULL, status = "warning",
#                sliderInput('alpha', 'Input Alpha', 
#                            min = 0.1, max = 9, value = 7, step = 0.1),
#                sliderInput('beta', 'Input Beta', 
#                            min = 0.1, max = 9, value = 5, step = 0.1),
#                sliderInput('gamma', 'Input Gamma', 
#                            min = 1, max = 90, value = 60, step = 10, post = '%'),
#                radioButtons('rrtype', "Type of RR",
#                             choices = c('weight', 'flux'),
#                             selected = 'weight'),
#                selectInput("interval", "Refresh interval",
#                            choices = c(
#                              "2 seconds" = 2,
#                              "5 seconds" = 5,
#                              "30 seconds" = 30,
#                              "1 minute" = 60,
#                              "2 minutes" = 120,
#                              "10 minutes" = 600
#                            ),
#                            selected = "5"
#                ),
#                uiOutput("timeSinceLastUpdate"),
#                
#                actionButton("refresh", "Refresh now"),
#                verbatimTextOutput('testText')
#                
#                )
#     )
#   )
# )
# 
# dashboardPage(
#   header,
#   dashboardSidebar(disable = T),
#   body
# )