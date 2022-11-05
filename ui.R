#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(waiter)
library(bslib)



# Define UI for application that draws a histogram
shinyUI(fixedPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
    
    use_waiter(),
    waiter_show_on_load(html = spin_flower()),
    
    
    


    
    # Application title
    #titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    navbarPage("Who survived the Titanic?", collapsible = TRUE,
               tabPanel("00 Start", icon = icon("play"),
                        fixedRow(
                            column(width = 6,
                                   includeMarkdown("txt/start.md"),
                                   div(style="display: inline-block; width: 150px;",
                                       actionButton("code", "Source Code", 
                                                    onclick ="window.open('https://github.com/edgar-treischl/logistic-regression-app', '_blank')", 
                                                    icon = icon("github"))),
                                   div(style="display: inline-block; width: 150px;",
                                       actionButton("download", "Download", 
                                                    onclick ="window.open('http://edgar-treischl.de/wp-content/uploads/2021/05/report.pdf')", 
                                                    icon = icon("file")))
                            ),
                            column(width = 6,
                                   tabsetPanel(type = "tabs", 
                                               tabPanel("Survival",
                                                        h4("Survival of the Titanic:"),  
                                                        plotOutput("survivedplot")),
                                               tabPanel("Sex", 
                                                        h4("Survival by Sex:"),
                                                        plotOutput("sexplot1")),
                                               tabPanel("Class",  
                                                        h4("Survival by Class:"),
                                                        plotOutput("classplot")),
                                               tabPanel("Age",  
                                                        h4("Survival by Age:"),
                                                        plotOutput("ageplot"))
                                   )
                            )
                        )
               ),
               tabPanel("01 Idea", icon = icon("lightbulb"),
                        fixedRow(
                            column(width = 6,
                                   includeMarkdown("txt/idea.md"),
                                   h4("Insert the logit in the scatterplot:"),
                                   checkboxInput("boolmethod", "Logit!", 
                                                 value = FALSE)
                            ),
                            column(width = 6,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Scatterplot", plotOutput("scatter_log")),
                                               tabPanel("Function", plotOutput("funcplot"))
                                   )
                            )
                        )
               ),
               tabPanel("02 Variables", icon = icon("chart-pie"),
                        fixedRow(
                            column(width = 6,
                                   includeMarkdown("txt/variables.md")
                            ),
                            column(width = 6,
                                   HTML(paste(h4("Add a variable:"))),
                                   div(style="display: inline-block; width: 150px;",
                                       checkboxInput("booladults", "Age", value = FALSE)),
                                   div(style="display: inline-block; width: 150px;",
                                       checkboxInput("boolclass", "Class", value = FALSE)),
                                   plotOutput("alluvialplot")
                            )
                        )
               ),
               tabPanel("03 Model", icon = icon("robot"),
                        fixedRow(
                            column(width = 5,
                                   includeMarkdown("txt/model.md")
                            ),
                            column(width = 7,
                                   h4("Logistic regression results:"),
                                   p("Pick the independent variables:"),
                                   div(style="display: inline-block; width: 100px;",
                                       checkboxInput("bool1", "Sex", value = FALSE)),
                                   div(style="display: inline-block; width: 100px;",
                                       checkboxInput("bool2", "Class", value = FALSE)),
                                   div(style="display: inline-block; width: 100px;",
                                       checkboxInput("bool3", "Age", value = FALSE)),
                                   verbatimTextOutput("model")
                            )
                        )
               ),
               tabPanel("04 Odds Ratio", icon = icon("otter"),
                        fixedRow(
                            column(width = 6,
                                   includeMarkdown("txt/odds.md"),
                            ),
                            column(width = 6,
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Barplot", plotOutput("sexplot2")),
                                               tabPanel("Calculator", verbatimTextOutput("odds"),
                                                        verbatimTextOutput("orperHand"))
                                   ),
                                   HTML(paste(h4("All the OR from the model you picked in the last pane:"))),
                                   plotOutput("modelor")
                            )
                        )
               ),
               tabPanel("05 Prediction", icon = icon("magic"),
                        fixedRow(
                            includeMarkdown("txt/prediction.md"),
                            column(width = 5,
                                   radioButtons("psex", h4("Sex"),
                                                choices = list("Male" = "male", "Female" = "female"), 
                                                selected = "female"),
                                   sliderInput("Age", 
                                               h4("Age"),
                                               min = 1,
                                               max = 100,
                                               value = 50),
                                   sliderInput("pclass", 
                                               h4("Class"),
                                               min = 1,
                                               max = 3,
                                               value = 2),
                                   div(style="display: inline-block; width: 300px;")
                            ),
                            column(width = 7,
                                   h4("The prediction result:"),
                                   plotOutput("predictionplot")
                            )
                        )
               ),
               tabPanel("06 Performance", icon = icon("hat-wizard"),
                        fixedRow(
                            column(width = 6,
                                   includeMarkdown("txt/performance.md"),
                            ),
                            column(width = 6,
                                   h4("Classification:"),
                                   plotOutput("performanceplot"),
                                   h4("ROC Plot:"),
                                   plotOutput("ROC")
                            )
                        )
               )
    )
))
