library(shiny)
library(bslib)

source("R/utils.R")

ui <- shinyUI(fixedPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  # Sidebar with a slider input for number of bins 
  navbarPage("Who survived the Titanic?", collapsible = TRUE,
             tabPanel("00 Start", icon = icon("play"),
                      fixedRow(
                        column(width = 6,
                               includeMarkdown("./txt/start.md"),
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
                               includeMarkdown("./txt/idea.md"),
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
                               includeMarkdown("./txt/variables.md")
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
                               includeMarkdown("./txt/odds.md"),
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
                        includeMarkdown("./txt/prediction.md"),
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
                               includeMarkdown("./txt/performance.md"),
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






server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  # -- Survival Plots --
  output$survivedplot <- renderPlot({
    ggplot(train_df, aes(x = Survived)) +
      geom_bar(aes(y = (..count..) / sum(..count..)), fill = c("#999999")) +
      ylab("Percent") +
      theme_bw(base_size = 18)
  })
  
  output$sexplot1 <- renderPlot({
    ggplot(train_df, aes(x = Sex, fill = Survived)) +
      geom_bar(aes(y = (..count..) / sum(..count..))) +
      ylab("Percent") +
      scale_fill_manual(values = c("#009E73", "#E69F00")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom")
  })
  
  output$ageplot <- renderPlot({
    ggplot(train_df, aes(x = Age, fill = Survived)) +
      geom_histogram() +
      ylab("") +
      scale_fill_manual(values = c("#009E73", "#E69F00")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom")
  })
  
  output$classplot <- renderPlot({
    ggplot(train_df, aes(x = Pclass, fill = Survived)) +
      geom_bar(aes(y = (..count..) / sum(..count..))) +
      ylab("Percent") +
      scale_fill_manual(values = c("#009E73", "#E69F00")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom")
  })
  
  # -- Logit vs Linear Regression --
  output$scatter_log <- renderPlot({
    set.seed(2)
    y <- rep(c(0, 1), 500)
    x <- 10 + rnorm(250, 3, 3) + rnorm(250, 10, 3) * y
    data <- tibble::tibble(x, y)
    
    gg <- ggplot(data, aes(x = x, y = y)) +
      geom_point(color = "gray") +
      theme_bw(base_size = 18)
    
    if (input$boolmethod) {
      gg + geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "#008080")
    } else {
      gg + geom_smooth(method = "lm", formula = y ~ x, color = "#008080")
    }
  })
  
  output$funcplot <- renderPlot({
    sigmoid <- function(x) 1 / (1 + exp(-x))
    probit <- function(x) pnorm(x)
    
    ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
      stat_function(fun = sigmoid, color = "#008080", size = 1) +
      stat_function(fun = probit, color = "#2C3E50", size = 1) +
      annotate("text", x = 3, y = 0.85, label = "Logit", colour = "#008080", size = 6) +
      annotate("text", x = 0.85, y = 0.95, label = "Probit", colour = "#2C3E50", size = 6) +
      theme_bw(base_size = 18)
  })
  
  # -- Alluvial Plot --
  output$alluvialplot <- renderPlot({
    thematic::thematic_off()
    if (input$booladults && !input$boolclass) {
      all_adults
    } else if (!input$booladults && input$boolclass) {
      all_class
    } else if (input$booladults && input$boolclass) {
      all_overall
    } else {
      all_sex
    }
  })
  
  # -- Logistic Model Summary --
  output$model <- renderPrint({
    if (input$bool1 && !input$bool2 && !input$bool3) {
      summary(model_call("m1"))
    } else if (!input$bool1 && input$bool2 && !input$bool3) {
      summary(model_call("m2"))
    } else if (!input$bool1 && !input$bool2 && input$bool3) {
      summary(model_call("m3"))
    } else if (input$bool1 && input$bool2 && !input$bool3) {
      summary(model_call("m4"))
    } else if (input$bool1 && input$bool2 && input$bool3) {
      summary(model_call("m5"))
    } else if (input$bool1 && !input$bool2 && input$bool3) {
      summary(model_call("m6"))
    } else if (!input$bool1 && input$bool2 && input$bool3) {
      summary(model_call("m7"))
    } else {
      print("Pick at least one independent variable, my friend :-)")
    }
  })
  
  # -- Odds Ratio Plots --
  output$sexplot2 <- renderPlot({
    ggplot(train_df, aes(x = Sex, fill = Survived)) +
      geom_bar() +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1, position = position_stack(vjust = 0), size = 6) +
      ylab("Count") +
      scale_fill_manual(values = c("#009E73", "#E69F00")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom")
  })
  
  output$odds <- renderText({
    "Men's odds: 109/469:  0.232906 \nWomen's odds: 233/81: 2.876543 \nMen's odds ratio to survive: 0.232906/2.876543"
  })
  
  output$modelor <- renderPlot({
    thematic::thematic_off()
    if (input$bool1 && !input$bool2 && !input$bool3) {
      or1
    } else if (!input$bool1 && input$bool2 && !input$bool3) {
      or2
    } else if (!input$bool1 && !input$bool2 && input$bool3) {
      or3
    } else if (input$bool1 && input$bool2 && !input$bool3) {
      or4
    } else if (input$bool1 && input$bool2 && input$bool3) {
      or5
    } else if (input$bool1 && !input$bool2 && input$bool3) {
      or6
    } else if (!input$bool1 && input$bool2 && input$bool3) {
      or7
    } else {
      print("Still, you have to pick at least one independent variable, my friend :-)")
    }
  })
  
  
  
  # -- Prediction Plot --
  output$predictionplot <- renderPlot({
    pred_df <- data.frame(
      Sex = factor(input$psex, levels = c("male", "female")),
      Age = input$Age,
      Pclass = factor(input$pclass, levels = c(1, 2, 3),
                      labels = c("First class", "Second class", "Third class"))
    )
    
    pred_df$total <- 100
    pred_df$prediction <- round(predict(glm_fit, newdata = pred_df, type = 'response'), 3) * 100
    
    thematic::thematic_off()
    
    ggplot(pred_df, aes(x = Age, y = prediction)) +
      geom_col(fill = "#E69F00") +
      geom_col(aes(y = total), alpha = 0.01, colour = "gray") +
      geom_text(aes(label = paste0(prediction, "%")),
                hjust = 0, color = "#2C3E50", size = 8) +
      coord_flip() +
      theme_minimal(base_size = 18) +
      ylab("Predicted probability of survival") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  
  
  # -- Performance Plot --
  output$performanceplot <- renderPlot({
    thematic::thematic_off()
    pred_plot
  })
  
  # -- ROC Curve --
  output$ROC <- renderPlot({
    thematic::thematic_off()
    roc_curve
  })
}








shinyApp(ui = ui, server = server)


