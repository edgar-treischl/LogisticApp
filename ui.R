#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggbeeswarm)
library(viridis)
library(broom)
library(ggalluvial)


# Define UI for application that draws a histogram
shinyUI(fixedPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),

    # Application title
    titlePanel("Who survived the Titanic?"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            radioButtons("variable", h5("How many people survived the Titanic? Who had a higher chance to survive, men or women?
                                               What about class and age?"),
                         choices = list("How many survived?" = 0, "Survival by sex" = 1, "Survival by class" = 2,
                                        "Survival by age" = 3), selected = 0),
            plotOutput("survivedplot"),
            br(),
            br(),
            div(style="display: inline-block; width: 150px;",
                actionButton("code", "Source Code", 
                             onclick ="window.open('https://github.com/edgartreischl/OLSinaNutshell.git', '_blank')", 
                             icon = icon("github"))),
            div(style="display: inline-block; width: 150px;",
                actionButton("download", "Download", 
                             onclick ="window.open('http://edgar-treischl.de/wp-content/uploads/2021/05/report.pdf')", 
                             icon = icon("file"))),
            
        ),

        # Mainpanel
        mainPanel(
            tabsetPanel(type = "pills",
                        tabPanel("Start", icon = icon("play"),
                                 HTML(
                                     paste(
                                         h4("This app shows you some basic aspects about logistic regression. We use passenger's sex, class,
                                            and age to estimate the effect on the survival of the Titanic accident."),'<br/>',
                                         '<center><img src="http://edgar-treischl.de/wp-content/uploads/2021/04/Titanic.jpg" width=90%/></center>'))
                        ),
                        tabPanel("Idea", icon = icon("lightbulb"),
                                 HTML(paste(h4("Logistic regression, but why?"))),
                                 HTML(paste(p("There are several reasony why logisitic regressions
                                              were invented to model binary outcomes. You can see the most obvious
                                              reason in the figure below. Imagine that we insert
                                              a regression line to model a binary outcome. Look how a scatter
                                              plot would look like in such a situation."))),
                                 plotOutput("distplot2"),
                                 HTML(paste(p("In a linear regression, we try to fit a line that minimizes the error,
                                              but in the case of a binary outcome, the observed error is not homoscedastic. 
                                              Moreover, the variance of the error term depends on the particular value of X, but
                                              we observe only 0 or 1. There are no
                                              no observations between zero and one, even though
                                              we use a regression line to model between the two outcome values. 
                                              The next outshows shows you how the distribution of a logistic 
                                              and the probit function looks like."))),
                                 plotOutput("distplot"),
                                 HTML(paste(p("Both distributions are often used to model 
                                              binary outcomes in the social sciences. Of course, we can adjust
                                              the first scatter plot and use a
                                              logit function to describe the relationsship
                                              between X and Y instead of a regression line."))),
                                 HTML(paste(h4("Insert the logit:"))),
                                 checkboxInput("boolmethod", "Logit instead of a linear effect?", value = FALSE),
                                 plotOutput("distplot3"),
                                 HTML(paste(p("The data for the scatter plot was 
                                              simulated, that is reason why it looks
                                              nice and smooth, but I hope you get a 
                                              first impression about the difference between
                                              linear and logistic regression.")))
                        ),
                        tabPanel("Variables", icon = icon("chart-pie"),
                                 HTML(paste(h4("The independent variables:"))),
                                 HTML(paste(p("The surivival of the Titanic is a binary outcome and on 
                                              the left side you can see how many people surived, 
                                              based on a series of simple bar plot. However, If we want to explore the
                                              effect of several independent variables simultaneously, we can use a 
                                              sankey plot. A sankey plot shows you how these variable work together. 
                                              You can literally see how many people of
                                              each of group survived(1) or did not survive (0)."))),
                                 HTML(paste(h4("Add a variable:"))),
                                 div(style="display: inline-block; width: 150px;",
                                     checkboxInput("booladults", "Age", value = FALSE)),
                                 div(style="display: inline-block; width: 150px;",
                                     checkboxInput("boolclass", "Class", value = FALSE)),
                                 plotOutput("sexplot"),
                                 HTML(paste(p("What would you say? Which one has the strongest effect 
                                              on the survival? It looks like sex and class have
                                              a strong effect on survival. Instead of guessing, 
                                              we can use a logisitic regression to estimate the effect of
                                              passengers' sex, age, and class simultaneously. You never 
                                              applied a linear regression? Well, check out
                                              the regression in a nutshell app first 
                                              because on this page I assume that you are familar 
                                              with the principals of a linear regression analysis:"))),
                                 HTML(paste('<center><a href="http://edgar-treischl.de/apps/linear-regression-app/" 
                                            target="_blank"><img border="0" alt="" 
                                            src="http://edgar-treischl.de/wp-content/uploads/2021/05/OLS_Nutshell_ENG.png" 
                                            width="75%"></a></center>'))
                        ),
                        tabPanel("Model", icon = icon("robot"),
                                 HTML(paste(h4("The Model:"))),
                                 HTML(paste(p("Let's run a logistic regression. 
                                              Which of the following independent 
                                              variables do you want to include to
                                              estimate the effect on survival?"),'<br/>')),
                                 h4("Independent variables:"),
                                 div(style="display: inline-block; width: 100px;",
                                    checkboxInput("bool1", "Sex", value = FALSE)),
                                 div(style="display: inline-block; width: 100px;",
                                    checkboxInput("bool2", "Class", value = FALSE)),
                                 div(style="display: inline-block; width: 100px;",
                                    checkboxInput("bool3", "Age", value = FALSE)),
                                 verbatimTextOutput("model"),
                                 HTML(paste(p("Maybe you picked passenger's sex, but what
                                              tells you the estimate(s) of your model? The 
                                              estimate for Male compared to women is negative
                                              and -2.51? Due to the assumptions of the logistic regression,
                                              we get the logarithm of the odds to survive
                                              as a results. In case of log(odds),
                                              we can only say whether an effect is positiv or 
                                              negative and check the significane. Thus, such a
                                              estimate is hard to explain what it really means.
                                              Instead of the log odds, we can estimate odds ratios
                                              and make prediction about the probability to survive.
                                              Both are easier to interpret.")))
                        ),
                        tabPanel("Odds Ratio", icon = icon("otter"),
                                 HTML(paste(h4("Odds Ratio?"),'<br/>')),
                                 HTML(paste(p("What would be the chance to 
                                              survive for men if they had the same odds (chance)
                                              to surive compare to women? The odds would be one, 
                                              since we expect that the same amount of men 
                                              and women would surived. You can calculate the odds ratio 
                                              with the help of the logisitic regression.
                                              However, let's try to calculate it by hand to get a 
                                              better intuition what a OR means. In order to
                                              do so, the next plot how many men and women 
                                              have survived."),'<br/>')),
                                 plotOutput("sexplot2"),
                                 HTML(paste(p("Look at the bar graph and the numbers of each group. 
                                              We get the men's odds to survive if we divide the number of 
                                              survived men (109) by the number of men 
                                              who did not surive (468). Women's odds to survive are
                                              calculated the very same way (233/81). In the last 
                                              step, divide men's odds by women's odds and 
                                              you get the odds ratio for men to survive"),'<br/>')),
                                 HTML(paste(p("We don't have to work this out in our own head,
                                              just use your statistics software as a calcultor, 
                                              as the next console shows:"),'<br/>')),
                                 verbatimTextOutput("odds"),
                                 verbatimTextOutput("orperHand"),
                                 HTML(paste(h4("Remember the interpretation:"),"<ul>
                                                <li>OR = 1: No effect</li>
                                                <li>OR > 1: Positive effect</li>
                                                <li>0 < OR < 1: Negative effect</li>
                                            </ul>")),
                                 HTML(paste(p("Thus, men's chance to surive is reduced
                                              by the factor 0.08 compared to women. What 
                                              about age and the other variables in your model? 
                                              Go back to the Model tab if you did not
                                              choose any independent variable for the
                                              analysis."),'<br/>')),
                                 HTML(paste(h4("All the OR from the model you picked:"))),
                                 plotOutput("modelor"),
                                 HTML(paste(p("A lot of people argue that OR are also 
                                              not very intuitive and they provide several 
                                              good reasons why this might be the case. 
                                              For instance, include age in your model. 
                                              What would you say regarding the odds ratio
                                              for age? Has age no or at least only a small effect? 
                                              Nope, age has a substantial effect 
                                              on the chance to surive! A OR is intuitive if 
                                              we compare groups, in the case of age
                                              it is easier to examine the effect size
                                              if we calculate probabilities for the entire range of age. Go 
                                              and grab your wand, on the next page 
                                              you can make predictions and 
                                              see how each variable affects the 
                                              probability to survive."))),
                        ),
                        tabPanel("Prediction", icon = icon("magic"),
                                 HTML(paste(h4("Make a prediction by providing values:"))),
                                 HTML(paste(p("I guess this is the most intuitive interpretation
                                              of a logistic regression, since we
                                              want to know about the probability to survive,
                                              not log odds or a ratio. We can use the model to predict values!
                                              For instance, see how the prediction of survival drops if you switch
                                              from women to men. Below you can provide
                                              values for age and class as well."))),
                                 radioButtons("psex", h4("Sex"),
                                              choices = list("Male" = "male", "Female" = "female"), 
                                              selected = "female"),
                                 HTML(paste( h4("The prediction result:"))),
                                 plotOutput("predictionplot"),
                                 div(style="display: inline-block; width: 350px;",
                                     sliderInput("Age", 
                                                 h4("Age"),
                                                 min = 1,
                                                 max = 100,
                                                 value = 50)),
                                 div(style="display: inline-block; width: 350px;",
                                     sliderInput("pclass", 
                                                 h4("Class"),
                                                 min = 1,
                                                 max = 3,
                                                 value = 2))

                        ),
                        tabPanel("Performance", icon = icon("hat-wizard"),
                                 HTML(paste(h4("How well does the model perform?"))),
                                 HTML(paste(p("Probably you know that R² is often used to 
                                              assess the performance of a linear model. 
                                              Unfortunately, it is a bit
                                              harder to assess the performance of 
                                              a logistic regression. There are pseudo 
                                              R² for logistic regression to compare nested models, 
                                              but we cannot interpret them as explained variance as we
                                              do it with linear model. Instead, you may encounter
                                              two terms: sensitivity and specificity."))),
                                 HTML(paste(p("Sensitivity takes into account 
                                              whether we classified the true outcome right. 
                                              How many people who survived (true positives) 
                                              did our model classify as survivor?
                                              The mosaic plot shows you how many passengers did
                                              (not) survive; on the x-axis as we have observed and y-axis
                                              displays our prediction. We can calculate the sensitivity by dividing the true
                                              positives by all the people who survived (207/290). Thus,
                                              the sensitivity is 0.71."))),
                                 plotOutput("performanceplot"),
                                 HTML(paste(p("How many people did we classify as not-surived, who
                                              actually did not survive the Titanic (true negative)? Hence, 
                                              the specificity does the same job for the negative outcome.
                                              If you divide the true negatives by all 
                                              people who did not survive (356/424), you get the specificity."))),
                                 HTML(paste(p("A common way to combine both indicators are ROC curves. As the plot below 
                                              illustrates, a ROC curve displays the sensitivity on the y-axis, and
                                              the false positive rate (1-specificity) on the x-axis. What does
                                              the ROC curve tells you? By predicting a binary outcome, we want to achieve
                                              two things simualtaneously: We want to classify the number of people who survived correctly, 
                                              while we wish that the number of false positives is rather small. Thus, we wish
                                              to have a sensitivity of 1 and a false positive rate of zero (highlighted in black in the ROC curve below). 
                                              However, if the model does not help to predict the outcome, 
                                              the ROC curve would be a diagnal line, since a fair toss of a coin 
                                              would have the same predicitive power. 50% of the time we identify people correctly, 
                                              50% of the time we make a wrong prediction."))),
                                 plotOutput("ROC"),
                                 HTML(paste(p("Sensitivity and specificity are not the only measures of performance, but in 
                                              terms of interpretation, we should remember the further 
                                              the ROC curve is away from the diagonal (and closer to the black line), 
                                              the greater the explanatory power of the model.")))
                        )
            )

        )
    )
))
