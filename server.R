#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(titanic)
library(viridis)
library(thematic)
library(broom)
#library(tidyverse)
library(ggalluvial)
library(waiter)
library(tibble)

source("utils.R")


#Data Prep#############
train_df<- titanic::titanic_train

train_df$Survived <- factor(train_df$Survived, 
                            levels = c(0, 1),
                            labels = c("Not survived", "Survived")) 

train_df$Pclass <- factor(train_df$Pclass, 
                          levels = c(1, 2 , 3),
                          labels = c("First class", "Second class", "Third class")) 

glm_fit <- glm(Survived ~ Sex + Age + Pclass , family = binomial(link = 'logit'), data = train_df)


df <- expand.grid(Sex = c("female"), 
                  Age = c(18),
                  Pclass = 3)

input <- c("shiny", "ggplot2", "titanic", "viridis", "thematic",
           "broom", "ggalluvial", "waiter", "bslib",
           "ggbeeswarm", "caret", "ggmosaic", "precrec", "dplyr",
           "tidyr", "tibble")

check <- lapply(input, check_installed)

check <- dplyr::bind_rows(check)

#check <- purrr::map_df(input, check_installed)

allinstalled <- length(check$packages)
plist <- check$packages

validExamplesMsg <-paste0(
  "The following packages need to be installed: '",
  paste(plist, collapse = "', '"),
  "'")

if (allinstalled > 0) {
  stop(print(validExamplesMsg))
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  thematic::thematic_shiny()
    #Surivivalplots#####
    output$survivedplot <- renderPlot({
        ggplot(train_df, aes(x= Survived)) + 
            geom_bar(aes(y = (..count..)/sum(..count..)), fill=c("#999999"))+
            ylab("Percent")+
            theme_bw(base_size = 18)
        
    })
    
    output$sexplot1 <- renderPlot({
        ggplot(train_df, aes(x= Sex, fill = Survived)) + 
            geom_bar(aes(y = (..count..)/sum(..count..)))+
            theme_bw()+
            ylab("Percent")+
            theme(legend.position="bottom")+
            theme(text = element_text(size=18))+
            scale_fill_manual(values=c("#009E73","#E69F00"))
        
    })
    
    output$ageplot <- renderPlot({
        ggplot(train_df, aes(x = Age, fill = Survived)) + 
            geom_histogram()+
            theme_bw(base_size = 18)+
            ylab("")+
            theme(legend.position="bottom")+
            scale_fill_manual(values=c("#009E73","#E69F00"))
        
    })
    
    
    output$classplot <- renderPlot({
        ggplot(train_df, aes(x= Pclass, fill = Survived)) + 
            geom_bar(aes(y = (..count..)/sum(..count..)))+
            theme_bw(base_size = 18)+
            ylab("Percent")+
            theme(legend.position="bottom")+
            scale_fill_manual(values=c("#009E73","#E69F00"))
        
    })
    
    #IDEA: functional plots#####
    output$funcplot <- renderPlot({
        sigmoid <- function(x){
            return(1/(1 + exp(-x)))
        }
        
        probit <- function(x){
            return(pnorm(x))
        }
      
        ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
            stat_function(fun = sigmoid, color="#008080", size = 1)+
            stat_function(fun = probit, color="#2C3E50", size = 1)+
            annotate("text", x = 3, y = 0.85, label = c("Logit"), colour = "#008080", size = 6)+
            annotate("text", x = 0.85, y = 0.95, label = c("Probit"), colour = "#2C3E50", size = 6)+
            theme_bw(base_size = 18) +
            ggtitle("")
        
    })
    
    
    
    output$distplot2 <- renderPlot({
        set.seed(2)
        y <- rep(c(0,1), 500)
        x <- 10 + rnorm(250, 3, 3)+ rnorm(250, 10, 3)*y
        
        data <- data.frame(x, y) %>%
            as.tibble()
        
        ggplot(data, aes(x = x, y = y)) + 
            geom_point(color = "gray")+
            geom_smooth(method='lm', formula= y~x, colour = "#008080")+
            theme_bw(base_size = 18)
        
    })
    
    
    output$scatter_log <- renderPlot({
        set.seed(2)
        y <- rep(c(0,1), 500)
        x <- 10 + rnorm(250, 3, 3)+ rnorm(250, 10, 3)*y
        
        data <- data.frame(x, y) %>%
            tibble::as.tibble()
        
        if (input$boolmethod == TRUE) {
            ggplot(data, aes(x= x, y = y)) + 
                geom_point(color = "gray")+
                geom_smooth(method = "glm", 
                            method.args = list(family = "binomial"), colour = "#008080")+
                theme_bw(base_size = 18)
        } else {
            ggplot(data, aes(x = x, y = y)) + 
                geom_point(color = "gray")+
                geom_smooth(method='lm', formula= y~x, colour = "#008080")+
                theme_bw(base_size = 18)
            
        }
        
    })
    
    #Alluvialplot#####
    output$alluvialplot <- renderPlot({
        
        thematic_off()
        
        if (input$booladults == TRUE & input$boolclass == FALSE) {
          all_adults
        } else if (input$booladults == FALSE & input$boolclass == TRUE) {
          all_class
        } else if (input$booladults == TRUE & input$boolclass == TRUE) {
          all_overall
        } else {
          all_sex
        }
        
        
    })
    
    
    
    #Model: output########
    output$model <- renderPrint({
        if (input$bool1 == TRUE & input$bool2 == FALSE &  input$bool3 == FALSE) {
          summary(model_call("m1"))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
          summary(model_call("m2"))
        } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
          summary(model_call("m3"))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
          summary(model_call("m4"))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
          summary(model_call("m5"))
        } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
          summary(model_call("m6"))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
          summary(model_call("m7"))
        } else {
            print("Pick at least one independent variable, my friend .-)")
        }
    })
    
    #ODDS RATIO#####
    output$sexplot2 <- renderPlot({
        thematic_on()
        
        ggplot(train_df, aes(x= Sex, fill = Survived)) + 
            geom_bar()+
            geom_text(stat='count', aes(label=..count..), vjust=-1,
                      position = position_stack(vjust = 0), size = 6)+
            theme_bw(base_size = 18)+
            ylab("Count")+
            theme(legend.position="bottom")
    })
    
    
    output$odds <- renderText({ 
        "Men's odds: 109/469:  0.232906 \nWomen's odds: 233/81: 2.876543 \nMen's odds ratio to survive: 0.232906/2.876543"
        
    })
    
    
    output$orperHand <- renderPrint({
        Oddswoman <- 233/81
        Oddswoman
        
        Oddsman <- 109/468
        Oddsman
        
        ORman <- round(Oddsman/Oddswoman, 3)
        ORman
        
    })
    
    
    #OR for each model#####
    output$modelor <- renderPlot({
        thematic_off()
        
        if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == FALSE) {
          or1
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
          or2
        } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
          or3
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
          or4
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
          or5
        } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
          or6
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
          or7
        } else {
            print("Still, you have to pick at least one independent variable, my friend .-)")
        }
        
    })
    
    
    #Prediction#####
    output$predictionplot <- renderPlot({
        glm_fit <- glm(Survived ~ Sex + Age + as.numeric(Pclass) , family = binomial(link = 'logit'), data = train_df)
        df$Age <- input$Age
        df$Sex <- input$psex
        df$Pclass <- input$pclass
        df$total <- 100
        df$prediction <- round(predict(glm_fit, df, type = 'response'),3)*100
        thematic_off()
        
        
        ggplot(df, aes(x=Age, y=prediction)) + 
            geom_col(fill = "#E69F00") +
            geom_col(aes(y = total), alpha = 0.01, colour = "gray")+
            geom_text(aes(label = paste(prediction, "%")), 
                      hjust = 0, color="#2C3E50", size = 8)+
            coord_flip()+
            theme_minimal(base_size = 18) +
            ylab("Predicted probability of survival")+
            xlab("")+
            theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    })
    
    
    #Performance#####
    output$performanceplot <- renderPlot({
        thematic_off()
        pred_plot
    })
    
    
    #ROC plot#####
    output$ROC <- renderPlot({
        thematic_off()
      roc_curve
        
    })
    
    

})
