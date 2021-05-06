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
library(ggbeeswarm)
library(viridis)
library(thematic)
library(broom)
library(tidyverse)
library(ggalluvial)
library(caret)
library(ggmosaic)
library(precrec)


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


#Titanic data prep for alluvial
tita<- titanic::titanic_train


tita<- tita %>% 
    select(Age, Sex, Survived, Pclass)%>% 
    mutate(
        Adults = case_when(
            Age >= 0 & Age < 14    ~ "Kids",
            Age >= 14 & Age < 100   ~ "Adult",
            TRUE                   ~ "NA")
    ) %>%
    mutate(Adults = na_if(Adults, "NA"))


tita<- tita %>% 
    group_by(Sex, Survived, Pclass, Adults) %>% 
    drop_na() %>% 
    count(Sex) 


tita$Survived <- factor(tita$Survived, 
                      levels = c(0, 1),
                      labels = c("Not survived", "Survived")) 

tita$Pclass <- factor(tita$Pclass, 
                          levels = c(1, 2 , 3),
                          labels = c("1. Class", "2. Class", "3. class")) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    thematic::thematic_shiny()

    
    new_points <- reactive({
        
        df$prediction <- (round(predict(glm_fit, df, type = 'response'), 2))*100
        df$round <- (predict(glm_fit, new_points, type = 'response') > 0.5) * 1
        df$total <- 100 
    })
    
    
    
    #Sidebar plots#####
    output$survivedplot <- renderPlot({
        thematic_off()
        
        if (input$variable == 1) {
            ggplot(train_df, aes(x= Sex, fill = Survived)) + 
                geom_bar(aes(y = (..count..)/sum(..count..)))+
                theme_bw()+
                ylab("Percent")+
                theme(legend.position="bottom")+
                theme(text = element_text(size=16))+
                scale_fill_manual(values=c("#009E73","#E69F00"))
        } else if (input$variable == 2) {
            ggplot(train_df, aes(x= Pclass, fill = Survived)) + 
                geom_bar(aes(y = (..count..)/sum(..count..)))+
                theme_bw(base_size = 14)+
                ylab("Percent")+
                theme(legend.position="bottom")+
                scale_fill_manual(values=c("#009E73","#E69F00"))
        } else if (input$variable == 3) {
            ggplot(train_df, aes(x = Age, fill = Survived)) + 
                geom_histogram()+
                theme_bw(base_size = 14)+
                ylab("")+
                theme(legend.position="bottom")+
                scale_fill_manual(values=c("#009E73","#E69F00"))
        } else {
            ggplot(train_df, aes(x= Survived)) + 
                geom_bar(aes(y = (..count..)/sum(..count..)), fill=c("#999999"))+
                ylab("Percent")+
                theme_bw(base_size = 14)
        }
        
    })
    
    #IDEA: functional plots#####
    output$distplot <- renderPlot({

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
            as.tibble
        
        ggplot(data, aes(x = x, y = y)) + 
            geom_point(color = "gray")+
            geom_smooth(method='lm', formula= y~x, colour = "#008080")+
            theme_bw(base_size = 18)
        
    })
    
    
    output$distplot3 <- renderPlot({
        set.seed(2)
        y <- rep(c(0,1), 500)
        x <- 10 + rnorm(250, 3, 3)+ rnorm(250, 10, 3)*y
        
        data <- data.frame(x, y) %>%
            as.tibble
        
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
    
    #Survival plots: Alluvial#####
    output$sexplot <- renderPlot({
        
        thematic_off()
        
        if (input$booladults == TRUE & input$boolclass == FALSE) {
            ggplot(data = tita,
                   aes(axis1 = Sex, axis2 = Adults,
                       y = n)) +
                scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
                xlab("Group") +
                geom_alluvium(aes(fill = Survived), alpha = 0.75) +
                geom_stratum() +
                geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
                theme_minimal(base_size = 16)+
                scale_fill_manual(values = c("#009E73","#E69F00"))+
                theme(legend.position="bottom")
        } else if (input$booladults == FALSE & input$boolclass == TRUE) {
            ggplot(data = tita,
                   aes(axis1 = Sex, axis2 = Pclass,
                       y = n)) +
                scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
                xlab("Group") +
                geom_alluvium(aes(fill = Survived),  alpha = 0.75) +
                geom_stratum() +
                geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
                theme_minimal(base_size = 16)+
                scale_fill_manual(values = c("#009E73","#E69F00"))+
                theme(legend.position="bottom")
        } else if (input$booladults == TRUE & input$boolclass == TRUE) {
            ggplot(data = tita,
                   aes(axis1 = Sex, axis2 = Adults, axis3 = Pclass,
                       y = n)) +
                scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
                xlab("Group") +
                geom_alluvium(aes(fill = Survived),  alpha = 0.75) +
                geom_stratum() +
                geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
                theme_minimal(base_size = 16)+
                scale_fill_manual(values = c("#009E73","#E69F00"))+
                theme(legend.position="bottom")
            
        } else {
            ggplot(data = tita,
                   aes(axis1 = Sex,
                       y = n)) +
                scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
                xlab("Group") +
                geom_alluvium(aes(fill = Survived),  alpha = .75) +
                geom_stratum() +
                geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
                theme_minimal(base_size = 16)+
                scale_fill_manual(values = c("#009E73","#E69F00"))+
                theme(legend.position="bottom")
        }
        
        
    })
    
    
    
    #Model: output########
    output$model <- renderPrint({
        if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == FALSE) {
            summary(glm(Survived ~ Sex , family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
            summary(glm(Survived ~ Pclass , family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
            summary(glm(Survived ~ Age , family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
            summary(glm(Survived ~ Sex + Pclass , family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
            summary(glm(Survived ~ Sex + Pclass + Age, family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
            summary(glm(Survived ~ Sex + Age , family = binomial(link = 'logit'), data = train_df))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
            summary(glm(Survived ~ Pclass + Age , family = binomial(link = 'logit'), data = train_df))
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
            model <- glm(Survived ~ Sex , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))+
                scale_color_manual(values = c("#2C3E50"))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == FALSE) {
            model <- glm(Survived ~ Pclass , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
        } else if (input$bool1 == FALSE & input$bool2 == FALSE & input$bool3 == TRUE) {
            model <- glm(Survived ~ Age , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == FALSE) {
            model <- glm(Survived ~ Sex + Pclass , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
        } else if (input$bool1 == TRUE & input$bool2 == TRUE & input$bool3 == TRUE) {
            model <- glm(Survived ~ Sex + Pclass + Age, family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
        } else if (input$bool1 == TRUE & input$bool2 == FALSE & input$bool3 == TRUE) {
            model <- glm(Survived ~ Sex + Age , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
        } else if (input$bool1 == FALSE & input$bool2 == TRUE & input$bool3 == TRUE) {
            model <- glm(Survived ~ Pclass + Age , family = binomial(link = 'logit'), data = train_df)
            tidy(model)%>% 
                mutate(oddsRatio = exp(estimate))%>%
                select(term, estimate, oddsRatio)%>%
                mutate(term=replace(term, term=="(Intercept)", NA)) %>% 
                drop_na()%>% 
                ggplot(aes(x = term, y= oddsRatio)) + 
                geom_bar(stat="identity")+
                geom_text(aes(label=round(oddsRatio, 3)), vjust=-1, size = 6)+
                ylab("Odds Ratio")+
                xlab("Coefficient")+
                theme_bw(base_size = 18)+
                expand_limits(y=c(0,2))
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
        pred_surv <- (predict(glm_fit, train_df, type = 'response') > 0.5) * 1
        
        #diese Prognose speichern wir zusammen mit der PassengerID
        df_pred <- data.frame('PassengerId' = train_df$PassengerId, 'True' =  train_df$Survived, 
                              'Predicted' = pred_surv) %>% drop_na()
        
        
        df_pred$Predicted <- factor(df_pred$Predicted, 
                                    levels = c(0, 1),
                                    labels = c("Not survived", "Survived")) 
        
        cmMatrix<- confusionMatrix(df_pred$Predicted, df_pred$True,  positive = "Survived")
        
        
        label_df <- df_pred %>% 
            group_by(True, Predicted) %>% 
            count() %>% 
            arrange(True)
        
        df <- data.frame(
            x = c(0.32, 0.32, 0.82, 0.82),
            y = c(0.1, 0.9, 0.1, 0.9),
            text = label_df$n,
            text2 = c("True negative", "False positive", "False negative", "True positive")
        )
        
        

        sample_size = df_pred %>% group_by(True) %>% summarize(num=n())
        
        df_pred %>%
            left_join(sample_size) %>%
            mutate(True = paste0(True, "\n", "n=", num)) %>%
            ggplot() + 
            geom_mosaic(aes(x = product(True), fill = Predicted), alpha = 1)+
            scale_fill_manual(values=c("#E69F00","#009E73"))+
            geom_label(data = df, 
                       aes(x = x, y = y, 
                           label = paste0(text2, ":", text)),
                       size = 5)+
            ylab("Predicted")+
            xlab("Observed")+
            theme_minimal(base_size = 18)+
            labs(caption = paste0("Sensitivity:", round(cmMatrix$byClass[1], 2),
                                  ", Specificity:", round(cmMatrix$byClass[2], 2)))+
            theme(plot.caption = element_text(color = "black",
                                              size = 18,
                                              hjust = 0,
                                              face = "bold"))+
            theme(legend.position = "none")
    })
    
    
    #ROC plot#####
    output$ROC <- renderPlot({
        pred_roc <- (predict(glm_fit, train_df, type = 'response') > 0.5) * 1
        df_roc <- data.frame('Survived' = train_df$Survived, 'predict' =  pred_roc)
        precrec_obj <- evalmod(scores = df_roc$predict, labels = df_roc$Survived)
        
        p <- autoplot(precrec_obj, "ROC")
        
        p+
            theme_minimal(base_size = 16)+
            geom_line(size = 1, color="#E69F00")+
            geom_vline(xintercept = 0, size=1, color="black") + 
            geom_hline(yintercept = 1, size=1, color="black") + 
            geom_abline(size = 0.8, linetype = "dashed", color="darkgray")+
            theme(legend.position = "none")+
            ggtitle("")
        
    })
    

})
