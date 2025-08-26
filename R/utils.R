# --- Libraries ---
library(ggplot2)
library(titanic)
library(ggbeeswarm)
library(viridis)
library(thematic)
library(broom)
library(ggalluvial)
library(caret)
library(ggmosaic)
library(precrec)
library(dplyr)
library(tidyr)


# --- 1. Data Preparation ---

train_df <- titanic::titanic_train

train_df$Survived <- factor(train_df$Survived, levels = c(0, 1),
                            labels = c("Not survived", "Survived"))

train_df$Pclass <- factor(train_df$Pclass, levels = c(1, 2 , 3),
                          labels = c("First class", "Second class", "Third class"))

# This model is used in prediction
glm_fit <- glm(Survived ~ Sex + Age + as.numeric(Pclass),
               family = binomial(link = 'logit'), data = train_df)


# --- 2. Alluvial Plot Data ---

tita <- titanic::titanic_train %>%
  select(Age, Sex, Survived, Pclass) %>%
  mutate(
    Adults = case_when(
      Age >= 0 & Age < 14 ~ "Kids",
      Age >= 14 & Age < 100 ~ "Adult",
      TRUE ~ "NA"
    ),
    Adults = na_if(Adults, "NA")
  ) %>%
  group_by(Sex, Survived, Pclass, Adults) %>%
  drop_na() %>%
  count(Sex)

tita$Survived <- factor(tita$Survived, levels = c(0, 1),
                        labels = c("Not survived", "Survived"))

tita$Pclass <- factor(tita$Pclass, levels = c(1, 2 , 3),
                      labels = c("1. Class", "2. Class", "3. class"))


# --- 3. Alluvial Plots ---

all_adults <- ggplot(tita, aes(axis1 = Sex, axis2 = Adults, y = n)) +
  scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
  xlab("Group") +
  geom_alluvium(aes(fill = Survived), alpha = 0.75) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("#009E73","#E69F00")) +
  theme(legend.position = "bottom")

all_class <- ggplot(tita, aes(axis1 = Sex, axis2 = Pclass, y = n)) +
  scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
  xlab("Group") +
  geom_alluvium(aes(fill = Survived), alpha = 0.75) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("#009E73","#E69F00")) +
  theme(legend.position = "bottom")

all_overall <- ggplot(tita, aes(axis1 = Sex, axis2 = Adults, axis3 = Pclass, y = n)) +
  scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
  xlab("Group") +
  geom_alluvium(aes(fill = Survived), alpha = 0.75) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("#009E73","#E69F00")) +
  theme(legend.position = "bottom")

all_sex <- ggplot(tita, aes(axis1 = Sex, y = n)) +
  scale_x_discrete(limits = c("Sex", "Adults", "Class"), expand = c(.2, .05)) +
  xlab("Group") +
  geom_alluvium(aes(fill = Survived), alpha = .75) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = rel(5)) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("#009E73","#E69F00")) +
  theme(legend.position = "bottom")


# --- 4. Logistic Model Switch Function ---

model_call <- function(type) {
  switch(type,
         m1 = glm(Survived ~ Sex, data = train_df, family = binomial),
         m2 = glm(Survived ~ Pclass, data = train_df, family = binomial),
         m3 = glm(Survived ~ Age, data = train_df, family = binomial),
         m4 = glm(Survived ~ Sex + Pclass, data = train_df, family = binomial),
         m5 = glm(Survived ~ Sex + Pclass + Age, data = train_df, family = binomial),
         m6 = glm(Survived ~ Sex + Age, data = train_df, family = binomial),
         m7 = glm(Survived ~ Pclass + Age, data = train_df, family = binomial))
}


# --- 5. Odds Ratio Plots ---

or_plot <- function(model_name) {
  tidy(model_call(model_name)) %>%
    mutate(oddsRatio = exp(estimate)) %>%
    select(term, oddsRatio) %>%
    mutate(term = replace(term, term == "(Intercept)", NA)) %>%
    drop_na() %>%
    ggplot(aes(x = term, y = oddsRatio)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(oddsRatio, 3)), vjust = -1, size = 6) +
    ylab("Odds Ratio") +
    xlab("Coefficient") +
    theme_bw(base_size = 16) +
    expand_limits(y = c(0, 2)) +
    scale_color_manual(values = c("#2C3E50"))
}

or1 <- or_plot("m1")
or2 <- or_plot("m2")
or3 <- or_plot("m3")
or4 <- or_plot("m4")
or5 <- or_plot("m5")
or6 <- or_plot("m6")
or7 <- or_plot("m7")


# --- 6. Prediction Performance Plots ---

pred_surv <- (predict(glm_fit, train_df, type = 'response') > 0.5) * 1

df_pred <- data.frame(PassengerId = train_df$PassengerId,
                      True = train_df$Survived,
                      Predicted = pred_surv) %>%
  drop_na()

df_pred$Predicted <- factor(df_pred$Predicted,
                            levels = c(0, 1),
                            labels = c("Not survived", "Survived"))

cmMatrix <- confusionMatrix(df_pred$Predicted, df_pred$True, positive = "Survived")

label_df <- df_pred %>%
  group_by(True, Predicted) %>%
  count() %>%
  arrange(True)

fcukit <- data.frame(
  x = c(0.32, 0.32, 0.82, 0.82),
  y = c(0.1, 0.9, 0.1, 0.9),
  text = label_df$n,
  text2 = c("True negative", "False positive", "False negative", "True positive")
)

sample_size <- df_pred %>%
  group_by(True) %>%
  summarize(num = n())

df_pred <- df_pred %>%
  left_join(sample_size) %>%
  mutate(True = paste0(True, "\nn=", num))

pred_plot <- ggplot(df_pred) +
  geom_mosaic(aes(x = product(True), fill = Predicted), alpha = 1) +
  scale_fill_manual(values = c("#E69F00", "#009E73")) +
  geom_label(data = fcukit,
             aes(x = x, y = y, label = paste0(text2, ": ", text)),
             size = 5) +
  ylab("Predicted") +
  xlab("Observed") +
  theme_minimal(base_size = 18) +
  labs(caption = paste0("Sensitivity: ", round(cmMatrix$byClass["Sensitivity"], 2),
                        ", Specificity: ", round(cmMatrix$byClass["Specificity"], 2))) +
  theme(
    plot.caption = element_text(color = "black", size = 18, hjust = 0, face = "bold"),
    legend.position = "none"
  )


# Generate ROC curve
df_roc <- data.frame(
  Survived = train_df$Survived,
  predict = predict(glm_fit, train_df, type = 'response')
)

precrec_obj <- evalmod(scores = df_roc$predict, labels = df_roc$Survived)

roc_curve <- autoplot(precrec_obj, "ROC") +
  theme_minimal(base_size = 18) +
  geom_line(size = 1, color = "#E69F00") +
  geom_vline(xintercept = 0, size = 1, color = "black") +
  geom_hline(yintercept = 1, size = 1, color = "black") +
  geom_abline(linetype = "dashed", size = 0.8, color = "darkgray") +
  theme(legend.position = "none") +
  ggtitle("")



