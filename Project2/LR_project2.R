library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(GGally)
library(pROC)
library(caret)

#1(a)#----
# Import the data
kommuner <- read_excel("kommunerProject2.xlsx", sheet = "kommuner_2")

kommuner |> mutate(highcars = as.numeric(Cars > 600)) -> kommuner

kommuner <- mutate(kommuner,
                   highcars_cat = factor(highcars,
                                         levels = c(0, 1),
                                         labels = c("low", "high")))

ggplot(kommuner, aes(x = GRP, y = highcars, color = highcars_cat)) +
  geom_point() +
  labs(title = "Highcars vs GRP ",
       x = "grp", y = "High Cars (0/1)") +
  theme_minimal()


# Part of Sweden: 1 = Götaland; 2 = Svealand; 3 = Norrland
kommuner |> count(Part, highcars_cat) -> count_data
count_data$Part <- factor(count_data$Part, levels = c(1, 2, 3), 
                          labels = c("Götaland", "Svealand", "Norrland"))

#1(b)#----
model_1b <- glm(highcars_cat~Part, family="binomial", data=kommuner)
cbind(summary(model_1b)$coefficients, confint(model_1b))|> round(digits = 2)
cbind(exp(summary(model_1b)$coefficients), exp(confint(model_1b)))|> round(digits = 2)

kommuner_x0 <- data.frame(Part = c("Gotaland", "Svealand", "Norrland"))
kommuner_pred <- cbind(
  kommuner_x0,
  phat = predict(model_1b, kommuner_x0, type = "response"))

kommuner_pred <- cbind(
  kommuner_pred,
  logit = predict(model_1b, kommuner_x0, se.fit = TRUE))

lambda <- qnorm(1 - 0.05/2)

kommuner_pred |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> kommuner_pred

kommuner_pred |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> kommuner_pred

kommuner_pred |> mutate(
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> kommuner_pred

round(kommuner_pred[2:11],2)
#1(c)#----
model_1b <- glm(highcars_cat ~ Part, family = "binomial", data = kommuner)
model_1b_sum <- summary(model_1b)

ggplot(kommuner, aes(Transit, highcars)) +
  geom_point() +
  geom_smooth() +
  xlab("TRANSIT") +
  ylab("HIGHCARS") +
  labs(title = "Highcars (=1) or Not highcars (=0) vs transit") 

# Fit the logistic regression model for 1c
model_1c <- glm(highcars ~ Transit, family = "binomial", data = kommuner)
model_1c_sum <- summary(model_1c)

# Interval for beta, Odds and Odds Ratio
beta_1c <- model_1c$coefficients
ci.beta_1c <- confint(model_1c)
cbind(b = beta_1c, ci.beta_1c, `exp(b)` = exp(beta_1c), exp(ci.beta_1c)) |> round(digits = 2)

pred_1c <- cbind(
  kommuner,
  phat = predict(model_1c, type = "response"))

pred_1c <- cbind(
  pred_1c,
  logit = predict(model_1c, se.fit = TRUE))

pred_1c |> mutate(logit.residual.scale = NULL) -> pred_1c

lambda <- qnorm(1 - 0.05/2)
pred_1c |> mutate(
  logit.lwr = logit.fit - lambda*logit.se.fit,
  logit.upr = logit.fit + lambda*logit.se.fit) -> pred_1c

# Confidence interval for the odds
pred_1c |> mutate(
  odds.lwr = exp(logit.lwr),
  odds.upr = exp(logit.upr)) -> pred_1c

# Confidence interval for the probabilities
pred_1c |> mutate(
  p = exp(logit.fit)/(1 + exp(logit.fit)),
  p.lwr = odds.lwr/(1 + odds.lwr),
  p.upr = odds.upr/(1 + odds.upr)) -> pred_1c

color_match<-c("Moving average"="blue","Estimate of p"="red")
legend<-c("Moving average","Estimate of p")
# ggplot(pred_1c, aes(x=Transit, y=highcars)) +
#   geom_point() +
#   geom_smooth(se=FALSE,aes(color=legend[1]))+
#   geom_line(aes(y = phat,color=legend[2]),size = 1) +
#   geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
#   xlab("TRANSIT") +
#   ylab("HIGHCARS") +
#   labs(title = "Highcars (=1) or Not highcars (=0) vs transit")+
#   scale_color_manual(values = color_match, labels = legend)

ggplot(pred_1c, aes(x = Transit, y = highcars)) +
  geom_point() +
  geom_line(aes(y = phat, color = "Moving average"), size = 1) +
  geom_smooth(se = FALSE, aes(color = "Estimated probability")) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("Transit") +
  ylab("Highcars") +
  labs(title = "Highcars (=1) or Not highcars (=0) vs Transit") +
  scale_color_manual(values = color_match, labels = legend) 
# P-value
# Intercept Pr(>|z|)=5.58e-06 < 0.05, reject H0
# Transit Pr(>|z|) = 1.07e-11 < 0.05, reject H0
model_1c_sum

#LR-test against the null model
D_diff <- model_1c_sum$null.deviance - model_1c_sum$deviance
df_diff <- model_1c_sum$df.null - model_1c_sum$df.residual
chi2_alpha <- qchisq(p = 1 - 0.05, df = df_diff)
Pvalue <- pchisq(q = D_diff, df = df_diff, lower.tail = FALSE)

cbind(D_diff, df_diff, chi2_alpha, Pvalue)
#1(d)#----

model_1c <- glm(highcars ~ Transit, family = "binomial", data = kommuner)

model_1c_infl <- influence(model_1c)
glimpse(model_1c_infl)

pred_1c <- cbind(kommuner,
                 xbeta = predict(model_1c),
                 v = model_1c_infl$hat)
glimpse(pred_1c)

pplus1_1c <- length(model_1c$coefficients)
n <- nobs(model_1c)

ggplot(pred_1c, aes(x = xbeta, y = v)) +
  geom_point() +
  geom_hline(yintercept = c(2*pplus1_1c/n)) +
  facet_wrap(~ highcars)

pred_1c |> slice_max(v, n = 8)

ggplot(pred_1c, aes(x = Transit, y = v)) +
  geom_point() +
  geom_point(data = filter(pred_1c, v > 0.021), 
             aes(color = "v > 0.021"), size = 3) +
  facet_wrap(~ highcars) +
  labs(color = "Highlight",
       title = "Leverage vs linear predictor by Y=0/Y=1") +
  theme(legend.position = "top",
        text = element_text(size = 14))
#1(e)#----
aic <- AIC(model_1b, model_1c)
bic <- BIC(model_1b, model_1c)
abic <- data.frame(aic, bic)
abic |> mutate(df.1 = NULL) -> abic

lnL0 <- logLik(model_1b)[1]
lnL0

abic |> mutate(
  loglik =  c(logLik(model_1b)[1],
              logLik(model_1c)[1])) -> abic
abic |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> abic
abic
#*************************************************************************************2a
# 2a. Imputation of missing data
kommuner |> mutate(Fertility = as.numeric(Fertility)) -> kommuner#waring emerges
I <- which(is.na(kommuner$Fertility))
kommuner |> filter(Part == 3 & Coastal == 0) |>
  summarise(meanfertility = mean(Fertility, na.rm = TRUE))
kommuner$Fertility[I] <- 1.57 # meanfertility = 1.57

#2(b). Variable selection 
model_full <- glm(highcars ~ log(Higheds) + Children + Seniors + log(Income) + 
                    log(GRP) + Persperhh + Fertility + Urban + Transit + log(Apartments), 
                  family = "binomial", 
                  data = kommuner)
summary(model_full)
model_full_sum <- summary(model_full)
vif(model_full)

model_null <- glm(highcars ~ 1, family = "binomial", data = kommuner)
# AIC stepwise selection
model_aic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = TRUE,  # trace=TRUE detail information for every steps
                  k = 2)  # k=2 means using AIC
model_aic_sum <- summary(model_aic)
summary(model_aic)

# BIC stepwise selection
model_bic <- step(model_null,
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both",
                  trace = TRUE,
                  k =  log(nobs(model_full)))  # BIC
model_bic_sum <- summary(model_bic)
# VIF
vif(model_aic)
vif(model_bic)
model_aic_sum

# LR-test-partially, bic-aic
D_diff <- model_bic_sum$deviance - model_aic_sum$deviance
df_diff <- model_bic_sum$df.residual - model_aic_sum$df.residual
cbind(D_diff, df_diff)
chi2_alpha <- qchisq(1 - 0.05, df_diff)
Pvalue <- pchisq(D_diff, df_diff, lower.tail = FALSE)
cbind(D_diff, df_diff, chi2_alpha, Pvalue)

### AIC and BIC
aic <- AIC(model_aic,model_bic)
bic <- BIC(model_aic,model_bic)
abic <- data.frame(aic, bic)
abic |> mutate(df.1 = NULL) -> abic
abic

### Pseudo R2
model_null_glm <- glm(highcars ~ 1, family = "binomial", data = kommuner)
# log likelihood
lnL0 <- logLik(model_null_glm)[1]
abic |> mutate(
  loglik =  c(logLik(model_aic)[1],
              logLik(model_bic)[1])) -> abic

# McFadden
abic |> mutate(
  R2McF = 1 - loglik/lnL0,
  R2McF.adj = 1 - (loglik - (df - 1)/2)/lnL0) -> abic
abic

model_2b <- model_aic


### 2(c). Influential observations
model_2b_infl <- influence(model_2b)
glimpse(model_2b_infl)

# Get the linear predictor and the hat values for the Model 2b
model_2b_pred <- cbind(kommuner,
                       xbeta = predict(model_2b),
                       leverage = model_2b_infl$hat,
                       D = cooks.distance(model_2b))
glimpse(model_2b_pred)



pplus1_2b<-length(model_2b$coefficients)
n<-length(model_2b$residuals)
# Find the 6 highest leverages,also hight all points which has leverage bigger than 0.05
highest_leverages <- model_2b_pred %>%
  slice_max(leverage, n = 6)
#plot leverage 
ggplot(model_2b_pred, aes(x = xbeta, y = leverage)) +
  geom_point() +
  geom_point(data = filter(model_2b_pred, leverage > 0.05), aes(color = "leverage > 0.05"), size = 3) +
  geom_point(data = highest_leverages, aes(color = "Highest Leverages"), size = 4) +
  geom_hline(yintercept = c(2*pplus1_2b/n)) +
  # facet_wrap(~ highcars) +
  labs( title = "Leverage vs linear predictor for Model.2b") +
  theme(legend.position = "top", text = element_text(size = 14))

#plot cook's distance
ggplot(model_2b_pred, aes(x = xbeta, y = D)) +
  geom_point() +
  geom_point(data = filter(model_2b_pred, D>4/n),
             color = "blue", size = 3) +
  geom_hline(yintercept = 4/n) +annotate("text",label="y=4/n",x=-10,y=0.02)+
  labs(title="Cook's Distance for Model.2b") +
  geom_text(data = filter(model_2b_pred, D > 4/n),
            aes(label = Kommun),
            nudge_y = 0.01,
            hjust = 0) 


high_cook_municipalities <- filter(model_2b_pred, D > 4/n) %>%
  select(Kommun)



F2<-model_2b$df.residual-pplus1_2b
cook.limit <- 1
cook.limit
kommuner_pred_DFBETAS <- mutate(
  model_2b_pred,
  df0 = dfbetas(model_2b)[, "(Intercept)"],
  df1 = dfbetas(model_2b)[, "Urban"],
  df2 = dfbetas(model_2b)[, "log(Apartments)"],
  df3 = dfbetas(model_2b)[, "Persperhh"],
  df4 = dfbetas(model_2b)[, "log(Income)"],
  df5=dfbetas(model_2b)[,"Fertility"],
  df6=dfbetas(model_2b)[,"Transit"],
  fit = predict(model_2b),
  r = rstudent(model_2b),
  D = cooks.distance(model_2b))

dftry<-dfbetas(model_2b)
dftry
df0
top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

summary(model_2b)
# DFBETAS of Municipalities with top cook's distance 
top_cooks_DFBETAS <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6) %>%
  select(Kommun, D, df0, df1, df2, df3, df4,df5,df6)

print(top_cooks_DFBETAS)


highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|r*|>3" = "red")

# Top influential municipalities in DFBETAS
top_influential <- kommuner_pred_DFBETAS %>%
  arrange(desc(abs(df2))) %>%
  slice(1:6)
ns<- 2/sqrt(n)
# Change the y axis into df0 ~ df6, check if the resulting plot 
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = df6)) +
  geom_point(size = 2) +
  geom_point(data = top_cooks, color = "red") +
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit) * c(-1, 1), color = "red") +
  geom_hline(yintercept = 2 / sqrt(n) * c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS for beta6") +
  xlab("Fitted values") +
  labs(title = "Impact on the Transit by Municipality")+
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)


## 2(d). Deviance residuals ###
model_2b_pred |> mutate(devresid = model_2b_infl$dev.res,
                        stddevresid = devresid/sqrt(1 - leverage)) -> model_2b_pred
glimpse(model_2b_pred)

# QQ-plot of the standardized deviance residuals
ggplot(model_2b_pred, aes(sample = stddevresid)) +
  geom_qq() + geom_qq_line()

# Plot the standardized deviance residuals against the linear predictor
ggplot(model_2b_pred, aes(x = xbeta, 
                          y = stddevresid, 
                          color = as.factor(highcars))) +
  geom_point() +
  geom_point(data = top_cooks, color = "red") +
  geom_point(data=filter(model_2b_pred,abs(devresid)>3),color="black")+
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed")+
  labs(title="Standardized Deviance Residuals against Linear Residuals")

top_cooks <- model_2b_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Plot the standardized deviance residuals vs Urban
# We need to plot all the x-variables
# Urban   Apartments    Persperhh  log(Income),Fertility, 
ggplot(model_2b_pred, aes(x = Transit, y = stddevresid, 
                          color = as.factor(highcars))) +
  geom_point() +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), linetype = "dashed") +
  geom_point(data = top_cooks, color = "red", size = 4) +
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
  labs(title="Standardized Ddeviance Residuals against Transit")
#********************************************************************************
#3(a)#----
pred_phat <- cbind(
  kommuner,
  p_null = predict(model_null, type = "response"),
  p_1b = predict(model_1b, type = "response"), 
  p_1c = predict(model_1c, type = "response"), 
  p_aic = predict(model_aic, type = "response"), 
  p_bic = predict(model_bic, type = "response"), 
  p_full = predict(model_full, type = "response")
)
glimpse(pred_phat)

# pred_phat |> mutate(
#   yhat_kommuner = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))) -> pred_phat
# 
# cm_aic <- confusionMatrix(
#   data = pred_phat$yhat_kommuner, 
#   reference = pred_phat$highcars_cat,
#   positive = "high")
# cm_aic

# 
model_predictions <- pred_phat |> mutate(
  yhat_null = factor(p_null > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1b = factor(p_1b > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high"))
)


cm_null <- confusionMatrix(data = model_predictions$yhat_null, reference = model_predictions$highcars_cat, positive = "high")
cm_1b <- confusionMatrix(data = model_predictions$yhat_1b, reference = model_predictions$highcars_cat, positive = "high")
cm_1c <- confusionMatrix(data = model_predictions$yhat_1c, reference = model_predictions$highcars_cat, positive = "high")
cm_bic <- confusionMatrix(data = model_predictions$yhat_bic, reference = model_predictions$highcars_cat, positive = "high")
cm_aic <- confusionMatrix(data = model_predictions$yhat_aic, reference = model_predictions$highcars_cat, positive = "high")
cm_full <- confusionMatrix(data = model_predictions$yhat_full, reference = model_predictions$highcars_cat, positive = "high")

### Confusion matrix
extract_stats <- function(cm) { 
  list( Accuracy = cm$overall['Accuracy'], 
        P_Value_Acc_Greater_NIR = cm$overall['AccuracyPValue'], 
        Cohen_Kappa = cm$overall['Kappa'], 
        P_Value_McNemars = cm$overall['McnemarPValue'], 
        Sensitivity = cm$byClass['Sensitivity'], 
        Specificity = cm$byClass['Specificity'] ) } 

# Apply the function to all models 
stats_null <- extract_stats(cm_null) 
stats_1b <- extract_stats(cm_1b) 
stats_1c <- extract_stats(cm_1c) 
stats_bic <- extract_stats(cm_bic) 
stats_aic <- extract_stats(cm_aic) 
stats_full <- extract_stats(cm_full) 

# Combine all stats into a data frame 
table_3a <- data.frame( 
  Model = c("Null", "1b", "1c", "BIC", "AIC", "Full"), 
  Accuracy = c(stats_null$Accuracy, stats_1b$Accuracy, 
               stats_1c$Accuracy, stats_bic$Accuracy, 
               stats_aic$Accuracy, stats_full$Accuracy),
  P_Value_Acc = c(stats_null$P_Value_Acc_Greater_NIR, 
                  stats_1b$P_Value_Acc_Greater_NIR, 
                  stats_1c$P_Value_Acc_Greater_NIR, 
                  stats_bic$P_Value_Acc_Greater_NIR, 
                  stats_aic$P_Value_Acc_Greater_NIR, 
                  stats_full$P_Value_Acc_Greater_NIR), 
  Cohen_Kappa = c(stats_null$Cohen_Kappa, 
                  stats_1b$Cohen_Kappa, 
                  stats_1c$Cohen_Kappa, 
                  stats_bic$Cohen_Kappa, 
                  stats_aic$Cohen_Kappa, 
                  stats_full$Cohen_Kappa), 
  P_Value_McNemars = c(stats_null$P_Value_McNemars, 
                       stats_1b$P_Value_McNemars, 
                       stats_1c$P_Value_McNemars, 
                       stats_bic$P_Value_McNemars, 
                       stats_aic$P_Value_McNemars, 
                       stats_full$P_Value_McNemars), 
  Sensitivity = c(stats_null$Sensitivity, stats_1b$Sensitivity, 
                  stats_1c$Sensitivity, stats_bic$Sensitivity, 
                  stats_aic$Sensitivity, stats_full$Sensitivity), 
  Specificity = c(stats_null$Specificity, stats_1b$Specificity, 
                  stats_1c$Specificity, stats_bic$Specificity, 
                  stats_aic$Specificity, stats_full$Specificity) )
table_3a

#3(b)#----

roc_null <- roc(highcars ~ p_null, data = pred_phat)
roc_null
glimpse(roc_null)

roc_bic <- roc(highcars ~ p_bic, data = pred_phat)
roc_bic
coords(roc_bic) |> head()
ggroc(roc_bic) +
  coord_fixed() +
  labs(title = "ROC-curve for model bic")

ggroc(list(null = roc_null, BIC = roc_bic)) +
  coord_fixed() +
  labs(title = "ROC-curves for model oslo and the null model")

# Find best threshold for bic 
youden <- coords(roc_bic, "best")
youden

topleft <- coords(roc_bic, "best", best.method = "closest.topleft")
topleft

youden$name <- "youden"
topleft$name = "topleft"

ggroc(list(null = roc_null, oslo = roc_bic), linewidth = 1) +
  geom_point(data = topleft, aes(x = specificity, y = sensitivity), size = 3) +
  geom_point(data = youden, aes(x = specificity, y = sensitivity), size = 3) +
  coord_fixed() +
  labs(title = "ROC-curve for model oslo",
       subtitle = "with optimal thresholds") +
  theme(text = element_text(size = 18))


# ROC-curves for all the models
roc_null <- roc(highcars ~ p_null, data = pred_phat)
roc_1b <- roc(highcars ~ p_1b, data = pred_phat)
roc_1c <- roc(highcars ~ p_1c, data = pred_phat)
roc_aic <- roc(highcars ~ p_aic, data = pred_phat)
roc_bic <- roc(highcars ~ p_bic, data = pred_phat)
roc_full <- roc(highcars ~ p_full, data = pred_phat)


ggroc(list( `Model Null` = roc_null, `Model 1b` = roc_1b, `Model 1c` = roc_1c, 
            `Model AIC` = roc_aic, `Model BIC` = roc_bic, `Model Full` = roc_full),
      linewidth = 1) +
  coord_fixed() +
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))

# AUC for all models
aucs <- 
  data.frame(
    model = c("Model Null", "Model 1b", "Model 1c", "Model AIC", "Model BIC", "Model Full"),
    auc = c(auc(roc_null), auc(roc_1b), auc(roc_1c), auc(roc_aic),
            auc(roc_bic), auc(roc_full)),
    lwr = c(ci(roc_null)[1], ci(roc_1b)[1],
            ci(roc_1c)[1], ci(roc_aic)[1],
            ci(roc_bic)[1], ci(roc_full)[1]),
    upr = c(ci(roc_null)[3], ci(roc_1b)[3],
            ci(roc_1c)[3], ci(roc_aic)[3],
            ci(roc_bic)[3], ci(roc_full)[3]))
aucs

roc_2b <- roc_aic

### pair-wise tests comparing the AUC
# Perform ROC tests
test_2b_vs_null = roc.test(roc_null, roc_2b)
test_2b_vs_1b = roc.test(roc_1b, roc_2b)
test_2b_vs_1c = roc.test(roc_1c, roc_2b)
test_2b_vs_bic = roc.test(roc_bic, roc_2b)
test_2b_vs_full = roc.test(roc_full, roc_2b)

# Create a data frame to store the results
roc_comparison_results <- data.frame(
  Comparison = c("Model 2b vs Null", "Model 2b vs 1b", "Model 2b vs 1c", "Model 2b vs BIC", "Model 2b vs Full"),
  p_Value = c(test_2b_vs_null$p.value, test_2b_vs_1b$p.value, test_2b_vs_1c$p.value, test_2b_vs_bic$p.value, test_2b_vs_full$p.value),
  Test_Statistic = c(test_2b_vs_null$statistic, test_2b_vs_1b$statistic, test_2b_vs_1c$statistic, test_2b_vs_bic$statistic, test_2b_vs_full$statistic),
  Alternative_Hypothesis = rep(test_2b_vs_null$alternative, 5)
)

roc_comparison_results

#3(c)#----
extract_metrics <- function(roc_obj) { 
  youden <- coords(roc_obj, "best") 
  topleft <- coords(roc_obj, "best", best.method = "closest.topleft") 
  data.frame( Threshold_Youden = youden$threshold, 
              Sensitivity_Youden = youden$sensitivity, 
              Specificity_Youden = youden$specificity, 
              Threshold_TopLeft = topleft$threshold, 
              Sensitivity_TopLeft = topleft$sensitivity, 
              Specificity_TopLeft = topleft$specificity ) } 

# Calculate metrics for all required models 
metrics_1b <- extract_metrics(roc_1b) 
metrics_1c <- extract_metrics(roc_1c) 
metrics_aic <- extract_metrics(roc_aic) 
metrics_bic <- extract_metrics(roc_bic) 
metrics_full <- extract_metrics(roc_full) 

# Combine all metrics into a single data frame 
roc_metrics <- data.frame( 
  Model = c("1b", "1c", "AIC", "BIC", "Full"), 
  rbind(metrics_1b, metrics_1c, metrics_aic, metrics_bic, metrics_full) 
) 

# Display the table 
roc_metrics

### Table 3c using optimal thresholds
model_predictions_3c <- pred_phat |> mutate(
  yhat_null = factor(p_null > 0.5, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1b = factor(p_1b > 0.1655906, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_1c = factor(p_1c > 0.1977642, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_bic = factor(p_bic > 0.2244232, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_aic = factor(p_aic > 0.2089494, levels = c(FALSE, TRUE), labels = c("low", "high")),
  yhat_full = factor(p_full > 0.2773927, levels = c(FALSE, TRUE), labels = c("low", "high"))
)

# Get confusion matrix for all the models
cm_null_3c <- confusionMatrix(data = model_predictions_3c$yhat_null, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_1b_3c <- confusionMatrix(data = model_predictions_3c$yhat_1b, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_1c_3c <- confusionMatrix(data = model_predictions_3c$yhat_1c, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_bic_3c <- confusionMatrix(data = model_predictions_3c$yhat_bic, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_aic_3c <- confusionMatrix(data = model_predictions_3c$yhat_aic, reference = model_predictions_3c$highcars_cat, positive = "high")
cm_full_3c <- confusionMatrix(data = model_predictions_3c$yhat_full, reference = model_predictions_3c$highcars_cat, positive = "high")

### Confusion matrix
extract_stats <- function(cm) { 
  list( Accuracy = cm$overall['Accuracy'], 
        P_Value_Acc_Greater_NIR = cm$overall['AccuracyPValue'], 
        Cohen_Kappa = cm$overall['Kappa'], 
        P_Value_McNemars = cm$overall['McnemarPValue'], 
        Sensitivity = cm$byClass['Sensitivity'], 
        Specificity = cm$byClass['Specificity'] ) } 

# Apply the function to all models 
stats_null_3c <- extract_stats(cm_null_3c) 
stats_1b_3c <- extract_stats(cm_1b_3c) 
stats_1c_3c <- extract_stats(cm_1c_3c) 
stats_bic_3c <- extract_stats(cm_bic_3c) 
stats_aic_3c <- extract_stats(cm_aic_3c) 
stats_full_3c <- extract_stats(cm_full_3c) 

# Combine all stats into a data frame 
table_3c <- data.frame( 
  Model = c("Null", "1b", "1c", "BIC", "AIC", "Full"), 
  Accuracy = c(stats_null_3c$Accuracy, stats_1b_3c$Accuracy, 
               stats_1c_3c$Accuracy, stats_bic_3c$Accuracy, 
               stats_aic_3c$Accuracy, stats_full_3c$Accuracy),
  P_Value_Acc = c(stats_null_3c$P_Value_Acc_Greater_NIR, 
                  stats_1b_3c$P_Value_Acc_Greater_NIR, 
                  stats_1c_3c$P_Value_Acc_Greater_NIR, 
                  stats_bic_3c$P_Value_Acc_Greater_NIR, 
                  stats_aic_3c$P_Value_Acc_Greater_NIR, 
                  stats_full_3c$P_Value_Acc_Greater_NIR), 
  Cohen_Kappa = c(stats_null_3c$Cohen_Kappa, 
                  stats_1b_3c$Cohen_Kappa, 
                  stats_1c_3c$Cohen_Kappa, 
                  stats_bic_3c$Cohen_Kappa, 
                  stats_aic_3c$Cohen_Kappa, 
                  stats_full_3c$Cohen_Kappa), 
  P_Value_McNemars = c(stats_null_3c$P_Value_McNemars, 
                       stats_1b_3c$P_Value_McNemars, 
                       stats_1c_3c$P_Value_McNemars, 
                       stats_bic_3c$P_Value_McNemars, 
                       stats_aic_3c$P_Value_McNemars, 
                       stats_full_3c$P_Value_McNemars), 
  Sensitivity = c(stats_null_3c$Sensitivity, stats_1b_3c$Sensitivity, 
                  stats_1c_3c$Sensitivity, stats_bic_3c$Sensitivity, 
                  stats_aic_3c$Sensitivity, stats_full_3c$Sensitivity), 
  Specificity = c(stats_null_3c$Specificity, stats_1b_3c$Specificity, 
                  stats_1c_3c$Specificity, stats_bic_3c$Specificity, 
                  stats_aic_3c$Specificity, stats_full_3c$Specificity) )
table_3c

#3(d)#----
cbind(summary(model_aic)$coefficients, confint(model_aic))|> round(digits = 2)
