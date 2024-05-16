library(readxl)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(car)
library(GGally)

###1(a)###----
# Import the data
my_data <- read_excel("kommuner.xlsx", sheet = "kommuner")
#log or not on PM10
ggplot(my_data, aes(x =Vehicles , y =PM10)) + geom_point()
ggplot(my_data, aes(x =Vehicles , y =log(PM10))) + geom_point()
#better, follow diag line
ggplot(my_data, aes(x =log(Vehicles) , y =log(PM10))) + geom_point() 

model_1b <- lm(log(PM10) ~ log(Vehicles), data = my_data)

residuals <- model_1b$residuals
fitted_values <- model_1b$fitted.values

ggplot(data.frame(residuals, fitted_values), aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted Values (Y_hat)") +
  ylab("Residuals")

##**************1.(b)*******************

#beta estimates, 95% confidence  inetrvals
beta_ln_ln<-model_PMln_ln$coefficients
beta_95intv_ln_ln<-confint(model_PMln_ln)
# rm(beta_esti)   #remove  the data block in  the right-hand side


#plot scatter points,fitted lines,95% confint and predint——ln(PM10)~ln(Vehicles)

#confint#predint for ln_ln model
df_1$confint_ln_ln<-data.frame(predict(model_PMln_ln,newdata=data.frame(Vehicles=kommuner_data$Vehicles),interval="confidence",level=0.95))
df_1$predint_ln_ln<-data.frame(predict(model_PMln_ln,newdata=data.frame(Vehicles=kommuner_data$Vehicles),interval="prediction",level=0.95))
df_1$confint_ln_ln$fit <- NULL
df_1$predint_ln_ln$fit <- NULL

#plot ln-ln  model

ggplot(df_1, aes(x = Vehicles_ln)) + 
  geom_smooth(aes(y = fit_ln_ln), method = "lm", se = FALSE) +
  geom_point(aes(y = PM10_ln)) +
  geom_ribbon(data = df_1, aes(x = Vehicles_ln, ymin = confint_ln_ln$lwr, ymax = confint_ln_ln$upr, fill = "Confidence Interval"), alpha = 0.1) +
  geom_ribbon(data = df_1, aes(x = Vehicles_ln, ymin = predint_ln_ln$lwr, ymax = predint_ln_ln$upr, fill = "Prediction Interval"), alpha = 0.1) +
  scale_fill_manual(name = "Interval",
                    values = c("Confidence Interval" = "blue", "Prediction Interval" = "red")) +
  labs(title = "ln(PM10) ~ ln(Vehicles)",
       x = "ln(Vehicles)",
       y = "ln(PM10)") 




#below operation can automatically   generate confint and predint 
df_1$confint_trans_upr<-data.frame(exp(df_1$confint_ln_ln$upr))
df_1$confint_trans_lwr<-data.frame(exp(df_1$confint_ln_ln$lwr))
df_1$predint_trans_upr<-data.frame(exp(df_1$predint_ln_ln$upr))
df_1$predint_trans_lwr<-data.frame(exp(df_1$predint_ln_ln$lwr))

#plot  scatter points,fitted lines,95% confint and predint——ln(PM10)~Vehicles
ggplot(df_1, aes(x = exp(Vehicles_ln)) )+ 
  geom_smooth(aes(y = exp(fit_ln_ln)), method = "lm", se = FALSE) +
  geom_point(aes(y = exp(PM10_ln)))+
  geom_ribbon(data = df_1, aes(x = exp(Vehicles_ln), ymin = exp(confint_ln_ln$lwr), ymax =exp( confint_ln_ln$upr),fill = "Confidence Interval"),alpha = 0.1) +
  geom_ribbon(data = df_1, aes(x = exp(Vehicles_ln), ymin =exp(predint_ln_ln$lwr) , ymax =exp(predint_ln_ln$upr), fill = "Prediction Interval"),alpha = 0.1) +
  scale_fill_manual(name = "Interval",values = c("Confidence Interval" = "blue", "Prediction Interval" = "red")) +
  labs(title = "PM10 ~ Vehicles(transformed)", x = "Vehicles",y = "PM10") 


#remove wrong stub
#df_1$predint_trans_lwr <- NULL
rm(beta1)

##**************1.(c)*******************
# ##*#confint_dec
#   df_1$confint_dec<-data.frame(predict(model_PMln_ln,newdata=data.frame(Vehicles=0.9*kommuner_data$Vehicles),interval="confidence",level=0.95))
#   
#   
#   ggplot( df_1, aes(x = exp(Vehicles_ln) ))+ 
#     geom_smooth(aes(y = exp(confint_dec$fit)), method = "lm", se = FALSE) +
#     geom_point(aes(y = exp(PM10_ln)))+
#     geom_ribbon(data = df_1, aes(x = exp(Vehicles_ln), ymin = exp(confint_dec$lwr), ymax =exp( confint_dec$upr)),alpha = 0.1) +
#       labs(title = "PM10 ~ Vehicles(10% vehicle number decresed)", x = "Vehicles",y = "PM10") 
#   
#  #confint_half
#   model_1c<-lm(log(PM10)~log(Vehicles),data = kommuner_data)
#   df_1$confint_half<-data.frame(predict(model_1c,newdata=data.frame(PM10=0.5*exp(df_1$fit_ln_ln)),interval="confidence",level=0.95))
#   
#   ggplot( df_1, aes(x = exp(PM10_ln) ))+ 
#     geom_smooth(aes(y = exp(confint_dec$fit)), method = "lm", se = FALSE) +
#     geom_point(aes(y = exp(PM10_ln)))+
#     geom_ribbon(data = df_1, aes(x = exp(Vehicles_ln), ymin = exp(confint_dec$lwr), ymax =exp( confint_dec$upr)),alpha = 0.1) +
#     labs(title = "PM10 ~ Vehicles(10% vehicle number decresed)", x = "Vehicles",y = "PM10") 

beta1 <- 1.29



beta1_se <- summary(model_PMln_ln)$coefficients["I(log(Vehicles))", "Std. Error"]


# ln(0.9)
ln_0_9 <- log(0.9)

# Changed PM10
delta_ln_PM10 <- beta1 * ln_0_9

# Calculate percentage of the change
percent_change_PM10 <- (exp(delta_ln_PM10) - 1) * 100

# 95% vi
z_value <- qnorm(0.975)
# ci_lower_r <- percent_change_PM10 - z_value * beta1_se* 10
# ci_upper_r <- percent_change_PM10 + z_value * beta1_se * 10
ci_lower_r <- (exp(1.170085*ln_0_9) - 1) * 100
ci_upper_r <- (exp(1.403780*ln_0_9) - 1) * 100
# To determine the reduction in vehicles needed to halve PM10 emissions
required_reduction <-0.5^(beta1)
print(required_reduction)
ci_lower <-  0.5^1.170085*100

ci_upper <-0.5^1.403780*100
cat("Expected change in PM10 for a 10% decrease in vehicles:", percent_change_PM10, "%\n")
cat("95% CI for this change rate: [", ci_lower_r, ",", ci_upper_r, "]\n")
cat("Reduction in vehicles needed to halve PM10 emissions:", required_reduction * 100, "%\n")
cat("95% CI for reduction: [", ci_lower, ",", ci_upper, "]\n")

###2(a)###----
summary(model_1b)
###2(b)###----
coast <- factor(my_data$Coastal, labels = c("No", "Yes"))
my_data <- mutate(my_data, Coastal = coast)
my_data$Part <- factor(my_data$Part, labels = c("Gotaland", "Svealand", "Norrland"))

model_2b = lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=my_data)
model_2b$coefficients

count(my_data, Part == "Gotaland", Coastal == "Yes")
count(my_data, Part == "Svealand", Coastal == "Yes")
count(my_data, Part == "Norrland", Coastal == "Yes")
confint(model_2b)

coast_part_lm<-lm(log(PM10)~log(Vehicles)+Coastal+Part, data=my_data)
anova(coast_part_lm, model_1b)

anova(model_2b, coast_part_lm)

test_data <- data.frame(
  Vehicles = 1000,
  Coastal = c("Yes", "Yes", "Yes", "No", "No", "No"),
  Part = c("Gotaland", "Svealand", "Norrland", "Gotaland", "Svealand", "Norrland")
)


for (i in 1:nrow(test_data)) {
  prediction <- predict(model_2b, newdata = test_data[i,], interval = "confidence")
  cat("Confint for", test_data[i, "Coastal"], test_data[i, "Part"], ":", prediction, "\n")
  cat("Exp confint for", test_data[i, "Coastal"], test_data[i, "Part"], ":", exp(prediction), "\n \n")
}

###2(c)###----
# my_data <- mutate(my_data, Coastal = relevel(Coastal,"Yes"))
# model_lev <- lm(log(PM10) ~ log(Vehicles) + Coastal*Part, data=my_data)
# 
# my_data <-
#  mutate(my_data, NewParts =
#           as.numeric(Part == "Gotaland" | Part == "Svealand") +
#           2*as.numeric(Part == "Norrland" & Coastal == "Yes") +
#           3*as.numeric(Part == "Norrland" & Coastal == "No"))
# 
# my_data$NewParts <- factor(my_data$NewParts, labels = c("GotalandSveland", "NorrlandYes", "NorrlandNo"))
# 
# test_model <- lm(log(PM10)~ log(Vehicles) + NewParts, data=my_data)
# 
# test_data2 <- data.frame(
#   Vehicles = 1000,
#   NewParts = c("GotalandSveland", "NorrlandYes", "NorrlandNo"))
# 
# 
# for (i in 1:nrow(test_data2)) {
#   prediction <- predict(test_model, newdata = test_data2[i,], interval = "confidence")
#   cat("Exp confint for", test_data2[i, "NewParts"], ":", exp(prediction), "\n \n")
# }

my_data <-
  mutate(my_data, NewParts2 =
           as.numeric(Part == "Gotaland" | (Part == "Svealand"& Coastal == "Yes")) +
           2*as.numeric(Part == "Svealand" & Coastal == "No") +
           3*as.numeric(Part == "Norrland" & Coastal == "Yes") +
           4*as.numeric(Part == "Norrland" & Coastal == "No"))

my_data$NewParts2 <- factor(my_data$NewParts2, labels = c("1", "SvealandNo","NorrlandYes", "NorrlandNo"))

model_2c <- lm(log(PM10)~ log(Vehicles) + NewParts2, data=my_data)

anova(model_2b, model_2c)
cbind(summary(model_2c)$coefficients, confint(model_2c))

# test_data3 <- data.frame(
#   Vehicles = 1000,
#   NewParts2 = c("1", "SvealandNo","NorrlandYes", "NorrlandNo"))
# 
# for (i in 1:nrow(test_data3)) {
#   prediction <- predict(test_model2, newdata = test_data3[i,], interval = "confidence")
#   cat("Exp confint for", test_data3[i, "NewParts"], ":", exp(prediction), "\n \n")
# }


###2(d)###----

numeric_vars <- sapply(my_data, is.numeric) & names(my_data) != "PM10"
numeric_data <- my_data[, numeric_vars]

my_data$log_PM10 <- log(my_data$PM10 + 1e-3)

plots <- list()

for (var in names(numeric_data)) {
  # fit 
  lin_model <- lm(log_PM10 ~ get(var), data = my_data)
  log_model <- lm(log_PM10 ~ log(get(var) + 1e-3), data = my_data)
  
  my_data$yhatlin <- predict(lin_model, newdata = my_data)
  my_data$yhatlog <- predict(log_model, newdata = my_data)
  my_data$elin <- resid(lin_model)
  my_data$elog <- resid(log_model)
  
  p1 <- ggplot(my_data, aes_string(x = var, y = "log_PM10")) +
    geom_point() +
    ggtitle(paste("Relationship of", var, "with log(PM10)"))
  
  p2 <- ggplot(my_data, aes_string(x = paste0("log(", var, "+1e-6)"), y = "log_PM10")) +
    geom_point() +
    ggtitle(paste("Log relationship of", var, "with log(PM10)"))
  
  p3 <- ggplot(my_data, aes(x = yhatlin, y = elin)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    ggtitle("Residual Plot without Log")
  
  p4 <- ggplot(my_data, aes(x = yhatlog, y = elog)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    ggtitle("Residual Plot with Log")
  
  p5 <- ggplot(data = my_data, aes(sample = elin)) +
    geom_qq() +
    geom_qq_line() +
    ggtitle("Normal Q-Q-plot of the residuals without Log")
  
  p6 <- ggplot(data = my_data, aes(sample = elog)) +
    geom_qq() +
    geom_qq_line() +
    ggtitle("Normal Q-Q-plot of the residuals with Log")
  
  combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
  plots[[var]] <- combined_plot
}


model_x <- lm(log(PM10)~ log(Vehicles)+log(Seniors)+log(Builton), data=my_data)
cbind(summary(model_x)$coefficients, confint(model_x))

vif(model_x)
ggpairs(my_data,columns=c(5,6,8))

model_2d <- lm(log(PM10)~log(Vehicles)+log(Seniors), data=my_data)
vif(model_2d)
cbind(summary(model_2d)$coefficients, confint(model_2d))

#2(e)####
model_2ee <- lm(log(PM10)~log(Vehicles)+log(Higheds)+log(Seniors)+Children+log(Income)+
                  log(GRP)+NewParts2, data=my_data)
vif(model_2ee)
ggpairs(my_data,columns=c(5,7,8,9,10,11,13)) #remove seniors
model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+
                 NewParts2, data=my_data)
vif(model_2e)
summary(model_2e)

# 3(a). Leverage.----
model_linear_3a <- lm(PM10 ~ Vehicles + Higheds + Children + Income + GRP + 
                        NewParts, data = kommuner)
kommuner_pred <- mutate(kommuner,
                        yhat_linear = predict(model_linear_3a),    
                        yhat = predict(model_2e),
                        r = rstudent(model_2e),
                        v = hatvalues(model_2e),
                        D = cooks.distance(model_2e))

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
pplus1 <- length(model_2e$coefficients)
n <- nobs(model_2e)

# Get top leverage
top_leverage <- kommuner_pred %>%
  arrange(desc(v)) %>%
  slice(1:6)

ggplot(kommuner_pred, aes(x = yhat, y = v)) +
  #  facet_wrap(~NewParts) +
  geom_point(size = 2)  +
  geom_point(data = top_leverage, aes(x = yhat, y = v), color = "red", size = 3) +  
  geom_text(data = top_leverage, aes(x = yhat, y = v, label = Kommun), 
            vjust = -1, color = "blue") + 
  geom_hline(yintercept = 1/n) +
  geom_hline(yintercept = 2*pplus1/n, color = "red") +
  labs(title = "Kommuner: leverage vs predictor",
       caption = "y = 1/n (black) and 2(p+1)/n (red)",
       color = "Highlight") +
  theme(legend.position = "bottom",
        text = element_text(size = 16))


# define columns with interested variables
columns_interest <- c(5, 7, 9, 10, 11)
column_names <- names(kommuner)[columns_interest]

# plot all the combinations of x-variables
plot_list <- list()
for (i in seq_along(column_names)) {
  for (j in seq_along(column_names)) {
    if (i < j) {
      p <- ggplot(kommuner, aes_string(x = column_names[i], y = column_names[j])) +
        geom_point() +
        facet_wrap(~NewParts) +
        geom_point(data = top_leverage, aes_string(x = column_names[i], y = column_names[j]), color = "red") +
        geom_text(data = top_leverage, aes_string(x = column_names[i], y = column_names[j], label = "Kommun"), vjust = -1.5, color = "blue", size = 3)
      plot_list[[length(plot_list) + 1]] <- p
    }
  }
}


# use gridExtra to arrange this plots
do.call(gridExtra::grid.arrange, c(plot_list, ncol = length(column_names) - 1))

for (p in plot_list) {
  print(p)  
}

################################################################################
# 3(b). Cook’s distance. 
f1.kommuner <- pplus1
f2.kommuner <- model_2e$df.residual
cook.limit.kommuner <- qf(0.1, f1.kommuner, f2.kommuner)

top_cooks <- kommuner_pred %>%
  arrange(desc(D)) %>%
  slice(1:6)

ggplot(kommuner_pred, aes(x = yhat, y = D)) + 
  geom_point(size = 3) +
  geom_point(data = top_cooks, color = "red", size = 4) +  
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1, color = "blue") + 
  geom_hline(yintercept = cook.limit.kommuner, color = "blue") +  
  geom_hline(yintercept = 4/n, linetype = 2, color = "red") +  
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Kommuner: Cook's D",
       caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)",
       color = "Highlight") +
  theme(text = element_text(size = 18))

# DFBETAS 
head(dfbetas(model_2e))

dfbetas_values <- dfbetas(model_2e)

max_dfbetas_indices <- apply(dfbetas_values, 2, which.max)

# Get the name of these municipalities
influential_municipalities <- kommuner_pred$Kommun[max_dfbetas_indices]

# Plot log(PM10) vs variables, maybe we need to change the β-parameter
# model_2e <- lm(log(PM10)~log(Vehicles)+log(Higheds)+Children+log(Income)+log(GRP)+NewParts, data=kommuner)
independent_vars <- c("log(Vehicles)", "log(Higheds)", "Children", "log(Income)", "log(GRP)")


for (var in independent_vars) {
  ggplot(kommuner_pred, aes_string(x = var, y = "log(PM10)")) +
    geom_point() +
    geom_point(data = top_cooks, color = "red", size = 4) +
    geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
    xlab(paste(var, "(1000/capita)")) +
    ylab("log(PM10) (g)") +
    labs(title = "Influence of Municipalities on PM10",
         subtitle = paste("Red points indicate municipalities with significant influence on beta parameters for", var)) +
    theme(text = element_text(size = 16))
  print(ggplot2::last_plot())  # plot all the vars
}


kommuner_pred_DFBETAS <- mutate(
  kommuner_pred,
  df0 = dfbetas(model_2e)[, "(Intercept)"],
  df1 = dfbetas(model_2e)[, "log(Vehicles)"],
  df2 = dfbetas(model_2e)[, "log(Higheds)"],
  df3 = dfbetas(model_2e)[, "Children"],
  df4 = dfbetas(model_2e)[, "log(Income)"],
  df5 = dfbetas(model_2e)[, "log(GRP)"],
  fit = predict(model_2e),
  r = rstudent(model_2e),
  D = cooks.distance(model_2e))

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# DFBETAS of Municipalities with top cook's distance 
top_cooks_DFBETAS <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6) %>%
  select(Kommun, D, df0, df1, df2, df3, df4, df5)

print(top_cooks_DFBETAS)


highlightshapes <- c("Cook's D>0.1" = 24)
highlightcolors <- c("|r*|>3" = "red",
                     "length>200" = "magenta", 
                     "all data" = "orange",
                     "excl.length>200" = "blue")

# Top influential municipalities in DFBETAS
top_influential <- kommuner_pred_DFBETAS %>%
  arrange(desc(abs(df2))) %>%
  arrange(desc(abs(df2))) %>%
  slice(1:6)

# Change the y axis into df0 ~ df5, check if the resulting plot 
# is the same as the previous plot of log(PM10) vs variables.
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = df2)) +
  geom_point(size = 2) +
  geom_point(data = filter(kommuner_pred_DFBETAS, abs(r) > 3),
             aes(color = "|r*|>3"), size = 3) +
  geom_point(data = filter(kommuner_pred_DFBETAS, D > 0.1),
             aes(shape = "Cook's D>0.1"), size = 3) +
  geom_point(data = top_cooks, color = "green", size = 4) +
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.kommuner) * c(-1, 1), color = "red") +
  geom_hline(yintercept = 2 / sqrt(n) * c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS for log(Higheds)") +
  xlab("Fitted values") +
  labs(title = "Impact on the Intercept by Municipality",
       subtitle = "Highlighting municipalities with significant influence",
       caption = "Red lines indicate critical values for influence") +
  theme(text = element_text(size = 18), 
        legend.position = "bottom") +
  scale_color_manual(values = highlightcolors) +
  scale_shape_manual(values = highlightshapes)


################################################################################
# 3(c). Studentized residuals.
filter(kommuner_pred_DFBETAS, abs(r) > 3)

top_cooks <- kommuner_pred_DFBETAS %>%
  arrange(desc(D)) %>%
  slice(1:6)

# Plot studentized residuals vs fitted values
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = r)) +
  geom_point(size = 2) +  
  geom_point(data = top_cooks, aes(color = "Top Cook's D"), size = 4) + 
  geom_text(data = top_cooks, aes(label = Kommun), vjust = -1.5, color = "red") + 
  geom_point(data = filter(kommuner_pred_DFBETAS, abs(r) > 3 & !(Kommun %in% top_cooks$Kommun)),
             aes(color = "|r*|>3"), size = 3) + 
  geom_text(data = filter(kommuner_pred_DFBETAS, abs(r) > 3),
            aes(label = Kommun), vjust = 1.5, color = "blue", size = 3) +  
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
  labs(title = "Studentized residuals vs fitted values",
       subtitle = "Highlighting municipalities with high influence",
       caption = "Horizontal lines at y = ±2, ±3",
       color = "Highlight") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("Top Cook's D" = "red", "|r*|>3" = "blue"))

high_residuals <- filter(kommuner_pred_DFBETAS, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_DFBETAS, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs fitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))


################################################################################
# 3(d). Explain, exclude, refit. 
remove_municipalities <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
                           "2523 Gällivare", "2514 Kalix", "2584 Kiruna",
                           "1480 Göteborg", "1761 Hammarö", "1484 Lysekil",
                           "1494 Lidköping", "1882 Askersund", "2284 Örnsköldsvik",
                           "0319 Älvkarleby", "1460 Bengtsfors",  "1781 Kristinehamn", 
                           "2262 Timrå",  "0980 Gotland", "1272 Bromölla",  
                           "1885 Lindesberg", "1764 Grums")

#remove_municipalities <- c("0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
#                           "2523 Gällivare", "1480 Göteborg", "2584 Kiruna")
newdata <- kommuner %>%
  filter(!Kommun %in% remove_municipalities)
kommuner_excl_lm <- update(model_2e, data = newdata)
kommuner_pred <- mutate(newdata,
                        yhat = predict(kommuner_excl_lm),
                        r = rstudent(kommuner_excl_lm),
                        v = hatvalues(kommuner_excl_lm),
                        D = cooks.distance(kommuner_excl_lm))
kommuner_pred_excl <- mutate(
  kommuner_pred,
  df0 = dfbetas(kommuner_excl_lm)[, "(Intercept)"],
  df1 = dfbetas(kommuner_excl_lm)[, "log(Vehicles)"],
  df2 = dfbetas(kommuner_excl_lm)[, "log(Higheds)"],
  df3 = dfbetas(kommuner_excl_lm)[, "Children"],
  df4 = dfbetas(kommuner_excl_lm)[, "log(Income)"],
  df5 = dfbetas(kommuner_excl_lm)[, "log(GRP)"],
  fit = predict(kommuner_excl_lm),
  r = rstudent(kommuner_excl_lm),
  D = cooks.distance(kommuner_excl_lm))

# Get municipality with high residuals
high_residuals_excl <- filter(kommuner_pred_excl, abs(r) > 3)

# Plot sqrt(|r*|) against fitted values and label the points where |r*| > 3.
ggplot(kommuner_pred_excl, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 2) +
  geom_point(data = high_residuals_excl, aes(color = "|r*|>3"), size = 4) +
  geom_text(data = high_residuals_excl, aes(label = Kommun), vjust = 2, color = "blue", size = 3) +  
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = "dashed") +
  labs(title = "sqrt(|r*|) vs fitted values",
       subtitle = "Analysis of variance stabilization",
       caption = "Reference lines at y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom") +
  scale_color_manual(values = c("|r*|>3" = "red"))

# summary & confint of both models
summary(kommuner_excl_lm)
confint(kommuner_excl_lm)

summary(model_2e)
confint(model_2e)

################################################################################
# 3(e). Variable selection.

# prepare models for stepwise selection
model_null <- lm(log(PM10) ~ 1, data = newdata)
model_null_sum <- summary(model_null)

model_1b <- lm(log(PM10) ~ log(Vehicles), data = newdata)
model_1b_sum <- summary(model_1b)

model_2c <- lm(log(PM10)~ log(Vehicles) + NewParts, data=newdata)
model_2c_sum <- summary(model_2c)

model_3d <- update(model_2e, data = newdata)
model_3d_sum <- summary(model_3d)

# AIC stepwise selection
step_model_aic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,  # trace=TRUE detail information for every steps
                       k = 2)  # k=2 means using AIC

summary(step_model_aic)


# BIC stepwise selection
step_model_bic <- step(model_1b,
                       scope = list(lower = model_null, upper = model_3d),
                       direction = "both",
                       trace = TRUE,
                       k =  log(nobs(model_3d)))  # BIC
summary(step_model_bic)

# Gathering statistics for each model
model_stats <- function(model) {
  data.frame(
    Beta_Parameters = length(coef(model)),  # Number of beta parameters
    Residual_SD = sigma(model),  # Residual Standard Deviation
    R_Squared = summary(model)$r.squared,  # R-squared
    Adjusted_R_Squared = summary(model)$adj.r.squared,  # Adjusted R-squared
    AIC = AIC(model),  # AIC
    BIC = BIC(model)  # BIC
  )
}

# Apply function to each model and combine results
model_comparison <- data.frame(
  Model = c("Null", "Model 1(b)", "Model 2(c)", "Model 3(d)", "AIC Model", "BIC Model"),
  rbind(
    model_stats(model_null),
    model_stats(model_1b),
    model_stats(model_2c),
    model_stats(model_3d),
    model_stats(step_model_aic),
    model_stats(step_model_bic)
  )
)

# Print the comparison table
print(model_comparison)