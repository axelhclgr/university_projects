#set directory
setwd("C:/Users/axelc/Documents/R")
getwd()

#read library
library(dplyr)
library(ggplot2)
library(tidyr)
library(emmeans)
library(tidyverse)
library(knitr)
library(car)
library(broom)
library(pROC)
library(gtable)
library(performance)
library(scales)
library(viridis)
library(caret)

#get dataset
naval_raw <- read.table("data.txt")
naval_raw

#manually assigning the colnames for features.txt to naval row
#Lever Position (lp), Ship Speed (v), Gas Turbine Shaft Torque (GTT),
#Gas Turbine Rate of Revolutions (GTn), Gas Generator Rate of Revolution (GGn), 
#Starboard Propeller Torque (Ts), Port Propeller Torque (Tp),
#HP Turbine exit temperature (T48), GT Compressor inlet air temperature (T1),
#GT Compressor outlet air temperature (T2), HP Turbine exit pressure (P48),
#GT Compressor inlet air pressure (P1), GT Compressor outlet air pressure (P2),
#Gas Turbine exhaust gas pressure (Pexh), Turbine Injecton Control (TIC), 
#Fuel flow (mf), GT Compressor decay state coefficient, 
#GT Turbine decay state coefficient
column_names <- c("lp", "v", "GTT", "GTn", "GGn", "Ts", "Tp", "T48", "T1", 
                  "T2", "P48", "P1", "P2", "Pexh", "TIC", "mf", "GCDSC", "GTTDSC")
colnames(naval_raw) <- column_names

#create row id
naval_raw$Observation_ID <- seq.int(nrow(naval_raw))
naval_raw <- naval_raw[, c("Observation_ID", column_names)]

#feature engineering
naval_raw$Speed_Regime <- factor(naval_raw$v, 
                                 levels = c(3, 6, 9, 12, 15, 18, 21, 24, 27), 
                                 labels = c("V3", "V6", "V9", "V12", "V15", "V18", "V21", "V24", "V27"))

#temperature ratio
naval_raw$T_P_ratio <- naval_raw$T48 / naval_raw$P48

#distribution of decay
mean_gcdsc <- mean(naval_raw$GCDSC)
mean_gcdsc
median_gcdsc <- median(naval_raw$GCDSC)
median_gcdsc
std_gcdsc <- sd(naval_raw$GCDSC)
std_gcdsc

ggplot(naval_raw, aes(x = GCDSC)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_gcdsc), color = "darkred", linewidth = 1) +
  geom_vline(aes(xintercept = median_gcdsc), color = "orange", linewidth = 1.2, linetype = "dotdash") +
  labs( 
    title = "Distribution of Compressor Decay",
    subtitle = paste("Mean =", round(mean_gcdsc, 2), "(Red Line) | Median =", (median_gcdsc), "(Green Dashed Line)"),
    x = "Compressor Decay Coefficient (GCDSC)",
    y = "Frequency",
    caption = "Values > 0.975 classified as 'Healthy'; Values <= 0.975 as 'Worn'"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14), axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), plot.subtitle = element_text(size=10))

#create of healthy and worn
naval_raw$Status <- ifelse(naval_raw$GCDSC > 0.975, "Healthy", "Worn")

#create dummy variable
naval_raw$status_dummy <- ifelse(naval_raw$Status == "Worn", 1, 0)

#status count
worn_count <- naval_raw %>%
  count(Status == "Healthy")
worn_count

#selecting variables
variables_fuel <- naval_raw[, c("v", "GCDSC", "T_P_ratio", "GTTDSC", "GGn", "GTT", "GTn")]

#summary
summary(variables_fuel)

#select variable for correlation
model_variables_correlation <- function(speed_label) { 
  sub_data <- naval_raw %>%  
    filter(Speed_Regime == speed_label) 
  
  #linear regression model
  fuel_prediction_model <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = sub_data)
    
  #correlation between variables
  collinearity_model <- check_collinearity(fuel_prediction_model)
  
  #col name
  coll_df <- as.data.frame(collinearity_model)
  
  #results list
  output <- list(
    Speed = speed_label,
    Keep = coll_df[coll_df$VIF < 10, 1],
    Drop = coll_df[coll_df$VIF >= 10, ],
    Full_Table = coll_df
  )
  
  return(output)
}

#looking at the results
results_v3 <- model_variables_correlation("V3")
print(results_v3$Keep)
print(results_v3$Drop)

results_v15 <- model_variables_correlation("V15")
print(results_v15$Keep)
print(results_v15$Drop)

results_v27 <- model_variables_correlation("V27")
print(results_v27$Keep)
print(results_v27$Drop)

#get the full model summaries GCDSC
final_model_v3 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V3")))
final_model_v6 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V6")))
final_model_v9 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V9")))
final_model_v12 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V12")))
final_model_v15 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V15")))
final_model_v18 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V18")))
final_model_v21 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V21")))
final_model_v24 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V24")))
final_model_v27 <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = (naval_raw %>% filter(Speed_Regime == "V27")))

#view of the coefficient and p-values
summary(final_model_v3)
summary(final_model_v6)
summary(final_model_v9)
summary(final_model_v12)
summary(final_model_v15)
summary(final_model_v18)
summary(final_model_v21)
summary(final_model_v24)
summary(final_model_v27)

#group all data and predict
naval_fuel_prediction <- naval_raw %>%
  group_by(Speed_Regime) %>%
  mutate(Predicted = exp(predict(lm(log(mf) ~ log(GCDSC) + log(GGn))))) %>%
  ungroup()
naval_fuel_prediction

#r-squared for each speed regime
r2_labels <- naval_fuel_prediction %>%
  group_by(Speed_Regime) %>%
  summarize(r2 = cor(mf, Predicted)^2, x_pos = min(mf), y_pos = max(Predicted)) %>%
  mutate(label = paste0("R² =", round(r2, 4)))
r2_labels

#plotting the predictions
ggplot(naval_fuel_prediction, aes(x = mf, y = Predicted)) +
  geom_point(alpha = 0.5, color = "darkgreen", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_text(data = r2_labels, aes(x = x_pos, y = y_pos, label = label), 
            hjust = 0, vjust = 1, size = 3, fontface = "italic") +
  facet_wrap(~ Speed_Regime, scales = "free") +
  labs( 
    title = "Predictive Robustness Across Speed Regimes",
    subtitle = "Actual vs. Predicted Fuel Flow",
    x = "Actual Fuel Flow",
    y = "Predicted Fuel Flow") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#residuals
naval_fuel_prediction <- naval_fuel_prediction %>%
  mutate(Residuals = mf - Predicted)
naval_fuel_prediction

#plotting residuals
ggplot(naval_fuel_prediction, aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.4, color = "blue", size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ Speed_Regime, scales = "free") +
  labs(
    title = "Residual Diagnostics Across Speed Regimes",
    subtitle = "Checking for patterns in errors",
    x = "Predicted Fuel Flow",
    y = "Residuals (Actual - Predicted)"
    ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#cook's distance
fuel_cook <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  model <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = sub_data)
  
  temp_influence <- data.frame(
    Observation_ID = sub_data$Observation_ID,
    Speed = speed,
    Cooks_distance = cooks.distance(model),
    Leverage = hatvalues(model)
  )
  
  fuel_cook <- rbind(fuel_cook, temp_influence)
}
fuel_cook

#plot cook
ggplot(fuel_cook, aes(x = Observation_ID, y = Cooks_distance, color = Leverage)) +
  geom_hline(yintercept = 4 / nrow(naval_raw), color = "red", linetype = "dashed") +
  geom_segment(aes(xend = Observation_ID, yend = 0), color = "darkred", alpha = 0.6) +
  scale_color_gradient(low = "darkred", high = "orange") +
  facet_wrap(~ Speed, scales = "free_x") +
  labs(
    title = "Cook's Distance: Identifying Influential Points", 
    subtitle = "Points above the red line (4/n) may be outliers", 
    x = "Observation ID", 
    y = "Cook's Distance"
    ) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#linear regression formula
for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  sub_data$GGn_scaled <- sub_data$GGn / mean(sub_data$GGn)
  model <- lm(log(mf) ~ log(GCDSC) + log(GGn_scaled), data = sub_data)
  coeffs <- coef(model)
  
  #formula
  cat(paste0("\nFormula for ", speed, ":\n"))
  cat(sprintf("mf = %.4f * (GCDSC^%.4f) * (GGn_scaled^%.4f)", exp(coeffs[1]), coeffs[2], coeffs[3]))
}

#final significance and model performance summary
model_summary_table <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  model <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = sub_data)
  
  #get f-statistic and p-value from ANOVA
  anova_f <- anova(model)
  
  temp_summary <- data.frame(
    Speed = speed,
    R_Squared = summary(model)$r.squared,
    F_Statistic = anova_f$F[1],
    p_value = anova_f$`Pr(>F)`[1],
    RMSE = sqrt(mean(residuals(model)^2))
  )
  
  model_summary_table <- rbind(model_summary_table, temp_summary)
}

#display of the summary table
kable(model_summary_table, digits = 6, caption = "Final Model Significance by Speed Regime")

#decay impact
decay_impact <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  m <- lm(log(mf) ~ log(GCDSC) + log(GGn), data = sub_data)
  decay_impact <- rbind(decay_impact, data.frame(Speed = speed, Impact = coef(m)[2]))
}

decay_impact$Speed <- factor(decay_impact$Speed, 
                             levels = c("V3", "V6", "V9", "V12", "V15", "V18", "V21", "V24", "V27"))

#visualization of decay impact
ggplot(decay_impact, aes(x = Speed, y = Impact)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = round(Impact, 2)), vjust = 1.5, color = "white", size = 3) +
  labs(
    title = "Economic Impact of Compressor Decay",
    subtitle = "Coefficient magnitude: How much fuel is lost per unit of health decay",
    x = "Ship Speed Regime",
    y = "GCDSC Coefficient (Impact on Fuel)"
    ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#running model using GTTDSC decay
#data engineering for GTTDSC
naval_raw$Specific_torque <- naval_raw$GTT / naval_raw$GTn

#select variable for correlation
model_gttdsc_correlation <- function(speed_label) { 
  sub_data <- naval_raw %>%  
    filter(Speed_Regime == speed_label) 
  
  #linear regression model
  fuel_prediction_model_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = sub_data)
  
  #correlation between variables
  collinearity_model_gttdsc <- check_collinearity(fuel_prediction_model_gttdsc)
  
  #col name
  coll_df_gttdsc <- as.data.frame(collinearity_model_gttdsc)
  
  #results list
  output <- list(
    Speed = speed_label,
    Keep = coll_df_gttdsc[coll_df_gttdsc$VIF < 10, 1],
    Drop = coll_df_gttdsc[coll_df_gttdsc$VIF >= 10, ],
    Full_Table = coll_df_gttdsc
  )
  
  return(output)
}

#looking at the results
results_v3_gttdsc <- model_gttdsc_correlation("V3")
print(results_v3_gttdsc$Keep)
print(results_v3_gttdsc$Drop)

results_v15_gttdsc <- model_gttdsc_correlation("V15")
print(results_v15_gttdsc$Keep)
print(results_v15_gttdsc$Drop)

results_v27_gttdsc <- model_gttdsc_correlation("V27")
print(results_v27_gttdsc$Keep)
print(results_v27_gttdsc$Drop)

#get the full model summaries GTTDSC
final_model_v3_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V3")))
final_model_v6_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V6")))
final_model_v9_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V9")))
final_model_v12_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V12")))
final_model_v15_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V15")))
final_model_v18_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V18")))
final_model_v21_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V21")))
final_model_v24_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V24")))
final_model_v27_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = (naval_raw %>% filter(Speed_Regime == "V27")))

#view of the coefficient and p-values for GTTDSC
summary(final_model_v3_gttdsc)
summary(final_model_v6_gttdsc)
summary(final_model_v9_gttdsc)
summary(final_model_v12_gttdsc)
summary(final_model_v15_gttdsc)
summary(final_model_v18_gttdsc)
summary(final_model_v21_gttdsc)
summary(final_model_v24_gttdsc)
summary(final_model_v27_gttdsc)

#group all data and predict GTTDSC
naval_fuel_prediction_gttdsc <- naval_raw %>%
  group_by(Speed_Regime) %>%
  mutate(Predicted_GTTDSC = exp(predict(lm(log(mf) ~ log(GTTDSC) + log(Specific_torque))))) %>%
  ungroup()
naval_fuel_prediction_gttdsc

#r-squared for each speed regime
r2_labels_gttdsc <- naval_fuel_prediction_gttdsc %>%
  group_by(Speed_Regime) %>%
  summarize(r2_GTTDCS = cor(mf, Predicted_GTTDSC)^2, x_pos = min(mf), y_pos = max(Predicted_GTTDSC)) %>%
  mutate(label = paste0("R² =", round(r2_GTTDCS, 4)))
r2_labels_gttdsc

#plotting the predictions GTTDSC
ggplot(naval_fuel_prediction_gttdsc, aes(x = mf, y = Predicted_GTTDSC)) +
  geom_point(alpha = 0.5, color = "darkgreen", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_text(data = r2_labels_gttdsc, aes(x = x_pos, y = y_pos, label = label), 
            hjust = 0, vjust = 1, size = 3, fontface = "italic") +
  facet_wrap(~ Speed_Regime, scales = "free") +
  labs( 
    title = "Predictive Robustness Across Speed Regimes for GTTDSC",
    subtitle = "Actual vs. Predicted Fuel Flow",
    x = "Actual Fuel Flow",
    y = "Predicted Fuel Flow") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#residuals GTTDSC
naval_fuel_prediction_gttdsc <- naval_fuel_prediction_gttdsc %>%
  mutate(Residuals = mf - Predicted_GTTDSC)
naval_fuel_prediction_gttdsc

#plotting residuals
ggplot(naval_fuel_prediction_gttdsc, aes(x = Predicted_GTTDSC, y = Residuals)) +
  geom_point(alpha = 0.4, color = "blue", size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ Speed_Regime, scales = "free") +
  labs(
    title = "Residual Diagnostics Across Speed Regimes for GTTDSC",
    subtitle = "Checking for patterns in errors",
    x = "Predicted Fuel Flow",
    y = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#cook's distance
fuel_cook_gttdsc <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  model <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = sub_data)
  
  temp_influence <- data.frame(
    Observation_ID = sub_data$Observation_ID,
    Speed = speed,
    Cooks_distance = cooks.distance(model),
    Leverage = hatvalues(model)
  )
  
  fuel_cook_gttdsc <- rbind(fuel_cook_gttdsc, temp_influence)
}
fuel_cook_gttdsc

#plot cook
ggplot(fuel_cook_gttdsc, aes(x = Observation_ID, y = Cooks_distance, color = Leverage)) +
  geom_hline(yintercept = 4 / nrow(naval_raw), color = "red", linetype = "dashed") +
  geom_segment(aes(xend = Observation_ID, yend = 0), color = "darkred", alpha = 0.6) +
  scale_color_gradient(low = "darkred", high = "orange") +
  facet_wrap(~ Speed, scales = "free_x") +
  labs(
    title = "Cook's Distance: Identifying Influential Points for GTTDSC", 
    subtitle = "Points above the red line (4/n) may be outliers", 
    x = "Observation ID", 
    y = "Cook's Distance"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#linear regression formula for GTTDSC
for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  sub_data$Torque_scaled <- sub_data$Specific_torque / mean(sub_data$Specific_torque)
  model_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Torque_scaled), data = sub_data)
  coeffs_gttdsc <- coef(model_gttdsc)
  
  #formula
  cat(paste0("\nGTTDSC Formula for ", speed, ":\n"))
  cat(sprintf("mf = %.4f * (GTTDSC^%.4f) * (Torque_scaled^%.4f)", exp(coeffs_gttdsc[1]), coeffs_gttdsc[2], coeffs_gttdsc[3]))
}

#final significance and model performance summary
model_summary_table_gttdsc <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  model_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = sub_data)
  
  #get f-statistic and p-value from ANOVA
  anova_f_gttdsc <- anova(model_gttdsc)
  
  temp_summary_gttdsc <- data.frame(
    Speed = speed,
    R_Squared = summary(model_gttdsc)$r.squared,
    F_Statistic = anova_f_gttdsc$F[1],
    p_value = anova_f_gttdsc$`Pr(>F)`[1],
    RMSE = sqrt(mean(residuals(model_gttdsc)^2))
  )
  
  model_summary_table_gttdsc <- rbind(model_summary_table_gttdsc, temp_summary_gttdsc)
}

#display of the summary table
kable(model_summary_table_gttdsc, digits = 6, caption = "Final Model Significance by Speed Regime for GTTDSC")

#decay impact
decay_impact_gttdsc <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  m_gttdsc <- lm(log(mf) ~ log(GTTDSC) + log(Specific_torque), data = sub_data)
  decay_impact_gttdsc <- rbind(decay_impact, data.frame(Speed = speed, Impact = coef(m_gttdsc)[2]))
}

decay_impact_gttdsc$Speed <- factor(decay_impact_gttdsc$Speed, 
                             levels = c("V3", "V6", "V9", "V12", "V15", "V18", "V21", "V24", "V27"))

#visualization of decay impact
ggplot(decay_impact_gttdsc, aes(x = Speed, y = Impact)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = round(Impact, 2)), vjust = 1.5, color = "white", size = 3) +
  labs(
    title = "Economic Impact of Compressor Decay for GTTDSC",
    subtitle = "Coefficient magnitude: How much fuel is lost per unit of health decay",
    x = "Ship Speed Regime",
    y = "GCDSC Coefficient (Impact on Fuel)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8), 
        strip.background = element_rect(fill = "whitesmoke"),
        strip.text = element_text(face = "bold"))

#savings model
#define constants
fuel_price_usd_per_kg <- 1.21 #price derived from $3.84/gallon NATO F-76 standard from DLA
health_scenarios <- seq(1.00, 0.94, by = -0.01)

#saving matrix
saving_matrix <- data.frame()

for(speed in levels(naval_raw$Speed_Regime)) {
  sub_data <- subset(naval_raw, Speed_Regime == speed)
  sub_data$GGn_scaled <- sub_data$GGn / mean(sub_data$GGn)
  
  #savings model
  saving_model <- lm(log(mf) ~ log(GCDSC) + log(GGn_scaled), data = sub_data)
  
  intercept <- exp(coef(saving_model)[1])
  beta_health <- coef(saving_model)[2]
  
  for(h in health_scenarios) {
    predicted_mf <- intercept * (h^beta_health)
    
    #converting to hourly metrics
    hourly_kg <- predicted_mf * 3600
    baseline_kg <- intercept * (1.00^beta_health) * 3600
    
    extra_fuel_kg <- hourly_kg - baseline_kg
    hourly_waste_usd <- extra_fuel_kg * fuel_price_usd_per_kg
    
    #store results
    saving_matrix <- rbind(saving_matrix, data.frame(
      Regime = speed,
      GCDSC_level = h,
      Hourly_fuel_kg = hourly_kg,
      Waste_kg_hour = extra_fuel_kg,
      Waste_USD_hour = hourly_waste_usd
    ))
  }
}

#visualization results saving matrix
saving_matrix

#table for penalty for 3% compressor decay
kable(subset(saving_matrix, GCDSC_level == 0.97), digits = 2, 
      caption = "Hourly Financial Penalty for 3% Compressor Decay")

#visualization
ggplot(saving_matrix, aes(x = GCDSC_level, y = Waste_USD_hour, color = Regime, group = Regime)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.8) +
  scale_x_reverse(breaks = seq(1.0, 0.94, by = -0.01)) +
  scale_y_continuous(labels = label_dollar(), n.breaks = 8) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "The Financial Cost of Compressor Decay",
    subtitle = "Hourly USD waste as GCDSC health degrades",
    x = "Compressor Health (GCDSC)",
    y = "Extra Fuel Cost per Hour (USD)",
    color = "Speed Regime",
    caption = "Baseline: GCDSC = 1.0 | Fuel Price: $1.21/kg (DLA FY26 Forecast)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
        strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
        )

#logistic regression to predict the health of the engine
#data engineering
naval_raw <- naval_raw %>%
  group_by(Speed_Regime) %>%
  mutate(
    baseline_mf = mean(mf[Status == "Healthy"]),
    extra_fuel = mf - baseline_mf,
    extra_T_P = T_P_ratio - mean(T_P_ratio[Status == "Healthy"]),
    extra_GGn = GGn - mean(GGn[Status == "Healthy"])
  ) %>%
  ungroup()

#logistic regression model
global_model <- glm(status_dummy ~ extra_fuel + Speed_Regime + extra_GGn, 
                    data = naval_raw, 
                    family = binomial)

#run audit for collinearity
collinearity_check <- check_collinearity(global_model)
print(collinearity_check)

#logistic regression formula
summary(global_model)$coefficients

#odds_ratio
odds_ratios <- exp(coef(global_model))
print(odds_ratios)

#odds ratio table
OR_table <- tidy(global_model, conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),
    Lower_CI = exp(conf.low),
    Upper_CI = exp(conf.high)
  ) %>%
  select(term, OR, Lower_CI, Upper_CI, p.value)

print(round(OR_table[, -1], 4) %>% cbind(term = OR_table$term))

#model coefficients
model_coeffs <- tidy(global_model, conf.int = TRUE)
model_coeffs

#visualization
plot_data <- model_coeffs %>% filter(term != "(Intercept)")

ggplot(plot_data, aes(x = estimate, y =reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, color = "#2c3e50") +
  geom_point(size = 3, color = "#34495e") +
  labs(
    title = "Predictor Importance: Log-Odds of Compressor Decay",
    subtitle = "Confidence intervals not crossing the red line indicate statistical significance",
    x = "Coefficient Estimate (Log-Odds)",
    y = "Engine Sensor / Operational Regime"
  ) +
  scale_y_discrete(labels = c(
    "extra_fuel" = "Fuel Flow Residual", "extra_GGn" = "GG Speed Residual", "Speed_RegimeV6" = "Speed: 6 knots",
    "Speed_RegimeV9" = "Speed: 9 knots", "Speed_RegimeV12" = "Speed: 12 knots", "Speed_RegimeV15" = "Speed: 15 knots",
    "Speed_RegimeV18" = "Speed: 18 knots", "Speed_RegimeV21" = "Speed: 21 knots", "Speed_RegimeV24" = "Speed: 24 knots",
    "Speed_RegimeV27" = "Speed: 27 knots"
  )) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
        strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_line(color = "grey90"),
        plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )

#prediction
naval_raw$probabilities <- predict(global_model, type = "response")

#visualization predictions
ggplot(naval_raw, aes(x = probabilities, fill = factor(status_dummy))) +
  geom_density(alpha = 0.6, color = "white", size = 0.3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  scale_fill_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                    labels = c("0" = "Healthy", "1" = "Worn")) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Class Separation: Predicted Probabilities of Engine Decay",
    subtitle = "Higher separation between peaks indicates a more robust classifier",
    x = "Predicted Probability [Pr(Worn)]",
    y = "Density",
    fill = "Ground Truth",
    caption = "Dashed line indicates the standard 0.5 classification threshold"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )

#c-statistics 
c_global_model <- roc(naval_raw$status_dummy, naval_raw$probabilities)
c_global_model
auc_value <- auc(c_global_model)
auc_value

#roc visualization
ggroc(c_global_model, colour = "#2c3e50", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted", color = "grey50") +
  labs(
    title = "ROC Analysis: Global Classifier Performance",
    subtitle = paste("Model achieved an area under the curve (AUC) of", round(auc_value, 4)),
    x = "Specificity (True Negative Rate)",
    y = "Sensitivity (True Positive Rate)",
    caption = "Baseline (dotted line) represents random classification (AUC = 0.5)"
  ) +
  coord_equal() +
  annotate("text", x = 0.15, y = 0.95, label = "Ideal Performance", 
           color = "#e74c3c", fontface = "italic", size = 3) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )

#confusion matrix
predicted_classes <- factor(ifelse(naval_raw$probabilities > 0.5, 1, 0),
                            levels = c(0, 1),
                            labels = c("Healthy", "Worn"))
actual_classes <- factor(naval_raw$status_dummy,
                         levels = c(0, 1),
                         labels = c("Healthy", "Worn"))

confusion_matrix <- confusionMatrix(predicted_classes, actual_classes, positive = "Worn")
print(confusion_matrix)

#convert caret to dataframe
cm_df <- as.data.frame(confusion_matrix$table)

#create heatmap
ggplot(cm_df, aes(x= Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#34495e", high = "#2c3e50") +
  labs(
    title = "Confusion Matrix: Naval Engine Decay Classification",
    subtitle = paste("Overall Model Accuracy:", round(confusion_matrix$overall['Accuracy'], 4)),
    x = "Predicted Status",
    y = "Actual Status (Ground Truth)",
    caption = "Correct classifications are concentrated on the diagonal"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )

#analysis per regime
regimes_accuracy <- naval_raw %>%
  group_by(Speed_Regime) %>%
  summarize(accuracy = mean(status_dummy == (probabilities > 0.5)))
regimes_accuracy

#visualization
ggplot(regimes_accuracy, aes(x = Speed_Regime, y = accuracy, fill = accuracy)) +
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "#c0392b", linewidth = 0.8) +
  scale_fill_gradient(low = "#5dade2", high = "#2c3e50") +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.1)),
            vjust = -0.8, fontface = "bold", size = 3.5) +
  labs(
    title = "Diagnostic Reliability Across Operational Profiles",
    subtitle = "Performance benchmark against the 90% reliability threshold",
    x = "Operational Speed Regime (V)",
    y = "Classification Accuracy",
    caption = "Note: Lower accuracy at V3-V6 correlates with lower thermodynamic signal-to-noise ratios."
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )

#cost of delay over hypothetical 1000-hour mission cycle
roi_summary <- saving_matrix %>%
  filter(GCDSC_level <= 1.00 & GCDSC_level >= 0.94) %>%
  group_by(Regime) %>%
  summarize(Average_Waste_per_Hour = mean(Waste_USD_hour),
            Max_Waste_per_Hour = max(Waste_USD_hour),
            Potential_Savings_500hr = Max_Waste_per_Hour * 500) %>%
  arrange(desc(Potential_Savings_500hr))

#visualization of ROI
ggplot(roi_summary, aes(x = reorder(Regime, Potential_Savings_500hr), y = Potential_Savings_500hr, fill = Potential_Savings_500hr)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "#34495e", high = "#e74c3c") +
  scale_y_continuous(labels = label_dollar()) +
  coord_flip() +
  labs(
    title = "Phase 3: Fleet Financial ROI Analysis",
    subtitle = "Projected savings per engine per 500 hours of early detection",
    x = "Operational Speed Profile",
    y = "Total Fuel Savings (USD)",
    caption = "Calculated using the validated Logistic Regression threshold (p=0.5)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10), axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill = "whitesmoke"),  strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(), legend.position = "right", legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 0, face = "italic", color = "grey40", size = 10)
  )