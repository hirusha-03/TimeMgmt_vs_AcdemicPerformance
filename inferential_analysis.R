
#phase 4: Inferential analysis - Hypothesis testing

library(dplyr)
library(car)

model_data <- read.csv("E:/ITProject/TPSM/Statistically_Optimized_Data.csv")

# 1 INFERENTIAL TESTS

print("--- 1. Pearson Correlation (Study Hours vs CGPA) ---")

# Testing the strict 1-on-1 relationship
pearson_test <- cor.test(model_data$Study_Hours_Daily, 
                         model_data$Current_CGPA, 
                         method = "pearson")
print(pearson_test)

print("--- 2. Independent T-Test (Group Comparison) ---")

# Creating groups: High Studiers vs Low Studiers
t_test_data <- model_data %>%
  mutate(
    Study_Group = ifelse(Study_Hours_Daily > 3, "High (>3 hrs)", "Low (<=3 hrs)")
  )

# Testing if the CGPA difference between groups is significant
t_test_result <- t.test(Current_CGPA ~ Study_Group, data = t_test_data)
print(t_test_result)

# 2 Multiple linear regression

# Testing all time management variables simultaneously
mlr_model <- lm(Current_CGPA ~ Study_Hours_Daily + Social_Media_Hours + 
                  Skill_Dev_Hours + Study_Sessions_Daily, 
                data = model_data)

# Extracting the P-values and Adjusted R-squared
model_summary <- summary(mlr_model)
print(model_summary)


# 3 MODEL VALIDATION

print("--- 4. Variance Inflation Factor (VIF) ---")

# Proving independent variables are not overlapping
vif_values <- vif(mlr_model)
print(vif_values)




#  MODEL DIAGNOSTICS & VALIDATION

print("Generating Model Diagnostic Plots...")

# The par() function splits'Plots' window into a 2x2 grid
# 4 diagnostic checks on a single screen.
par(mfrow = c(2, 2))

# Plotting the model automatically generates the 4 core diagnostic graphs
plot(mlr_model)
# Reset the plot window back to a normal 1x1 view for future graphs
par(mfrow = c(1, 1))







