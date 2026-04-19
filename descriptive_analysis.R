
#phase 3 : descriptive analysis
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

clean_data <- read.csv("E:/ITProject/TPSM/Final_Model_Ready_Performance.csv")

#1. Central Tendencies & Spread
summary_stats <- clean_data %>%
  summarise(
    Avg_CGPA = mean(Current_CGPA, na.rm = TRUE),
    SD_CGPA = sd(Current_CGPA, na.rm = TRUE),
    
    Avg_Study_Hours = mean(Study_Hours_Daily, na.rm = TRUE),
    SD_Study_Hours = sd(Study_Hours_Daily, na.rm = TRUE)
  )

print("--- Step 1: Summary Statistics ---")
print(summary_stats)

# 2 Univariate Visualization (Histogram)

#  Histogram for Dependent Variable (Y = Current_CGPA)
cgpa_dist <- ggplot(clean_data, aes(x = Current_CGPA)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 20) +
  theme_minimal() +
  labs(
    title = "Distribution of Current CGPA (Dependent Variable Y)",
    x = "Current CGPA",
    y = "Number of Students"
  )
print(cgpa_dist)

#Histogram for study hours
study_histogram <- ggplot(clean_data, aes(x = Study_Hours_Daily)) +
  geom_histogram(fill = "forestgreen", color = "black", bins = 15) +
  theme_minimal() +
  labs(
    title = "Distribution of Daily Study Hours",
    x = "Hours Studied per Day",
    y = "Frequency"
  )

print(study_histogram)


#3 Bivariate Visualizations (Scatter Plots)

scatter_data <- clean_data %>%
  select(Current_CGPA, Study_Hours_Daily, Social_Media_Hours, Skill_Dev_Hours, Study_Sessions_Daily) %>%
  pivot_longer(cols = -Current_CGPA, names_to = "Variable", values_to = "Hours")

multi_scatter <- ggplot(scatter_data, aes(x = Hours, y = Current_CGPA, color = Variable)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) + 
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Step 3: Independent Variables vs CGPA",
    x = "Hours Spent",
    y = "Current CGPA"
  ) +
  theme(legend.position = "none")

print(multi_scatter)

# 4 Correlation Matrix & Noise Reduction
# Goal: Mathematically prove relationships and drop variables with r < 0.05.

numeric_data <- clean_data %>% select(where(is.numeric))

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")


# Visualize the correlation matrix (Highly recommended for presentations)
print("Generating Correlation Plot...")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.7, 
         title = "Pearson Correlation Matrix", mar=c(0,0,1,0))

# Extract correlations specifically related to CGPA
cgpa_correlations <- cor_matrix[, "Current_CGPA"]

#  Identify weak variables (correlation between -0.05 and 0.05)
weak_vars <- names(cgpa_correlations[abs(cgpa_correlations) < 0.05])

#Protection List for core hypothesis variables
protected_vars <- c("Study_Hours_Daily", "Study_Sessions_Daily", 
                    "Social_Media_Hours", "Skill_Dev_Hours")

weak_vars_to_drop <- setdiff(weak_vars, protected_vars)

print("--- Step 4: Variables Dropped Due to Low Correlation (r < 0.05) ---")
print(weak_vars_to_drop)

# Drop ONLY the unprotected weak variables
model_ready_data <- clean_data %>%
  select(-all_of(weak_vars_to_drop))

# Save the final dataset
write.csv(model_ready_data, "E:/ITProject/TPSM/Statistically_Optimized_Data.csv", row.names = FALSE)





