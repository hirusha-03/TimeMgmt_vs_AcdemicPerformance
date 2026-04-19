
# Install the tidyverse package if you haven't already:
install.packages("tidyverse")

# Load required libraries
library(dplyr)
library(readr)
library(stringr)

# 1. Load the data
df <- read.csv("E:/ITProject/TPSM/Students_Performance_data_set.csv")

# 2. Rename all columns to clean snake_case
colnames(df) <- c(
  "Admission_Year", "Gender", "Age", "HSC_Year", "Program", "Current_Semester",
  "Has_Scholarship", "Uses_Uni_Transport", "Study_Hours_Daily", 
  "Study_Sessions_Daily", "Learning_Mode", "Uses_Smartphone", "Has_PC",
  "Social_Media_Hours", "English_Proficiency", "Attendance_Percentage",
  "Fell_Probation", "Got_Suspension", "Attends_Consultancy", "Skills",
  "Skill_Dev_Hours", "Interested_Area", "Relationship_Status",
  "Has_CoCurriculars", "Living_With", "Has_Health_Issues", "Previous_SGPA",
  "Has_Disabilities", "Current_CGPA", "Credits_Completed", "Family_Income"
)

# 3. Clean the Attendance column (Remove '%' and make numeric)
df$Attendance_Percentage <- as.numeric(gsub("%", "", df$Attendance_Percentage))

# 4. Drop any rows with missing values
clean_data <- na.omit(df)

# 5. Convert text columns to factors for statistical modelling
character_columns <- sapply(clean_data, is.character)
clean_data[character_columns] <- lapply(clean_data[character_columns], as.factor)

# 6. Save the clean dataset
write.csv(clean_data, "E:/ITProject/TPSM/Cleaned_Students_Performance.csv", row.names = FALSE)

# Display the structure of the clean dataset to verify
glimpse(clean_data)
str(df)

