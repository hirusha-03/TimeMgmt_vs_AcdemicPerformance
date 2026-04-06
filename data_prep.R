# 1. LOAD THE DATASET
raw_data <- read.csv("E:/ITProject/TPSM/Students_Performance_data_set.csv")

#2. Rename columns
colnames(raw_data) <- c(
  "Admission_Year", "Gender", "Age", "HSC_Year", "Program", "Current_Semester",
  "Has_Scholarship", "Uses_Uni_Transport", "Study_Hours_Daily", 
  "Study_Sessions_Daily", "Learning_Mode", "Uses_Smartphone", "Has_PC",
  "Social_Media_Hours", "English_Proficiency", "Attendance_Percentage",
  "Fell_Probation", "Got_Suspension", "Attends_Consultancy", "Skills",
  "Skill_Dev_Hours", "Interested_Area", "Relationship_Status",
  "Has_CoCurriculars", "Living_With", "Has_Health_Issues", "Previous_SGPA",
  "Has_Disabilities", "Current_CGPA", "Credits_Completed", "Family_Income"
)

# 3. DROP UNUSABLE TEXT COLUMNS
# "Skills" and "Interested_Area" have too many random text answers. 
# "Program" is mostly the same for everyone. We drop them so they don't break the math.
clean_data <- raw_data[, !(names(raw_data) %in% c("Skills", "Interested_Area", "Program"))]

# 4. CLEAN MISSING DATA
clean_data <- na.omit(clean_data)

#ENCOADING
clean_data$Uses_Uni_Transport <- ifelse(clean_data$Uses_Uni_Transport == "Yes", 1, 0)
clean_data$Uses_Smartphone <- ifelse(clean_data$Uses_Smartphone == "Yes", 1, 0)
clean_data$Has_PC <- ifelse(clean_data$Has_PC == "Yes", 1, 0)
clean_data$Fell_Probation <- ifelse(clean_data$Fell_Probation == "Yes", 1, 0)
clean_data$Got_Suspension <- ifelse(clean_data$Got_Suspension == "Yes", 1, 0)
clean_data$Attends_Consultancy <- ifelse(clean_data$Attends_Consultancy == "Yes", 1, 0)
clean_data$Has_CoCurriculars <- ifelse(clean_data$Has_CoCurriculars == "Yes", 1, 0)
clean_data$Has_Health_Issues <- ifelse(clean_data$Has_Health_Issues == "Yes", 1, 0)
clean_data$Has_Disabilities <- ifelse(clean_data$Has_Disabilities == "Yes", 1, 0)
clean_data$Has_Scholarship <- ifelse(clean_data$Has_Scholarship == "Yes", 1, 0)

# Custom Variable Mappings
clean_data$isMale <- ifelse(clean_data$Gender == "Male", 1, 0)
clean_data$Learning_Mode_isOffline <- ifelse(clean_data$Learning_Mode == "Offline", 1, 0)
clean_data$Is_Single <- ifelse(clean_data$Relationship_Status == "Single", 1, 0)
clean_data$Is_bachelor <- ifelse(clean_data$Living_With == "Bachelor", 1, 0)

# Ordinal Mapping for English (1=Basic, 2=Intermediate, 3=Advance)
clean_data$English_Proficiency <- ifelse(clean_data$English_Proficiency == "Basic", 1,
                                         ifelse(clean_data$English_Proficiency == "Intermediate", 2, 3))

# 6. DROP ORIGINAL TEXT COLUMNS 
# columns for Gender, Mode, Relationship, and Living
final_data <- clean_data[, !(names(clean_data) %in% c("Gender", "Learning_Mode", "Relationship_Status", "Living_With"))]

# Ensure every single column is strictly numeric before running the correlation
final_data[] <- lapply(final_data, as.numeric)

# 7. CORRELATION MATRIX
cor_matrix <- cor(final_data)
# Extract ONLY the relationships with Current_CGPA
cgpa_cor <- cor_matrix[, "Current_CGPA"]

# 8. PRINT THE RESULTS
print("=== FEATURE RANKING: IMPACT ON CGPA ===")
print(sort(cgpa_cor, decreasing = TRUE))

