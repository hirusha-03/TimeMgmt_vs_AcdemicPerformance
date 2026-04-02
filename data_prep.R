# 1. LOAD THE DATASET
raw_data <- read.csv("E:/ITProject/TPSM/Students_Performance_data_set.csv")

# 2. CLEAN MISSING VALUES 
clean_data <- na.omit(raw_data)
