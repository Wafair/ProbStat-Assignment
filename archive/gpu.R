install.packages("tidyverse")
library(tidyverse)
library(dplyr)    # alternatively, this also loads %>%
install.packages("VIM")
library(VIM)

#DATA READING
#access work space
setwd("F:/OneDrive - hcmut.edu.vn/HK232/XSTK/BTL")
#import .csv file
gpu_data <- read.csv("All_GPUs.csv") 

gpu_data <- gpu_data[c("Architecture","Core_Speed","L2_Cache","Manufacturer","Integrated","Max_Power","Memory","Memory_Bandwidth","Memory_Bus","Memory_Speed","Memory_Type","PSU")]

#L2_Cache convert
L2_IntConvert <- function(input_string) {
  pattern <- "^([0-9]+)KB(?:\\(x([0-9]+)\\))?$"
  
  # Initialize the result vector
  result <- vector("numeric", length(input_string))
  
  # Iterate over each input string
  for (i in seq_along(input_string)) {
    # Get the current string
    current_string <- input_string[i]
    # Check for NA input
    if (is.na(current_string)) {
      result[i] <- NA
      next
    }
    # Perform regex search and extract matches
    matches <- regmatches(current_string, regexec(pattern, current_string))[[1]]
    # If no match, set result to NA
    if (length(matches) == 0) {
      result[i] <- NA
      next
    }
    
    # Extract the numeric part and optional multiplier
    numeric_value <- as.integer(matches[2])
    multiplier <- if (length(matches) > 2 && matches[3] != "") as.integer(matches[3]) else 1
    
    result[i] <- numeric_value * multiplier
  }
  
  return(result)
}

#DATA CLEANING
#standardize variables with units
gpu_data$Core_Speed <- as.integer(gsub("\\D","",gpu_data$Core_Speed))
gpu_data$Max_Power <- as.integer(gsub("\\D","",gpu_data$Max_Power))
gpu_data$Memory <- as.integer(gsub("\\D","",gpu_data$Memory))
gpu_data$Memory_Bandwidth <- as.numeric(gsub("[^0-9.]+","",gpu_data$Memory_Bandwidth))
gpu_data$Memory_Bus <- as.integer(gsub("\\D","",gpu_data$Memory_Bus))
gpu_data$Memory_Speed <- as.integer(gsub("\\D","",gpu_data$Memory_Speed))
gpu_data$L2_Cache <- L2_IntConvert(gpu_data$L2_Cache)

gpu_data$PSU_Watt <- str_extract(gpu_data$PSU,"\\d+(?= Watt)") %>% as.integer()
gpu_data$PSU_Amps <- str_extract(gpu_data$PSU, "\\d+(?= Amps)") %>% as.integer()

gpu_data <- gpu_data[,-11]

instances = nrow(gpu_data)
colname <- names(gpu_data)

gpu_data[gpu_data == ""]<- NA
missing_count <- colSums(is.na(gpu_data)) * 100 / instances
print(missing_count)

missing_data <- data.frame(feature = colname, non_missing = 100 - missing_count, missing = missing_count)

missing_data %>%
  pivot_longer(-feature) %>%
  ggplot( aes(x = feature, y = value, fill = name)) +
  geom_col(position = position_stack(vjust=1, reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.3)) + 
  xlab("Features") +
  ylab("Percentages") +
  labs(fill = NULL)


gpu_KNN <- kNN(gpu_data, k = 5)
View(gpu_KNN)
