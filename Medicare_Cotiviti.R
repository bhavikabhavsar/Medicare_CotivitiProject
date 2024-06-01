#Step 1: Loading the necessary libraries
# Loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(synthpop)
library(ggplot2)
library(dplyr)

# Step 2: Data Exploration and Cleaning
# Loading the dataset
data <- read.csv("C:/Users/Bhavika Bhavsar/Downloads/NH_QualityMsr_Claims_May2024.csv")


# Inspecting the data
glimpse(data)
summary(data)

# Handling missing values
# Identifying missing values
colSums(is.na(data))

# Imputing missing numerical values with the mean
Data <- data %>%
  mutate(
    Adjusted.Score = ifelse(is.na(Adjusted.Score), mean(Adjusted.Score, na.rm = TRUE), Adjusted.Score),
    Observed.Score = ifelse(is.na(Observed.Score), mean(Observed.Score, na.rm = TRUE), Observed.Score),
    Expected.Score = ifelse(is.na(Expected.Score), mean(Expected.Score, na.rm = TRUE), Expected.Score)
  )

# Imputing missing categorical values
Data$Footnote.for.Score[is.na(data$Footnote.for.Score)] <- "No Footnote"

# Removing duplicates and standardizing formats
DATA <- Data %>% distinct()

# Standardizing text columns
Final_Data <- DATA %>%
  mutate(
    Provider.Name = toupper(Provider.Name),
    City.Town = toupper(City.Town),
    State = toupper(State)
  )

# Step 3: Data Analysis and Visualization
# Statistical Analysis

# Summary statistics
summary(Final_Data)

# Correlation analysis
cor(Final_Data %>% select_if(is.numeric))
    
    # Visualization
    # Histogram of Adjusted Scores
    ggplot(Final_Data, aes(x = Adjusted.Score)) + 
      geom_histogram(binwidth = 1, fill = "darkblue", color = "red") +
      theme_minimal() +
      labs(title = "Distribution of Adjusted Scores", x = "Adjusted Score", y = "Frequency")
    
    # Violin Plot of Adjusted Scores by Resident Type
    ggplot(Final_Data, aes(x = Resident.type, y = Adjusted.Score)) + 
      geom_violin(trim = FALSE, fill = "darkblue", color = "purple") +
      theme_minimal() +
      labs(title = "Violin Plot of Adjusted Scores by Resident Type", x = "Resident Type", y = "Adjusted Score")
    
    # Step 4: Generative AI Application
    # Defining a function to generate synthetic data
    generate_synthetic_data <- function(Final_Data, num_samples = 1000, noise_factor = 0.1) {
      num_features <- ncol(Final_Data)
      synthetic_data <- matrix(NA, nrow = num_samples, ncol = num_features)
      
      for (i in 1:num_samples) {
        # Randomly sample from the original data
        sample_row <- sample(1:nrow(Final_Data), 1)
        original_sample <- as.numeric(Final_Data[sample_row, ])  # Convert to numeric
        
        # Add random noise
        noise <- rnorm(num_features, 0, noise_factor)
        
        # Convert noise to numeric if needed
        noise <- as.numeric(noise)
        
        synthetic_data[i, ] <- original_sample + noise
      }
      
      return(synthetic_data)
    }
    
    # Generate synthetic data based on the statistical properties of the original data
    synthetic_data <- generate_synthetic_data(Final_Data, num_samples = 1000, noise_factor = 0.1)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    