---
  title: "Lecture 4"
output: html_document
#Linda 
Author: "Linda" 
---
  {r setup, include=FALSE}

library(tidyverse)
library(pROC)
library(lubridate)
library(broom)

#defining read_and_clean function
read_and_clean <- function(f){
  df <- read_csv(f, skip = 1) # Skip the first row
  df <- df[,-1] # Remove the first column
  col_names <- read_csv(f, n_max=0) %>% names() # Retrieve the column names as a separate csv file
  names(df) <- col_names
  return(df)
}

#defining retrieve_data function 
retrieve_data <- function(data_loc){
  files <- list.files(data_loc, full.names = TRUE)
  list_df <- map(files, read_and_clean)
  df <- bind_rows(list_df)
  return(df)
}

df <- retrieve_data("~/Documents/ESDA/Energy_Data_Analysis/BENV0091-Linda/occupancy")

head(df)

### Plotting

df_clean <- df %>%
  select(Occupancy, Humidity, CO2, Temperature) %>%
  mutate(Occupancy = as.factor(Occupancy))

df %>% 
  filter(month(as_date(date)) == 2) %>% 
  ggplot(aes(x = date, y = Occupancy, fill = Occupancy)) + 
  geom_line(size = 1, color = 'darkgreen') + 
  theme_bw()

#Set seed and split train/test data
set.seed(456)
train_idx <- sample(1:nrow(df_clean), 0.8*nrow(df_clean))
train <- df_clean[train_idx,]
test <- df_clean[-train_idx,]

train %>%
  pivot_longer(Humidity:Temperature,
               names_to = 'variable') %>% 
  ggplot(aes(x = Occupancy, y = value)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = 'free_y')


log_reg <- glm(Occupancy ~ ., data = train, family = 'binomial')


test_pred <- test %>% 
  mutate(pred = predict(log_reg, newdata = test, type = 'response'),
         pred_class = as.integer(pred > 0.5),
         correct = pred_class == Occupancy) 

classification_rate <- mean(test_pred$correct)
print(classification_rate)
plot(roc(test_pred$Occupancy, test_pred$pred), print.auc = TRUE)

conf_matrix <- table(test_pred$pred_class, test_pred$Occupancy) # True class (columns); predicted class (rows)

conf_matrix

conf_matrix / colSums(conf_matrix)

count(train, Occupancy)

downsample_by_occupancy <- function(df){
  ## Function for downsampling a data frame to even out class membership 
  ## (specifically by occupancy)
  
  # Determine the majority class
  class_counts <- count(df, Occupancy)
  majority_class <- class_counts$Occupancy[which.max(class_counts$n)]
  
  # Calculate difference in observations between majority/minority classes (how many rows to remove)
  n_to_downsample <- abs(diff(class_counts$n))
  
  # Add rownames
  df <- df %>% rownames_to_column()
  
  # Sample rows from majority class to remove
  to_remove <- df %>% 
    filter(Occupancy == majority_class) %>%
    sample_n(n_to_downsample)
  
  # Use anti_join to remove those rows
  downsampled_df <- df %>% anti_join(to_remove)
  
  # Remove rownames
  downsampled_df <- downsampled_df %>% select(-rowname)
  
  return(downsampled_df)
}


train %>% 
  downsample_by_occupancy() %>% 
  count(Occupancy)

set.seed(2021)

# Downsample the data
train_downsampled <- train %>% 
  downsample_by_occupancy() 

# Function for standardising a data frame relative to training data 
standardise_df <- function(df, training_df){
  # For loop over columns
  for (col in names(df)){
    # Check if column is numeric (not applicable to character, factor etc.)
    if (is.numeric(df[[col]])){
      mu <- mean(training_df[[col]]) # mean of vector in training data
      sigma <- sd(training_df[[col]]) # sd of vector in training data
      df[[col]] <- (df[[col]] - mu) / sigma # standardise
    }
  }
  return(df)
}

train_stand <- standardise_df(train_downsampled, train_downsampled)

# Fit the model to the processed data
log_reg <- glm(Occupancy ~ ., data = train_stand, family = 'binomial')

# Standardise the test data, with reference to the (downsampled) training data
test_stand <- standardise_df(test, train_downsampled)

# Add predictions
test_stand <- test_stand %>% 
  mutate(pred = predict(log_reg, newdata = test_stand, type = 'response'),
         pred_class = as.integer(pred > 0.5),
         correct = pred_class == Occupancy)

# Create confusion matrix
conf_matrix <- table(test_stand$pred_class, test_stand$Occupancy) 
conf_matrix / colSums(conf_matrix)

classification_rate <- mean(test_stand$correct)
print(classification_rate)

# Plot the ROC
plot(roc(test_stand$Occupancy, test_pred$pred), print.auc = TRUE)

# Plot the coefficients
tidy(log_reg) %>%
  ggplot(aes(x = term, 
             y = estimate, 
             ymin = estimate - std.error,
             ymax = estimate + std.error)) + 
  geom_pointrange()

