# This script analyzes tries to extract an inactivity threshold using gaussian mixture models on ACC data
# the input is a dataframe with a log VeDBA values which are based on burst sampling regime. The goal is to determine threshold values where the probability of belonging to the leftmost
# distribution is below 25%. These thresholds are used to identify inactive states for each individual.
# see https://journal.r-project.org/articles/RJ-2023-043/ for information on gaussian mixture models in R 

library(mixtools)
library(ggplot2)

# the file can be found in -- cross_sleep\working\Data\VeDBA_Seq\baboons
thres_value <- 0.1 # prob of being sampled from the left gaussian 
d1 <- readRDS("DATA\\baboon_vedba_example.RDS")

# Initialize a vector to store the threshold values
individuals <- unique(d1$individual_local_identifier)
thresholds <- numeric(length(individuals))

# Loop over each individual and fit a Gaussian mixture model
for (i in seq_along(individuals)) {
  ind <- individuals[i]
  
  # Subset the data for the current individual
  data_subset <- d1[d1$individual_local_identifier == ind, ]
  
  # Remove rows with NA values in the log_vedba column
  data_subset <- data_subset[!is.na(data_subset$log_vedba), ]
  
  # Standardize the data
  data_subset$log_vedba <- scale(data_subset$log_vedba)
  
  # Check if the data subset is valid
  if (nrow(data_subset) < 2) {
    cat("Skipping individual", ind, "due to insufficient data.\n")
    next
  }
  
  # Fit a Gaussian mixture model with 2 components
  fit <- tryCatch({
    normalmixEM(data_subset$log_vedba, k = 2, arbmean = TRUE, arbvar = TRUE)
  }, error = function(e) {
    cat("Error fitting model for individual", ind, ":", e$message, "\n")
    return(NULL)
  })
  
  # Skip if the model fitting failed
  if (is.null(fit)) next
  
  # Identify the leftmost and rightmost distributions
  sorted_indices <- order(fit$mu)
  leftmost_index <- sorted_indices[1]
  rightmost_index <- sorted_indices[2]
  
  # Calculate the posterior probabilities for the leftmost and rightmost components
  posterior_probs_leftmost <- fit$posterior[, leftmost_index]
  posterior_probs_rightmost <- fit$posterior[, rightmost_index]
  
  # Add the probabilities to the data frame
  data_subset$prob_leftmost <- posterior_probs_leftmost
  data_subset$prob_rightmost <- posterior_probs_rightmost
  

  # Calculate the leftmost point where the probability of being from the left distribution is under 25%
  threshold <- min(data_subset$log_vedba[
      data_subset$log_vedba > max(data_subset$log_vedba[data_subset$prob_leftmost > thres_value]) &
      data_subset$prob_leftmost < thres_value])
  
  # Store the threshold values
  thresholds[i] <- threshold
  
  # Create the plot
  plot <- ggplot(data_subset, aes(x = log_vedba)) +
    geom_histogram(aes(y = ..density..), bins = 100, fill = "blue", alpha = 0.5) +
    geom_point(aes(y = prob_leftmost, color = "Leftmost"), size = 1, alpha = 0.7) +
    geom_point(aes(y = prob_rightmost, color = "Rightmost"), size = 1, alpha = 0.7) +
    scale_color_manual(values = c("Leftmost" = "red", "Rightmost" = "green")) +
    labs(title = paste("Density Plot of log_vedba for Individual", ind),
         x = "Standardized log_vedba",
         y = "Density / Probability") +
    theme_minimal() +
    geom_vline(xintercept = threshold, linetype = "dotted", color = "blue", size = 2) +
    annotate("text", x = threshold, y = 0, label = paste("Threshold:", round(threshold, 2)), vjust = -1, color = "blue")
  # Print the plot
  print(plot)
}

# Create a data frame for the thresholds
thresholds_df <- data.frame(
  individual_local_identifier = individuals,
  threshold = thresholds
)

# Save the thresholds to a CSV file
write.csv(thresholds_df, "inactive_thresholds.csv", row.names = FALSE)
