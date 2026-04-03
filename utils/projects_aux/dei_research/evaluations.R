# ==============================================================================
# Setup and Data Loading
# ==============================================================================

# Install necessary packages if not already installed
if (!require("caret")) install.packages("caret")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tools")) install.packages("tools")

library(caret)
library(dplyr)
library(tools)

# 1. Load Data
df <- read.csv("labels.csv", check.names = FALSE, stringsAsFactors = FALSE)
df <- labels
# Select relevant columns
cols <- c("gemini", "openAI-nano", "openAI-35", "human")
df <- df[, cols]

# 2. Preprocessing
# Fill NAs with "Unknown", trim whitespace, and standardize capitalization
df[is.na(df)] <- "Unknown"
for(col in cols) {
  df[[col]] <- toTitleCase(trimws(df[[col]]))
}

# Create a unified set of factor levels to ensure confusion matrices work later
all_levels <- sort(unique(unlist(df)))

# Convert all columns to factors with the same levels
for(col in cols) {
  df[[col]] <- factor(df[[col]], levels = all_levels)
}

# ==============================================================================
# 1. Correlation Analysis (Cramér's V) - CORRECTED
# ==============================================================================

# Function to calculate Cramér's V
calc_cramers_v <- function(x, y) {
  # FIX: Drop unused levels for this specific pair to avoid zero-sum rows/cols
  x <- droplevels(x)
  y <- droplevels(y)
  
  # Create contingency table
  tbl <- table(x, y)
  
  # Check dimensions
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl))
  
  # If there's less than 2 categories in common, correlation is 0
  if (k < 2) return(0)
  
  # Chi-squared statistic
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE))$statistic
  
  # Formula: V = sqrt(chi2 / (n * (k - 1)))
  v <- sqrt(chi2 / (n * (k - 1)))
  return(as.numeric(v))
}

# Compute Correlation Matrix
cramer_matrix <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(cols, cols))

for(i in cols) {
  for(j in cols) {
    if(i == j) {
      cramer_matrix[i, j] <- 1.0
    } else {
      cramer_matrix[i, j] <- calc_cramers_v(df[[i]], df[[j]])
    }
  }
}

cat("\n--- 1. Cramér's V Correlation Matrix ---\n")
print(round(cramer_matrix, 3))

# ==============================================================================
# 2. Classification Metrics
# ==============================================================================
# (This part remains the same as it benefits from the global factor levels)

models <- c("gemini", "openAI-nano", "openAI-35")
ground_truth <- df$human

metrics_df <- data.frame(
  Model = character(),
  Accuracy = numeric(),
  Precision_Weighted = numeric(),
  Recall_Weighted = numeric(),
  F1_Weighted = numeric(),
  stringsAsFactors = FALSE
)

gt_support <- table(ground_truth)

for(model in models) {
  pred <- df[[model]]
  
  cm <- confusionMatrix(data = pred, reference = ground_truth, mode = "prec_recall")
  acc <- cm$overall["Accuracy"]
  by_class <- cm$byClass
  by_class[is.na(by_class)] <- 0
  
  class_names <- gsub("Class: ", "", rownames(by_class))
  weights <- gt_support[class_names]
  weights[is.na(weights)] <- 0 
  
  prec_w <- weighted.mean(by_class[, "Precision"], w = weights)
  rec_w  <- weighted.mean(by_class[, "Recall"],    w = weights)
  f1_w   <- weighted.mean(by_class[, "F1"],        w = weights)
  
  metrics_df[nrow(metrics_df) + 1, ] <- c(
    model, 
    round(acc, 4), 
    round(prec_w, 4), 
    round(rec_w, 4), 
    round(f1_w, 4)
  )
}

cat("\n--- 2. Classification Metrics (Human as Ground Truth) ---\n")
print(metrics_df)

# ==============================================================================
# 3. Match Percentage per Ethnicity (Recall per Class)
# ==============================================================================

recall_matrix <- matrix(
  NA, 
  nrow = length(all_levels), 
  ncol = length(models), 
  dimnames = list(all_levels, models)
)

for(model in models) {
  cm <- confusionMatrix(df[[model]], ground_truth, mode = "prec_recall")
  by_class <- cm$byClass
  by_class[is.na(by_class)] <- 0
  class_rows <- gsub("Class: ", "", rownames(by_class))
  recall_matrix[class_rows, model] <- by_class[, "Recall"]
}

valid_gt_classes <- names(gt_support)[gt_support > 0]
recall_matrix_filtered <- recall_matrix[valid_gt_classes, ]

cat("\n--- 3. Match Percentage per Ethnicity (Recall) ---\n")
print(round(recall_matrix_filtered * 100, 2))

# ==============================================================================
# Generate Overall Match Percentage (Accuracy) Table
# ==============================================================================


# 3. Compute Accuracy
models <- c("gemini", "openAI-nano", "openAI-35")
ground_truth <- df$human

# Create an empty dataframe for results
accuracy_table <- data.frame(
  Model = character(),
  Accuracy = character(), # Character type to hold the "%" string
  stringsAsFactors = FALSE
)

# Loop through each model to calculate accuracy
for(model_col in models) {
  # Calculate simple accuracy: (Number of Exact Matches) / (Total Rows)
  acc_value <- mean(df[[model_col]] == ground_truth)
  
  # Format names for the final table (Mapping CSV headers to Display names)
  display_name <- switch(model_col,
                         "gemini"      = "Gemini",
                         "openAI-nano" = "OpenAI-Nano",
                         "openAI-35"   = "OpenAI-3.5"
  )
  
  # Format as percentage with 1 decimal place
  acc_percent <- paste0(sprintf("%.1f", acc_value * 100), "%")
  
  # Append to dataframe
  accuracy_table[nrow(accuracy_table) + 1, ] <- c(display_name, acc_percent)
}

# 4. Print the Final Table
cat("\n--- 3.2. Overall Match Percentage (Accuracy) ---\n")
print(accuracy_table, row.names = FALSE)
