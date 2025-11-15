cross_validate_turnover_model <- function(data, 
                                          outcome_var = "turnover_count",
                                          k_folds = 5,
                                          optimizer = "bobyqa",
                                          max_iterations = 2e5,
                                          seed = 123,
                                          verbose = TRUE) {
  
  # Load required libraries
  if (!require(lme4)) stop("lme4 package required")
  if (!require(dplyr)) stop("dplyr package required")
  if (!require(pROC)) cat("Warning: pROC package recommended for AUC calculation\n")
  
  set.seed(seed)
  
  # Define required variables
  required_vars <- c(
    outcome_var, "x", "y", "distance_ball_moved", "ball_movement_speed", 
    "percent_distance", "pass.angle", "pressing_count_1", "pressing_count_2", "pressing_count_3",
    "play_pattern.name", "pass.type.name", 
    "right_option", "front_option", "left_option", "back_option",
    "player.name", "match_id", "position_group", "team.name" , "x_end", "y_end"
  )
  
  # Check required variables
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Clean data
  clean_data <- data %>%
    select(all_of(required_vars)) %>%
    na.omit()
  
  if (verbose) {
    cat("=== 5-FOLD CROSS-VALIDATION ===\n")
    cat("Total observations:", nrow(clean_data), "\n")
    cat("Outcome distribution:\n")
    print(table(clean_data[[outcome_var]]))
    cat("\n")
  }
  
  # Create folds (stratified by outcome)
  n_obs <- nrow(clean_data)
  
  # Stratified sampling to ensure balanced folds
  outcome_0 <- which(clean_data[[outcome_var]] == 0)
  outcome_1 <- which(clean_data[[outcome_var]] == 1)
  
  folds_0 <- split(sample(outcome_0), rep(1:k_folds, length.out = length(outcome_0)))
  folds_1 <- split(sample(outcome_1), rep(1:k_folds, length.out = length(outcome_1)))
  
  # Combine folds
  folds <- list()
  for (i in 1:k_folds) {
    folds[[i]] <- c(folds_0[[i]], folds_1[[i]])
  }
  
  # Storage for results
  fold_results <- list()
  all_predictions <- data.frame()
  
  # Define numeric variables for scaling
  numeric_vars <- c("x", "y", "distance_ball_moved", "ball_movement_speed", 
                    "percent_distance", "pass.angle", "pressing_count_1", 
                    "pressing_count_2", "pressing_count_3")
  
  # Cross-validation loop
  for (fold in 1:k_folds) {
    if (verbose) cat("Processing Fold", fold, "of", k_folds, "...\n")
    
    # Split data
    test_indices <- folds[[fold]]
    train_indices <- setdiff(1:n_obs, test_indices)
    
    train_data <- clean_data[train_indices, ]
    test_data <- clean_data[test_indices, ]
    
    if (verbose) {
      cat("  Train set:", nrow(train_data), "obs |")
      cat(" Test set:", nrow(test_data), "obs\n")
    }
    
    # Scale training data and get scaling parameters
    scaling_params <- list()
    train_data_scaled <- train_data
    
    for (var in numeric_vars) {
      scaling_params[[var]] <- list(
        mean = mean(train_data[[var]], na.rm = TRUE),
        sd = sd(train_data[[var]], na.rm = TRUE)
      )
      
      train_data_scaled[[paste0(var, "_scaled")]] <- 
        (train_data[[var]] - scaling_params[[var]]$mean) / scaling_params[[var]]$sd
    }
    
    # Apply same scaling to test data
    test_data_scaled <- test_data
    for (var in numeric_vars) {
      test_data_scaled[[paste0(var, "_scaled")]] <- 
        (test_data[[var]] - scaling_params[[var]]$mean) / scaling_params[[var]]$sd
    }
    
    # Build model formula
    scaled_predictors <- paste0(numeric_vars, "_scaled")
    factor_predictors <- c("as.factor(play_pattern.name)", "as.factor(pass.type.name)",
                           "as.factor(right_option)", "as.factor(front_option)", 
                           "as.factor(left_option)", "as.factor(back_option)")
    all_predictors <- c(scaled_predictors, factor_predictors)
    random_effects <- "(1 | player.name) + (1 | match_id) + (1 | position_group)"
    
    formula_str <- paste(outcome_var, "~", paste(all_predictors, collapse = " + "), "+", random_effects)
    model_formula <- as.formula(formula_str)
    
    # Fit model on training data
    tryCatch({
      fold_model <- glmer(
        formula = model_formula,
        data = train_data_scaled,
        family = "binomial",
        control = glmerControl(optimizer = optimizer, optCtrl = list(maxfun = max_iterations))
      )
      
      # Make predictions on test data
      pred_probs <- predict(fold_model, newdata = test_data_scaled, type = "response", allow.new.levels = TRUE)
      pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
      
      # Calculate metrics
      actual <- test_data[[outcome_var]]
      
      # Accuracy
      accuracy <- mean(pred_classes == actual)
      
      # Confusion matrix components
      tp <- sum(pred_classes == 1 & actual == 1)
      tn <- sum(pred_classes == 0 & actual == 0)
      fp <- sum(pred_classes == 1 & actual == 0)
      fn <- sum(pred_classes == 0 & actual == 1)
      
      # Additional metrics
      precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
      recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
      specificity <- ifelse(tn + fp > 0, tn / (tn + fp), 0)
      f1 <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
      
      # AUC (if pROC available)
      auc_value <- NA
      if (require(pROC, quietly = TRUE)) {
        tryCatch({
          auc_value <- as.numeric(auc(actual, pred_probs))
        }, error = function(e) auc_value <<- NA)
      }
      
      # Store fold results
      fold_results[[fold]] <- list(
        fold = fold,
        n_train = nrow(train_data),
        n_test = nrow(test_data),
        converged = fold_model@optinfo$conv$opt == 0,
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        specificity = specificity,
        f1 = f1,
        auc = auc_value,
        confusion_matrix = matrix(c(tn, fp, fn, tp), nrow = 2, 
                                  dimnames = list(Actual = c("0", "1"), Predicted = c("0", "1")))
      )
      
      # Store individual predictions
      fold_predictions <- data.frame(
        fold = fold,
        actual = actual,
        predicted_prob = pred_probs,
        predicted_class = pred_classes,
        row_index = test_indices
      )
      
      all_predictions <- rbind(all_predictions, fold_predictions)
      
      if (verbose) {
        cat("    Accuracy:", round(accuracy, 3), 
            "| AUC:", round(ifelse(is.na(auc_value), 0, auc_value), 3), 
            "| Converged:", fold_model@optinfo$conv$opt == 0, "\n")
      }
      
    }, error = function(e) {
      if (verbose) cat("    ERROR in fold", fold, ":", e$message, "\n")
      fold_results[[fold]] <- list(fold = fold, error = e$message)
    })
  }
  # Calculate overall metrics
  if (verbose) cat("\n=== CROSS-VALIDATION RESULTS ===\n")
  
  # Extract successful fold metrics
  successful_folds <- fold_results[sapply(fold_results, function(x) is.null(x$error))]
  
  if (length(successful_folds) == 0) {
    cat("ERROR: No folds completed successfully!\n")
    return(NULL)
  }
  
  # Summary statistics
  accuracies <- sapply(successful_folds, function(x) x$accuracy)
  precisions <- sapply(successful_folds, function(x) x$precision)
  recalls <- sapply(successful_folds, function(x) x$recall)
  f1_scores <- sapply(successful_folds, function(x) x$f1)
  aucs <- sapply(successful_folds, function(x) x$auc)
  aucs <- aucs[!is.na(aucs)]
  
  summary_stats <- data.frame(
    Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "AUC"),
    Mean = c(mean(accuracies), mean(precisions), mean(recalls), mean(f1_scores), 
             ifelse(length(aucs) > 0, mean(aucs), NA)),
    SD = c(sd(accuracies), sd(precisions), sd(recalls), sd(f1_scores),
           ifelse(length(aucs) > 0, sd(aucs), NA)),
    Min = c(min(accuracies), min(precisions), min(recalls), min(f1_scores),
            ifelse(length(aucs) > 0, min(aucs), NA)),
    Max = c(max(accuracies), max(precisions), max(recalls), max(f1_scores),
            ifelse(length(aucs) > 0, max(aucs), NA))
  )
  
  if (verbose) {
    cat("Summary Statistics:\n")
    # Fix for the printing error
    summary_display <- summary_stats
    summary_display[, -1] <- round(summary_stats[, -1], 3)
    print(summary_display)
    cat("\nSuccessful folds:", length(successful_folds), "out of", k_folds, "\n")
  }
  
  # Overall confusion matrix (aggregated)
  overall_confusion <- table(Actual = all_predictions$actual, 
                             Predicted = all_predictions$predicted_class)
  
  if (verbose) {
    cat("\nOverall Confusion Matrix:\n")
    print(overall_confusion)
    
    # Overall AUC
    if (require(pROC, quietly = TRUE) && nrow(all_predictions) > 0) {
      tryCatch({
        overall_auc <- auc(all_predictions$actual, all_predictions$predicted_prob)
        cat("Overall AUC:", round(overall_auc, 3), "\n")
      }, error = function(e) {
        cat("Could not calculate overall AUC\n")
      })
    }
  }
  
  # Create detailed results by fold
  fold_summary <- do.call(rbind, lapply(successful_folds, function(x) {
    data.frame(
      fold = x$fold,
      n_train = x$n_train,
      n_test = x$n_test,
      accuracy = x$accuracy,
      precision = x$precision,
      recall = x$recall,
      specificity = x$specificity,
      f1 = x$f1,
      auc = x$auc,
      converged = x$converged
    )
  }))
  
  # CREATE xTURNOVER DATASET - This is the key addition!
  if (verbose) cat("\nCreating xTurnover dataset...\n")
  
  # Start with original clean data
  xTurnover_dataset <- clean_data
  
  # Initialize new columns
  xTurnover_dataset$xTurnover <- NA
  xTurnover_dataset$predicted_turnover <- NA
  xTurnover_dataset$cv_fold <- NA
  xTurnover_dataset$risk_category <- NA
  xTurnover_dataset$correct_prediction <- NA
  
  # Fill in predictions for each observation
  for (i in 1:nrow(all_predictions)) {
    row_idx <- all_predictions$row_index[i]
    xTurnover_dataset$xTurnover[row_idx] <- all_predictions$predicted_prob[i]
    xTurnover_dataset$predicted_turnover[row_idx] <- all_predictions$predicted_class[i]
    xTurnover_dataset$cv_fold[row_idx] <- all_predictions$fold[i]
    xTurnover_dataset$correct_prediction[row_idx] <- 
      (xTurnover_dataset[[outcome_var]][row_idx] == all_predictions$predicted_class[i])
  }
  
  # Add risk categories based on xTurnover probabilities
  xTurnover_dataset$risk_category <- case_when(
    is.na(xTurnover_dataset$xTurnover) ~ "No Prediction",
    xTurnover_dataset$xTurnover < 0.3 ~ "Low Risk",
    xTurnover_dataset$xTurnover < 0.7 ~ "Medium Risk",
    TRUE ~ "High Risk"
  )
  
  # Add risk categories based on xTurnover probabilities using 2 standard deviations
  xTurnover_mean <- mean(xTurnover_dataset$xTurnover, na.rm = TRUE)
  xTurnover_sd <- sd(xTurnover_dataset$xTurnover, na.rm = TRUE)
  high_risk_threshold <- xTurnover_mean + (2 * xTurnover_sd)
  
  xTurnover_dataset$risk_category <- case_when(
    is.na(xTurnover_dataset$xTurnover) ~ "No Prediction",
    xTurnover_dataset$xTurnover >= high_risk_threshold ~ "High Risk",
    TRUE ~ "Non-High Risk"
  )
  
  if (verbose) {
    cat("xTurnover dataset created with", nrow(xTurnover_dataset), "observations\n")
    cat("xTurnover mean:", round(xTurnover_mean, 4), "\n")
    cat("xTurnover standard deviation:", round(xTurnover_sd, 4), "\n")
    cat("High-risk threshold (mean + 2*SD):", round(high_risk_threshold, 4), "\n")
    cat("Risk category distribution:\n")
    print(table(xTurnover_dataset$risk_category))
    
    # Show prediction accuracy by risk category
    accuracy_by_risk <- xTurnover_dataset %>%
      filter(!is.na(xTurnover)) %>%
      group_by(risk_category) %>%
      summarise(
        n_passes = n(),
        actual_turnover_rate = mean(.data[[outcome_var]], na.rm = TRUE),
        predicted_turnover_rate = mean(xTurnover, na.rm = TRUE),
        accuracy = mean(correct_prediction, na.rm = TRUE),
        .groups = 'drop'
      )
    
    cat("\nAccuracy by Risk Category:\n")
    # Fix: Only round the numeric columns
    accuracy_display <- accuracy_by_risk
    accuracy_display[, -1] <- round(accuracy_by_risk[, -1], 3)
    print(accuracy_display)
  }
  
  # Return comprehensive results
  results <- list(
    summary_stats = summary_stats,
    fold_details = fold_summary,
    fold_results = fold_results,
    all_predictions = all_predictions,
    overall_confusion_matrix = overall_confusion,
    xTurnover_dataset = xTurnover_dataset,  # This is the key output!
    successful_folds = length(successful_folds),
    total_folds = k_folds,
    parameters = list(
      k_folds = k_folds,
      optimizer = optimizer,
      max_iterations = max_iterations,
      seed = seed
    )
  )
  
  return(results)
}

# Usage example:
# cv_results <- cross_validate_turnover_model(subset_data, verbose = TRUE)
# xTurnover_data <- cv_results$xTurnover_dataset
# write.csv(xTurnover_data, "dataset_with_xTurnover.csv", row.names