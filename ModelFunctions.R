
library(tidyverse)
library(tidymodels)
library(readr)

#' @title Run a Complete Regression Analysis with a User-Defined Model
#' @description This function takes a file path, a target variable, and a tidymodels
#'   model specification, then performs a full machine learning workflow: data
#'   splitting, preprocessing, hyperparameter tuning, and final model evaluation.
#' @param model_spec A tidymodels parsnip model specification (e.g., from `boost_tree()`
#'   or `rand_forest()`). The model should have its mode set to "regression".
#' @param file_path A string representing the path to the input CSV file.
#' @param target_variable A string with the name of the column to be used as the
#'   outcome or dependent variable (e.g., "y").
#' @param train_prop The proportion of data for the initial training set. Defaults to 0.6.
#' @param validation_prop The proportion of data for the validation set. Defaults to 0.2.
#' @param tune_grid_size The number of hyperparameter combinations to try. Defaults to 20.
#' @param seed An integer to set the random seed for reproducibility. Defaults to 123.
#' @return A list containing three elements:
#'   \item{final_metrics}{A tibble with the performance metrics (RMSE, R-squared, MAE)
#'     on the held-out test set.}
#'   \item{best_hyperparameters}{A tibble with the best hyperparameters found
#'     during tuning, selected based on RMSE.}
#'   \item{final_fitted_workflow}{The final, fitted workflow object, which can be used
#'     for making predictions on new data.}
#' @examples
#' # First, create a dummy CSV file for demonstration
#' # set.seed(42)
#' # sample_data <- data.frame(
#' #   x1 = rnorm(200),
#' #   x2 = rnorm(200),
#' #   x3 = sample(letters[1:4], 200, replace = TRUE),
#' #   y = rnorm(200, mean = 10)
#' # )
#' # write.csv(sample_data, "sample_data.csv", row.names = FALSE)
#'
#' # --- Example 1: XGBoost Model ---
#' # xgb_spec <- boost_tree(
#' #   trees = tune(),
#' #   tree_depth = tune(),
#' #   learn_rate = 0.1
#' # ) |>
#' #   set_engine("xgboost") |>
#' #   set_mode("regression")
#'
#' # xgb_results <- run_ml_analysis(
#' #   model_spec = xgb_spec,
#' #   file_path = "sample_data.csv",
#' #   target_variable = "y"
#' # )
#' # print(xgb_results$final_metrics)
#'
#' # --- Example 2: Random Forest Model ---
#' # rf_spec <- rand_forest(
#' #   mtry = tune(),
#' #   trees = 1000,
#' #   min_n = tune()
#' # ) |>
#' #   set_engine("ranger") |>
#' #   set_mode("regression")
#'
#' # rf_results <- run_ml_analysis(
#' #   model_spec = rf_spec,
#' #   file_path = "sample_data.csv",
#' #   target_variable = "y"
#' # )
#' # print(rf_results$final_metrics)

run_ml_analysis <- function(model_spec,
                            file_path,
                            target_variable,
                            train_prop = 0.6,
                            validation_prop = 0.2,
                            tune_grid_size = 20,
                            seed = 123) {


  tryCatch({
    df <- read_csv(file_path, show_col_types = FALSE)
  }, error = function(e) {
    stop("Error reading the file. Please check if the file_path is correct.")
  })

  if (!target_variable %in% names(df)) {
    stop(paste("The target variable '", target_variable, "' was not found in the data.", sep = ""))
  }

  target_sym <- rlang::sym(target_variable)


  set.seed(seed)
  split_obj <- initial_validation_split(
    df,
    strata = !!target_sym,
    prop = c(train_prop, validation_prop)
  )
  cat("Data splitting complete.\n")
  cat("Training set size:", nrow(training(split_obj)), "\n")
  cat("Validation set size:", nrow(validation(split_obj)), "\n")
  cat("Test set size:", nrow(testing(split_obj)), "\n\n")


  rec <- recipe(!!target_sym ~ ., data = training(split_obj)) |>
    step_rm(all_predictors(), -all_numeric()) |>
    step_dummy(all_nominal(), one_hot = TRUE) |>
    step_zv(all_predictors()) |>
    step_corr(all_numeric(), threshold = 0.9) |>
    step_normalize(all_numeric())

  
  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model_spec) 

  
  set.seed(seed)
  grid <- grid_latin_hypercube(
    extract_parameter_set_dials(wf),
    size = tune_grid_size
  )

  val_rs <- validation_set(split_obj)

  cat("Starting hyperparameter tuning...\n")
  tune_res <- tune_grid(
    wf,
    resamples = val_rs,
    grid      = grid,
    metrics   = metric_set(rmse, rsq, mae)
  )
  cat("Tuning complete.\n\n")

  best_params <- select_best(tune_res, "rmse")
  cat("Best hyperparameters selected:\n")
  print(best_params)
  cat("\n")

  
  final_wf <- finalize_workflow(wf, best_params)

  
  final_split_prop <- train_prop + validation_prop
  final_split <- initial_split(df, prop = final_split_prop, strata = !!target_sym)

  cat("Fitting final model and evaluating on the test set...\n")
  final_fit <- last_fit(
    final_wf,
    final_split
  )
  cat("Final evaluation complete.\n\n")

  final_metrics <- collect_metrics(final_fit)
  cat("Final performance metrics on the test set:\n")
  print(final_metrics)
  cat("\n")

  
  return(
    list(
      final_metrics = final_metrics,
      best_hyperparameters = best_params,
      final_fitted_workflow = final_fit
    )
  )
}
