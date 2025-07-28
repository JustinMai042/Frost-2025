

library(pmlbr)
library(tidyverse)
library(reticulate)
library(OpenML)
library(dplyr)


simulated_patterns <- c(
  "^fri_", 
  "^feynman_", 
  "^first_principles_", 
  "^strogatz_", 
  "^nikuradse_"
)


regex_pattern <- paste(simulated_patterns, collapse = "|")


all_regression_names <- regression_datasets()


simulated_dataset_names <- grep(regex_pattern, all_regression_names, value = TRUE)


simulated_datasets_list <- list()

cat("Fetching simulated datasets \n")


for (dataset_name in simulated_dataset_names) {
  cat(paste(" -> Fetching:", dataset_name, "\n"))
  simulated_datasets_list[[dataset_name]] <- pmlbr::fetch_data(dataset_name)
}

cat(paste("\n Fetched", length(simulated_datasets_list), "simulated datasets.\n"))




cat("\n Fetching OpenML-CTR23 Datasets \n")
openml_datasets_list <- list()

tryCatch({
  py_run_string("import openml; suite = openml.study.get_suite(353)")
  task_ids <- py$suite$tasks

  for (tid in task_ids) {
    cat(paste(" -> Fetching OpenML task ID:", tid, "\n"))
    tryCatch({
      task <- getOMLTask(as.integer(tid))
      dataset_name <- task$input$data.set$desc$name
      
      if (dataset_name %in% names(openml_datasets_list)) {
        dataset_name <- paste0(dataset_name, "_", tid)
      }
      
      openml_datasets_list[[dataset_name]] <- task$input$data.set$data
    }, error = function(e) {
      cat(paste(" Error fetching task", tid, ":", e$message, "\n"))
    })
  }
  cat(paste("\n Fetched", length(openml_datasets_list), "OpenML-CTR23 datasets.\n"))
  
}, error = function(e) {
  cat(" Could not fetch OpenML suite.\n")
  cat(e$message, "\n")
})

cat(paste("\n All", length(openml_datasets_list), "datasets have been fetched into the 'openml_datasets_list'.\n"))



if (!exists("openml_datasets_list") || !is.list(openml_datasets_list)) {
  stop(" 'openml_datasets_list' was not found.")
}
if (!exists("simulated_datasets_list") || !is.list(simulated_datasets_list)) {
  stop(" 'simulated_datasets_list' was not found.")
}

all_datasets <- c(openml_datasets_list, simulated_datasets_list)

filtered_datasets_list <- list()

for (name in names(all_datasets)) {
  df <- all_datasets[[name]]
  
  condition1 <- nrow(df) < 10000
  condition2 <- ncol(df) >= 1  && ncol(df) <= 50000000
  condition3 <- !any(is.na(df))
  
  if (condition1 && condition2 && condition3) {
    filtered_datasets_list[[name]] <- df
  }
}

cat(paste("Original number of datasets:", length(all_datasets), "\n"))
cat(paste("Number of datasets after filtering:", length(filtered_datasets_list), "\n"))

print(names(filtered_datasets_list))
