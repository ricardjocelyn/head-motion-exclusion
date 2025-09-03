# =============================================================================
# ABCD Head Motion Exclusion Project - Master Data Processing Pipeline
# =============================================================================

# This script runs the complete data processing pipeline by executing
# all the refactored data processing scripts in sequence.

cat("=== ABCD Head Motion Exclusion Project ===\n")
cat("Master Data Processing Pipeline\n")
cat("Started at:", Sys.time(), "\n\n")

# =============================================================================
# Pipeline Configuration
# =============================================================================

# Define the pipeline steps
pipeline_steps <- c(
  "code/01a_data_loading.R",
  "code/01b_data_cleaning.R", 
  "code/01c_data_integration.R",
  "code/01d_data_quality.R"
)

# Define step descriptions
step_descriptions <- c(
  "Data Loading and Validation",
  "Data Cleaning Operations",
  "Data Integration and Merging",
  "Data Quality Assessment"
)

# =============================================================================
# Pipeline Execution Functions
# =============================================================================

# Function to execute a pipeline step
execute_step <- function(step_file, step_description, step_number) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("STEP", step_number, ":", step_description, "\n")
  cat("Executing:", step_file, "\n")
  cat("Started at:", Sys.time(), "\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  # Check if step file exists
  if (!file.exists(step_file)) {
    stop("Step file not found:", step_file)
  }
  
  # Execute the step
  start_time <- Sys.time()
  
  tryCatch({
    source(step_file, echo = TRUE)
    end_time <- Sys.time()
    duration <- difftime(end_time, start_time, units = "secs")
    
    cat("\n", rep("-", 80), "\n", sep = "")
    cat("✓ STEP", step_number, "COMPLETED SUCCESSFULLY\n")
    cat("Duration:", round(duration, 1), "seconds\n")
    cat("Completed at:", end_time, "\n")
    cat(rep("-", 80), "\n\n", sep = "")
    
    return(list(
      step = step_number,
      file = step_file,
      description = step_description,
      status = "SUCCESS",
      start_time = start_time,
      end_time = end_time,
      duration = duration
    ))
    
  }, error = function(e) {
    end_time <- Sys.time()
    duration <- difftime(end_time, start_time, units = "secs")
    
    cat("\n", rep("!", 80), "\n", sep = "")
    cat("✗ STEP", step_number, "FAILED\n")
    cat("Error:", e$message, "\n")
    cat("Duration:", round(duration, 1), "seconds\n")
    cat("Failed at:", end_time, "\n")
    cat(rep("!", 80), "\n\n", sep = "")
    
    return(list(
      step = step_number,
      file = step_file,
      description = step_description,
      status = "FAILED",
      start_time = start_time,
      end_time = end_time,
      duration = duration,
      error = e$message
    ))
  })
}

# Function to check pipeline prerequisites
check_prerequisites <- function() {
  cat("Checking pipeline prerequisites...\n")
  
  # Check if required directories exist
  required_dirs <- c(
    "data",
    "outputs"
  )
  
  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      cat("Creating directory:", dir, "\n")
      dir.create(dir, recursive = TRUE)
    } else {
      cat("✓ Directory exists:", dir, "\n")
    }
  }
  
  # Check if setup files exist
  setup_files <- c(
    "code/00_setup.R",
    "code/00_data_utils.R"
  )
  
  for (file in setup_files) {
    if (!file.exists(file)) {
      stop("Required setup file not found:", file)
    } else {
      cat("✓ Setup file exists:", file, "\n")
    }
  }
  
  cat("✓ All prerequisites satisfied\n\n")
}

# Function to generate pipeline report
generate_pipeline_report <- function(step_results) {
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("PIPELINE EXECUTION REPORT\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  # Summary statistics
  total_steps <- length(step_results)
  successful_steps <- sum(sapply(step_results, function(x) x$status == "SUCCESS"))
  failed_steps <- total_steps - successful_steps
  
  cat("Pipeline Summary:\n")
  cat("- Total steps:", total_steps, "\n")
  cat("- Successful steps:", successful_steps, "\n")
  cat("- Failed steps:", failed_steps, "\n")
  cat("- Success rate:", round(successful_steps / total_steps * 100, 1), "%\n\n")
  
  # Step-by-step results
  cat("Step-by-Step Results:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  for (i in seq_along(step_results)) {
    result <- step_results[[i]]
    status_icon <- if(result$status == "SUCCESS") "✓" else "✗"
    
    cat(sprintf("%2d. %-50s %s\n", 
                result$step, 
                result$description, 
                status_icon))
    
    if (result$status == "SUCCESS") {
      cat(sprintf("    Duration: %6.1f seconds\n", result$duration))
    } else {
      cat(sprintf("    Error: %s\n", result$error))
    }
  }
  
  # Total execution time
  if (successful_steps > 0) {
    total_duration <- sum(sapply(step_results, function(x) x$duration))
    cat("\nTotal execution time:", round(total_duration, 1), "seconds\n")
  }
  
  cat(rep("=", 80), "\n\n", sep = "")
  
  # Return overall status
  return(failed_steps == 0)
}

# =============================================================================
# Main Pipeline Execution
# =============================================================================

# Initialize pipeline
cat("Initializing data processing pipeline...\n")
check_prerequisites()

# Execute pipeline steps
cat("Starting pipeline execution...\n")
step_results <- list()

for (i in seq_along(pipeline_steps)) {
  step_file <- pipeline_steps[i]
  step_description <- step_descriptions[i]
  
  result <- execute_step(step_file, step_description, i)
  step_results[[i]] <- result
  
  # If a step fails, stop the pipeline
  if (result$status == "FAILED") {
    cat("\nPipeline stopped due to step failure.\n")
    break
  }
}

# Generate final report
pipeline_success <- generate_pipeline_report(step_results)

# Final status
if (pipeline_success) {
  cat("🎉 PIPELINE COMPLETED SUCCESSFULLY! 🎉\n")
  cat("All data processing steps completed without errors.\n")
  cat("Your data is ready for analysis.\n")
} else {
  cat("❌ PIPELINE FAILED ❌\n")
  cat("Some steps failed. Please review the errors above and fix them.\n")
  cat("You can restart the pipeline from any step by running the individual scripts.\n")
}

cat("\nPipeline completed at:", Sys.time(), "\n")
cat("Check the 'data' directory for processed datasets.\n")
cat("Check the 'outputs' directory for analysis results.\n")
