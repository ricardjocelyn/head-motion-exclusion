# =============================================================================
# ABCD Head Motion Exclusion Project - Setup and Configuration
# =============================================================================

# Load required packages with error handling
load_packages <- function(packages, install_if_missing = TRUE) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (install_if_missing) {
        cat("Installing", pkg, "...\n")
        install.packages(pkg)
      } else {
        stop("Package", pkg, "is required but not installed")
      }
    }
    library(pkg, character.only = TRUE)
  }
  cat("✓ All packages loaded successfully\n")
}

# Core packages for data processing
core_packages <- c(
  "dplyr", "readr", "jsonlite", "tidyr", "stringr"
)

# Analysis packages
analysis_packages <- c(
  "lme4", "broom", "bayestestR", "ggplot2", "stats", "e1071",
  "patchwork", "multcomp", "lmerTest", "broom.mixed", "tableone", 
  "flextable", "officer", "BayesFactor", "MuMIn", "brms", "consort", 
  "ggplotify", "ggeffects", "performance"
)

# Load core packages by default
load_packages(core_packages)

# Load lme4 for model control (needed for glmerControl)
if (requireNamespace("lme4", quietly = TRUE)) {
  library(lme4)
  cat("✓ lme4 package loaded for model control\n")
} else {
  cat("⚠️  lme4 package not available - model control will be limited\n")
}

# =============================================================================
# Configuration Constants
# =============================================================================

# Motion threshold for exclusion
MOTION_THRESHOLD <- 0.2

# ABCD missing value codes
NA_CODES <- c("777", "888", "999") # 777 refuse to answer, 888 decline to answer/question not asked, 999 dont know
NA_NUMERIC <- c(777, 888, 999)

# Race/ethnicity mapping
RACE_ETHNICITY_LABELS <- c(
  "1" = "White",
  "2" = "Black", 
  "3" = "Hispanic",
  "4" = "Asian",
  "5" = "Other"
)

# Sites to exclude
EXCLUDED_SITES <- c("888", "site22") # 22 = unknown, 888 = not reported

# =============================================================================
# Project Paths
# =============================================================================

# Configure data paths - UPDATE THESE FOR YOUR ENVIRONMENT
PROJECT_PATHS <- list(
  # Project root directory
  project_root = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/",
  
  # Data directories
  data_dir = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/data/",
  abcd_data_dir = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/data/",
  
  # Output directory
  output_dir = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/outputs/",
  
  # Participant data files  
  participants_json = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/data/participants.json",
  participants_tsv = "/Users/ricard/Desktop/poldrack/projects/head-motion-exclusion/data/participants.tsv"
)

# Create necessary directories
create_project_directories <- function() {
  dirs_to_create <- c(
    PROJECT_PATHS$data_dir,
    PROJECT_PATHS$output_dir
  )
  
  for (dir_path in dirs_to_create) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat("Created directory:", dir_path, "\n")
    }
  }
}

# =============================================================================
# Model Configuration
# =============================================================================

# Mixed-effects model control parameters
MODEL_CONTROL <- tryCatch({
  if (requireNamespace("lme4", quietly = TRUE)) {
    library(lme4)
    glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5)
    )
  } else {
    NULL
  }
}, error = function(e) {
  cat("⚠️  Could not create model control:", e$message, "\n")
  NULL
})

# =============================================================================
# Utility Functions
# =============================================================================

# Helper function to read ABCD CSV files with standard NA handling
read_abcd_csv <- function(path, ...) {
  if (!file.exists(path)) {
    stop(paste("File does not exist:", path))
  }
  read_csv(path, na = NA_CODES, show_col_types = FALSE, ...)
}

# Clean and validate numeric variables
clean_numeric_variables <- function(df, variables) {
  for (var in variables) {
    if (var %in% names(df)) {
      df[[var]] <- ifelse(df[[var]] %in% NA_NUMERIC, NA, df[[var]])
    }
  }
  return(df)
}

# Print session info for reproducibility
print_session_info <- function() {
  cat("=== Session Info ===\n")
  cat("R version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  cat("Working directory:", getwd(), "\n")
  cat("Project root:", PROJECT_PATHS$project_root, "\n")
  cat("Data directory:", PROJECT_PATHS$data_dir, "\n")
  cat("Output directory:", PROJECT_PATHS$output_dir, "\n")
  cat("Motion threshold:", MOTION_THRESHOLD, "\n")
  cat("==================\n\n")
}

# Initialize project
initialize_project <- function() {
  cat("=== Initializing ABCD Head Motion Exclusion Project ===\n")
  create_project_directories()
  print_session_info()
  cat("✓ Project initialized successfully\n\n")
}
