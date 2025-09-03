# =============================================================================
# ABCD Data Loading and Validation
# =============================================================================

# Source the data utilities
source("code/00_data_utils.R")

# =============================================================================
# Main Data Loading Pipeline
# =============================================================================

cat("=== ABCD Data Loading Pipeline ===\n\n")

# Initialize project
initialize_project()

# Load all datasets
cat("Step 1: Loading ABCD core datasets...\n")
abcd_data <- load_abcd_data(PROJECT_PATHS$abcd_data_dir)

cat("\nStep 2: Loading participant metadata...\n")
participant_data <- load_participant_data(
  PROJECT_PATHS$participants_json, 
  PROJECT_PATHS$participants_tsv
)

# =============================================================================
# Data Validation
# =============================================================================

cat("\n=== Data Validation ===\n")

# Validate ABCD data structure
validate_abcd_data <- function(abcd_data) {
  cat("Validating ABCD data structure...\n")
  
  # Check required columns exist
  required_cols <- list(
    inclusion = c("src_subject_id", "eventname", "imgincl_rsfmri_include"),
    demographics = c("src_subject_id", "eventname", "race_ethnicity"),
    motion = c("src_subject_id", "eventname", "rsfmri_meanmotion"),
    family = c("src_subject_id", "eventname", "rel_family_id")
  )
  
  for (dataset_name in names(required_cols)) {
    dataset <- abcd_data[[dataset_name]]
    missing_cols <- setdiff(required_cols[[dataset_name]], names(dataset))
    
    if (length(missing_cols) > 0) {
      cat("⚠️  Missing columns in", dataset_name, ":", paste(missing_cols, collapse = ", "), "\n")
    } else {
      cat("✓", dataset_name, "has all required columns\n")
    }
  }
  
  # Check for completely empty datasets
  for (dataset_name in names(abcd_data)) {
    dataset <- abcd_data[[dataset_name]]
    if (nrow(dataset) == 0) {
      cat("⚠️  Dataset", dataset_name, "is empty\n")
    } else {
      cat("✓", dataset_name, "has", nrow(dataset), "records\n")
    }
  }
}

# Validate participant data
validate_participant_data <- function(participant_data) {
  cat("\nValidating participant data...\n")
  
  if (is.null(participant_data$key)) {
    cat("⚠️  Participant key is missing\n")
  } else {
    cat("✓ Participant key loaded\n")
  }
  
  if (nrow(participant_data$data) == 0) {
    cat("⚠️  Participant data is empty\n")
  } else {
    cat("✓ Participant data has", nrow(participant_data$data), "records\n")
  }
  
  # Check required columns in participant data
  required_participant_cols <- c("participant_id", "session_id", "site", "income", 
                                 "siblings_twins", "sex", "parental_education", "age")
  
  missing_participant_cols <- setdiff(required_participant_cols, names(participant_data$data))
  
  if (length(missing_participant_cols) > 0) {
    cat("⚠️  Missing columns in participant data:", paste(missing_participant_cols, collapse = ", "), "\n")
  } else {
    cat("✓ Participant data has all required columns\n")
  }
}

# Run validation
validate_abcd_data(abcd_data)
validate_participant_data(participant_data)

# =============================================================================
# Data Summary
# =============================================================================

cat("\n=== Data Summary ===\n")

# Summary of loaded data
cat("ABCD Core Datasets:\n")
cat("- Inclusion data:", nrow(abcd_data$inclusion), "records\n")
cat("- Demographics data:", nrow(abcd_data$demographics), "records\n")
cat("- Motion data:", nrow(abcd_data$motion), "records\n")
cat("- Family data:", nrow(abcd_data$family), "records\n")

cat("\nParticipant Metadata:\n")
cat("- Participant data:", nrow(participant_data$data), "records\n")
cat("- Participant key:", ifelse(is.null(participant_data$key), "Missing", "Loaded"), "\n")

# Check for unique subjects
cat("\nUnique Subjects:\n")
cat("- Inclusion data:", length(unique(abcd_data$inclusion$src_subject_id)), "subjects\n")
cat("- Demographics data:", length(unique(abcd_data$demographics$src_subject_id)), "subjects\n")
cat("- Motion data:", length(unique(abcd_data$motion$src_subject_id)), "subjects\n")
cat("- Family data:", length(unique(abcd_data$family$src_subject_id)), "subjects\n")

# =============================================================================
# Save Loaded Data for Next Steps
# =============================================================================

cat("\n=== Saving Loaded Data ===\n")

# Save the loaded data for use in subsequent scripts
loaded_data_file <- file.path(PROJECT_PATHS$data_dir, "01a_loaded_data.rds")
saveRDS(list(
  abcd_data = abcd_data,
  participant_data = participant_data
), file = loaded_data_file)

cat("✓ Loaded data saved to:", loaded_data_file, "\n")
cat("✓ Data loading pipeline complete\n\n")
