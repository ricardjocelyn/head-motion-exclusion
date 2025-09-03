# =============================================================================
# ABCD Data Cleaning Operations
# =============================================================================

# Source the data utilities
source("code/00_data_utils.R")

# =============================================================================
# Load Previously Loaded Data
# =============================================================================

cat("=== ABCD Data Cleaning Pipeline ===\n\n")

# Load the data from the previous step
loaded_data_file <- file.path(PROJECT_PATHS$data_dir, "01a_loaded_data.rds")

if (!file.exists(loaded_data_file)) {
  stop("Please run 01a_data_loading.R first to load the data")
}

loaded_data <- readRDS(loaded_data_file)
abcd_data <- loaded_data$abcd_data
participant_data <- loaded_data$participant_data

cat("✓ Loaded data from previous step\n")

# =============================================================================
# Data Cleaning Pipeline
# =============================================================================

cat("\nStep 1: Cleaning participant demographics data...\n")
df_site_info <- clean_participant_data(participant_data$data)

cat("\nStep 2: Cleaning demographics with race/ethnicity...\n")
df_demographics_clean <- clean_demographics(abcd_data$demographics)

# =============================================================================
# Additional Cleaning Operations
# =============================================================================

# Clean motion data
clean_motion_data <- function(df_motion) {
  cat("\nCleaning motion data...\n")
  
  df_clean <- df_motion %>%
    # Keep ALL motion data (matching original script behavior)
    # Create motion categories for analysis
    mutate(
      motion_category = case_when(
        rsfmri_meanmotion < 0.1 ~ "Low",
        rsfmri_meanmotion < 0.2 ~ "Medium",
        rsfmri_meanmotion >= 0.2 ~ "High",
        TRUE ~ "Missing"
      ),
      motion_category = factor(motion_category, levels = c("Low", "Medium", "High", "Missing"))
    )
  
  cat("✓ Motion data cleaned:", nrow(df_clean), "records\n")
  cat("✓ Motion categories created (keeping all data)\n")
  
  return(df_clean)
}

# Clean inclusion data
clean_inclusion_data <- function(df_inclusion) {
  cat("\nCleaning inclusion data...\n")
  
  df_clean <- df_inclusion %>%
    # Ensure binary coding
    mutate(
      imgincl_rsfmri_include = as.numeric(imgincl_rsfmri_include),
      imgincl_rsfmri_include = ifelse(imgincl_rsfmri_include == 1, 1, 0)
    )
  
  cat("✓ Inclusion data cleaned:", nrow(df_clean), "records\n")
  cat("✓ Binary coding applied\n")
  
  return(df_clean)
}

# Clean family data
clean_family_data <- function(df_family) {
  cat("\nCleaning family data...\n")
  
  df_clean <- df_family %>%
    # Keep ALL family data (matching original script behavior)
    # Keep family ID as numeric to match original script
    # No data type conversion needed
    identity()
  
  cat("✓ Family data cleaned:", nrow(df_clean), "records\n")
  cat("✓ Family IDs kept as numeric (matching original)\n")
  
  return(df_clean)
}

# Run additional cleaning
df_motion_clean <- clean_motion_data(abcd_data$motion)
df_inclusion_clean <- clean_inclusion_data(abcd_data$inclusion)
df_family_clean <- clean_family_data(abcd_data$family)

# =============================================================================
# Data Quality Checks After Cleaning
# =============================================================================

cat("\n=== Data Quality After Cleaning ===\n")

# Check for missing data patterns
check_missing_patterns <- function(df, df_name) {
  cat("\nMissing data in", df_name, ":\n")
  
  missing_summary <- df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_n") %>%
    mutate(
      missing_pct = round(missing_n / nrow(df) * 100, 1),
      df_name = df_name
    ) %>%
    arrange(desc(missing_n))
  
  print(missing_summary)
  return(missing_summary)
}

# Check each cleaned dataset
missing_patterns <- list()
missing_patterns$site_info <- check_missing_patterns(df_site_info, "Site Info")
missing_patterns$demographics <- check_missing_patterns(df_demographics_clean, "Demographics")
missing_patterns$motion <- check_missing_patterns(df_motion_clean, "Motion")
missing_patterns$inclusion <- check_missing_patterns(df_inclusion_clean, "Inclusion")
missing_patterns$family <- check_missing_patterns(df_family_clean, "Family")

# =============================================================================
# Data Summary After Cleaning
# =============================================================================

cat("\n=== Data Summary After Cleaning ===\n")

cat("Cleaned Datasets:\n")
cat("- Site info:", nrow(df_site_info), "records\n")
cat("- Demographics:", nrow(df_demographics_clean), "records\n")
cat("- Motion:", nrow(df_motion_clean), "records\n")
cat("- Inclusion:", nrow(df_inclusion_clean), "records\n")
cat("- Family:", nrow(df_family_clean), "records\n")

# Check unique subjects after cleaning
cat("\nUnique Subjects After Cleaning:\n")
cat("- Site info:", length(unique(df_site_info$src_subject_id)), "subjects\n")
cat("- Demographics:", length(unique(df_demographics_clean$src_subject_id)), "subjects\n")
cat("- Motion:", length(unique(df_motion_clean$src_subject_id)), "subjects\n")
cat("- Inclusion:", length(unique(df_inclusion_clean$src_subject_id)), "subjects\n")
cat("- Family:", length(unique(df_family_clean$src_subject_id)), "subjects\n")

# =============================================================================
# Save Cleaned Data for Next Steps
# =============================================================================

cat("\n=== Saving Cleaned Data ===\n")

# Save the cleaned data for use in subsequent scripts
cleaned_data_file <- file.path(PROJECT_PATHS$data_dir, "01b_cleaned_data.rds")
saveRDS(list(
  df_site_info = df_site_info,
  df_demographics_clean = df_demographics_clean,
  df_motion_clean = df_motion_clean,
  df_inclusion_clean = df_inclusion_clean,
  df_family_clean = df_family_clean,
  missing_patterns = missing_patterns
), file = cleaned_data_file)

cat("✓ Cleaned data saved to:", cleaned_data_file, "\n")
cat("✓ Data cleaning pipeline complete\n\n")
