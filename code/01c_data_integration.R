# =============================================================================
# ABCD Data Integration and Merging
# =============================================================================

# Source the data utilities
source("code/00_data_utils.R")

# =============================================================================
# Load Previously Cleaned Data
# =============================================================================

cat("=== ABCD Data Integration Pipeline ===\n\n")

# Load the cleaned data from the previous step
cleaned_data_file <- file.path(PROJECT_PATHS$data_dir, "01b_cleaned_data.rds")

if (!file.exists(cleaned_data_file)) {
  stop("Please run 01b_data_cleaning.R first to clean the data")
}

cleaned_data <- readRDS(cleaned_data_file)
df_site_info <- cleaned_data$df_site_info
df_demographics_clean <- cleaned_data$df_demographics_clean
df_motion_clean <- cleaned_data$df_motion_clean
df_inclusion_clean <- cleaned_data$df_inclusion_clean
df_family_clean <- cleaned_data$df_family_clean

cat("✓ Loaded cleaned data from previous step\n")

# =============================================================================
# Data Integration Pipeline
# =============================================================================

cat("\nStep 1: Joining datasets...\n")

# Create a combined ABCD data object for the join function
abcd_data_clean <- list(
  motion = df_motion_clean,
  inclusion = df_inclusion_clean,
  family = df_family_clean
)

# Join all datasets
df_merged <- join_datasets(abcd_data_clean, df_site_info, df_demographics_clean)

cat("\nStep 2: Propagating baseline values...\n")
df_final <- propagate_baseline_values(df_merged)

cat("\nStep 3: Finalizing dataset...\n")
df_final <- finalize_dataset(df_final)

# =============================================================================
# Data Integration Quality Checks
# =============================================================================

cat("\n=== Data Integration Quality Checks ===\n")

# Check for duplicate records
check_duplicates <- function(df) {
  duplicates <- df %>%
    group_by(src_subject_id, eventname) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicates) > 0) {
    cat("⚠️  Found", nrow(duplicates), "duplicate records\n")
    print(head(duplicates, 5))
  } else {
    cat("✓ No duplicate records found\n")
  }
  
  return(duplicates)
}

# Check data consistency across timepoints
check_timepoint_consistency <- function(df) {
  cat("\nChecking timepoint consistency...\n")
  
  timepoint_summary <- df %>%
    group_by(eventname) %>%
    summarise(
      n_records = n(),
      n_subjects = n_distinct(src_subject_id),
      motion_mean = mean(rsfmri_meanmotion, na.rm = TRUE),
      motion_sd = sd(rsfmri_meanmotion, na.rm = TRUE),
      exclude_count = sum(exclude_motion, na.rm = TRUE),
      exclude_pct = round(mean(exclude_motion, na.rm = TRUE) * 100, 1)
    )
  
  print(timepoint_summary)
  return(timepoint_summary)
}

# Check family relationships
check_family_relationships <- function(df) {
  cat("\nChecking family relationships...\n")
  
  family_summary <- df %>%
    group_by(rel_family_id) %>%
    summarise(
      n_members = n(),
      n_timepoints = n_distinct(eventname),
      motion_mean = mean(rsfmri_meanmotion, na.rm = TRUE),
      exclude_count = sum(exclude_motion, na.rm = TRUE)
    ) %>%
    arrange(desc(n_members))
  
  cat("Family size distribution:\n")
  family_size_dist <- table(family_summary$n_members)
  print(family_size_dist)
  
  return(family_summary)
}

# Run quality checks
duplicates <- check_duplicates(df_final)
timepoint_summary <- check_timepoint_consistency(df_final)
family_summary <- check_family_relationships(df_final)

# =============================================================================
# Create Analysis-Ready Datasets
# =============================================================================

cat("\n=== Creating Analysis-Ready Datasets ===\n")

# Create subset with imaging inclusion criteria
df_final_included <- df_final %>% 
  filter(imgincl_rsfmri_include == 1)

# Create baseline-only dataset
df_baseline <- df_final %>%
  filter(eventname == "baseline_year_1_arm_1")

# Create follow-up dataset
df_followup <- df_final %>%
  filter(eventname == "2_year_follow_up_y_arm_1")

# Create motion exclusion dataset
df_motion_excluded <- df_final %>%
  filter(exclude_motion == 1)

cat("✓ Created analysis-ready datasets:\n")
cat("  - All participants:", nrow(df_final), "records\n")
cat("  - Included participants:", nrow(df_final_included), "records\n")
cat("  - Baseline only:", nrow(df_baseline), "records\n")
cat("  - Follow-up only:", nrow(df_followup), "records\n")
cat("  - Motion excluded:", nrow(df_motion_excluded), "records\n")

# =============================================================================
# Final Data Summary
# =============================================================================

cat("\n=== Final Data Summary ===\n")

# Generate comprehensive quality report
quality_report <- generate_quality_report(df_final)

# Summary statistics
cat("\nFinal Dataset Summary:\n")
cat("Total records:", nrow(df_final), "\n")
cat("Unique subjects:", length(unique(df_final$src_subject_id)), "\n")
cat("Variables:", ncol(df_final), "\n")
cat("Timepoints:", length(unique(df_final$eventname)), "\n")
cat("Sites:", length(unique(df_final$site)), "\n")

# Motion exclusion summary
cat("\nMotion Exclusion Summary:\n")
cat("Total participants:", sum(!is.na(df_final$exclude_motion)), "\n")
cat("Excluded for motion:", sum(df_final$exclude_motion, na.rm = TRUE), "\n")
cat("Exclusion rate:", round(mean(df_final$exclude_motion, na.rm = TRUE) * 100, 1), "%\n")

# =============================================================================
# Save Final Integrated Data
# =============================================================================

cat("\n=== Saving Final Integrated Data ===\n")

# Save all datasets
final_data_file <- file.path(PROJECT_PATHS$data_dir, "01c_final_integrated_data.rds")
saveRDS(list(
  df_final = df_final,
  df_final_included = df_final_included,
  df_baseline = df_baseline,
  df_followup = df_followup,
  df_motion_excluded = df_motion_excluded,
  quality_report = quality_report,
  timepoint_summary = timepoint_summary,
  family_summary = family_summary
), file = final_data_file)

# Also save the main datasets in the standard format for backward compatibility
output_files <- save_processed_data(df_final, df_final_included, PROJECT_PATHS$data_dir)

cat("✓ Final integrated data saved to:", final_data_file, "\n")
cat("✓ Standard format datasets saved\n")
cat("✓ Data integration pipeline complete\n\n")
