# =============================================================================
# ABCD Head Motion Exclusion Project - Data Utility Functions
# =============================================================================

# Source the setup file to get configuration
source("code/00_setup.R")

# =============================================================================
# Data Loading Functions
# =============================================================================

# Load core ABCD datasets
load_abcd_data <- function(data_dir) {
  
  cat("Loading ABCD core datasets...\n")
  
  # Load imaging inclusion data
  df_inclusion <- read_abcd_csv(file.path(data_dir, "mri_y_qc_incl.csv")) %>%
    dplyr::select(src_subject_id, eventname, imgincl_rsfmri_include)
  
  # Load demographics data  
  df_demographics <- read_abcd_csv(file.path(data_dir, "abcd_p_demo.csv")) %>%
    dplyr::select(src_subject_id, eventname, race_ethnicity)
  
  # Load motion data
  df_motion <- read_abcd_csv(file.path(data_dir, "mri_y_qc_motion.csv")) %>%
    dplyr::select(src_subject_id, eventname, rsfmri_meanmotion)
  
  # Load family relationship data
  df_family <- read_abcd_csv(file.path(data_dir, "abcd_y_lt.csv")) %>%
    dplyr::select(src_subject_id, eventname, rel_family_id)
  
  cat("✓ Loaded", nrow(df_inclusion), "inclusion records\n")
  cat("✓ Loaded", nrow(df_demographics), "demographic records\n") 
  cat("✓ Loaded", nrow(df_motion), "motion records\n")
  cat("✓ Loaded", nrow(df_family), "family records\n")
  
  return(list(
    inclusion = df_inclusion,
    demographics = df_demographics, 
    motion = df_motion,
    family = df_family
  ))
}

# Load participant metadata  
load_participant_data <- function(json_path, tsv_path) {
  
  cat("Loading participant metadata...\n")
  
  # Read participant key and data
  demo_key <- read_json(json_path)
  raw_demo_data <- read_tsv(tsv_path, show_col_types = FALSE)
  
  cat("✓ Loaded", nrow(raw_demo_data), "participant records\n")
  
  return(list(
    key = demo_key,
    data = raw_demo_data
  ))
}

# =============================================================================
# Data Cleaning Functions
# =============================================================================

# Clean participant demographics data
clean_participant_data <- function(raw_demo_data) {
  
  cat("Cleaning participant data...\n")
  
  df_site_info <- raw_demo_data %>%
    # Standardize subject ID format
    rename(src_subject_id = participant_id) %>%
    mutate(
      src_subject_id = gsub("sub-", "", src_subject_id),
      src_subject_id = gsub("NDARINV", "NDAR_INV", src_subject_id)
    ) %>%
    # Standardize event names
    rename(eventname = session_id) %>%
    mutate(
      eventname = case_when(
        eventname == "ses-baselineYear1Arm1" ~ "baseline_year_1_arm_1",
        eventname == "ses-2YearFollowUpYArm1" ~ "2_year_follow_up_y_arm_1",
        TRUE ~ eventname
      )
    ) %>%
    # Select and clean variables
    dplyr::select(src_subject_id, eventname, site, income, siblings_twins, 
           sex, parental_education, age) %>%
    mutate(site = as.factor(site)) %>%
    # Remove excluded sites
    filter(!site %in% EXCLUDED_SITES)
  
  cat("✓ Cleaned", nrow(df_site_info), "participant records\n")
  return(df_site_info)
}

# Clean demographics with race/ethnicity
clean_demographics <- function(df_demographics) {
  
  cat("Processing race/ethnicity data...\n")
  
  df_clean <- df_demographics %>%
    mutate(
      race_ethnicity = as.factor(race_ethnicity),
      race_ethnicity_label = factor(
        race_ethnicity,
        levels = names(RACE_ETHNICITY_LABELS),
        labels = RACE_ETHNICITY_LABELS
      )
    ) %>%
    # Forward/backward fill missing race/ethnicity within subjects
    group_by(src_subject_id) %>%
    fill(race_ethnicity, race_ethnicity_label, .direction = "downup") %>%
    ungroup()
  
  cat("✓ Processed race/ethnicity for", length(unique(df_clean$src_subject_id)), "subjects\n")
  return(df_clean)
}

# =============================================================================
# Data Integration Functions
# =============================================================================

# Efficiently join all datasets
join_datasets <- function(abcd_data, df_site_info, df_demographics_clean) {
  
  cat("Joining datasets...\n")
  
  # Start with demographics and add other datasets
  df_merged <- df_demographics_clean %>%
    left_join(abcd_data$motion, by = c("src_subject_id", "eventname")) %>%
    left_join(abcd_data$inclusion, by = c("src_subject_id", "eventname")) %>%
    left_join(df_site_info, by = c("src_subject_id", "eventname"))
  
  # Add family information (baseline only, so use distinct)
  family_info <- abcd_data$family %>%
    dplyr::select(src_subject_id, rel_family_id) %>%
    distinct()
  
  df_merged <- df_merged %>%
    left_join(family_info, by = "src_subject_id")
  
  cat("✓ Merged datasets:", nrow(df_merged), "total records\n")
  cat("✓ Unique subjects:", length(unique(df_merged$src_subject_id)), "\n")
  
  return(df_merged)
}

# Extract and propagate baseline values
propagate_baseline_values <- function(df_merged) {
  
  cat("Propagating baseline values...\n")
  
  # Remove duplicates first
  df_merged <- df_merged %>% distinct(src_subject_id, eventname, .keep_all = TRUE)
  
  # Overwrite ALL parental education values with baseline value 
  if ("parental_education" %in% names(df_merged)) {
    df_final <- df_merged %>%
      group_by(src_subject_id) %>%
      mutate(parental_education = parental_education[which(eventname == "baseline_year_1_arm_1")[1]]) %>%
      ungroup()
  } else {
    df_final <- df_merged
  }
  
  # Report final status parental education
  final_pe_summary <- df_final %>%
    group_by(eventname) %>%
    summarise(
      total = n(),
      pe_present = sum(!is.na(parental_education), na.rm = TRUE),
      pe_missing = sum(is.na(parental_education), na.rm = TRUE),
      pct_missing = round(pe_missing / total * 100, 1)
    )
  
  cat("\nFinal parental education status:\n")
  print(final_pe_summary)
  
  cat("✓ Propagated baseline values\n")
  return(df_final)
}

# =============================================================================
# Data Finalization Functions
# =============================================================================

# Create final variables and apply exclusions
finalize_dataset <- function(df_final) {
  
  cat("Creating final variables...\n")
  
  df_final <- df_final %>%
    # Remove duplicates
    distinct(src_subject_id, eventname, .keep_all = TRUE) %>%
    # Create exclusion variable based on motion threshold
    mutate(
      exclude_motion = ifelse(rsfmri_meanmotion >= MOTION_THRESHOLD, 1, 0)
    ) %>%
    # Clean numeric variables with ABCD missing codes
    clean_numeric_variables(c("age", "parental_education", "income")) %>%
    # Reorder columns logically
    dplyr::select(
      src_subject_id, eventname, rel_family_id,
      race_ethnicity, race_ethnicity_label, 
      site, age, sex, parental_education, income, siblings_twins,
      rsfmri_meanmotion, exclude_motion, imgincl_rsfmri_include
    )
  
  cat("✓ Final dataset:", nrow(df_final), "records\n")
  cat("✓ Variables:", ncol(df_final), "\n")
  
  return(df_final)
}

# =============================================================================
# Data Quality Functions
# =============================================================================

# Generate comprehensive data quality report
generate_quality_report <- function(df_final) {
  
  cat("=== Data Quality Summary ===\n\n")
  
  # Missing data summary
  missing_summary <- df_final %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_n") %>%
    mutate(missing_pct = round(missing_n / nrow(df_final) * 100, 1)) %>%
    arrange(desc(missing_n))
  
  cat("Missing Data Summary:\n")
  print(missing_summary)
  
  # Motion statistics
  cat("\nMotion Statistics:\n")
  cat("Mean motion (mm):", round(mean(df_final$rsfmri_meanmotion, na.rm = TRUE), 3), "\n")
  cat("SD motion:", round(sd(df_final$rsfmri_meanmotion, na.rm = TRUE), 3), "\n")
  cat("Motion > threshold (", MOTION_THRESHOLD, "):", 
      sum(df_final$exclude_motion, na.rm = TRUE), "/", sum(!is.na(df_final$exclude_motion)), 
      "(", round(mean(df_final$exclude_motion, na.rm = TRUE) * 100, 1), "%)\n")
  
  # Race/ethnicity distribution
  cat("\nRace/Ethnicity Distribution:\n")
  race_table <- table(df_final$race_ethnicity_label, useNA = "ifany")
  print(race_table)
  
  # Site distribution
  cat("\nSite Distribution:\n")
  site_table <- table(df_final$site, useNA = "ifany")
  print(head(site_table, 10)) # Show first 10 sites
  
  return(missing_summary)
}

# =============================================================================
# Data Export Functions
# =============================================================================

# Save processed datasets
save_processed_data <- function(df_final, df_final_included, output_dir) {
  
  cat("Saving processed datasets...\n")
  
  output_files <- list(
    all_participants = file.path(output_dir, "df_final_all-refactored.rds"),
    included_participants = file.path(output_dir, "df_final_included-refactored.rds")
  )
  
  # Save datasets
  saveRDS(df_final, file = output_files$all_participants)
  saveRDS(df_final_included, file = output_files$included_participants)
  
  cat("=== Files Saved ===\n")
  cat("All participants:", output_files$all_participants, "\n")
  cat("Included participants:", output_files$included_participants, "\n")
  
  return(output_files)
}
