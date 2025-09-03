# =============================================================================
# ABCD Data Quality Assessment and Summary
# =============================================================================

# Source the data utilities
source("code/00_data_utils.R")

# =============================================================================
# Load Final Integrated Data
# =============================================================================

cat("=== ABCD Data Quality Assessment ===\n\n")

# Load the final integrated data from the previous step
final_data_file <- file.path(PROJECT_PATHS$data_dir, "01c_final_integrated_data.rds")

if (!file.exists(final_data_file)) {
  stop("Please run 01c_data_integration.R first to integrate the data")
}

final_data <- readRDS(final_data_file)
df_final <- final_data$df_final
df_final_included <- final_data$df_final_included
df_baseline <- final_data$df_baseline
df_followup <- final_data$df_followup
df_motion_excluded <- final_data$df_motion_excluded

cat("✓ Loaded final integrated data from previous step\n")

# =============================================================================
# Comprehensive Data Quality Assessment
# =============================================================================

cat("\n=== Comprehensive Data Quality Assessment ===\n")

# 1. Missing Data Analysis
analyze_missing_data <- function(df, dataset_name) {
  cat("\n--- Missing Data Analysis for", dataset_name, "---\n")
  
  # Overall missing data
  total_cells <- nrow(df) * ncol(df)
  missing_cells <- sum(is.na(df))
  missing_pct <- round(missing_cells / total_cells * 100, 2)
  
  cat("Total cells:", total_cells, "\n")
  cat("Missing cells:", missing_cells, "\n")
  cat("Missing percentage:", missing_pct, "%\n")
  
  # Missing data by variable
  missing_by_var <- df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "missing_n") %>%
    mutate(
      missing_pct = round(missing_n / nrow(df) * 100, 1),
      dataset = dataset_name
    ) %>%
    arrange(desc(missing_n))
  
  cat("\nMissing data by variable:\n")
  print(missing_by_var)
  
  # Missing data patterns
  cat("\nMissing data patterns:\n")
  missing_patterns <- df %>%
    mutate(across(everything(), ~is.na(.))) %>%
    group_by_all() %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  print(head(missing_patterns, 10))
  
  return(list(
    missing_by_var = missing_by_var,
    missing_patterns = missing_patterns,
    summary = list(total_cells = total_cells, missing_cells = missing_cells, missing_pct = missing_pct)
  ))
}

# 2. Data Distribution Analysis
analyze_distributions <- function(df, dataset_name) {
  cat("\n--- Distribution Analysis for", dataset_name, "---\n")
  
  # Numeric variables
  numeric_vars <- df %>%
    select_if(is.numeric) %>%
    names()
  
  if (length(numeric_vars) > 0) {
    cat("Numeric variables:", paste(numeric_vars, collapse = ", "), "\n")
    
    numeric_summary <- df %>%
      select(all_of(numeric_vars)) %>%
      summarise(across(everything(), list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        missing = ~sum(is.na(.))
      ))) %>%
      pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "_") %>%
      pivot_wider(names_from = stat, values_from = value)
    
    print(numeric_summary)
  }
  
  # Categorical variables
  categorical_vars <- df %>%
    select_if(is.factor) %>%
    names()
  
  if (length(categorical_vars) > 0) {
    cat("\nCategorical variables:", paste(categorical_vars, collapse = ", "), "\n")
    
    for (var in categorical_vars) {
      cat("\n", var, "distribution:\n")
      print(table(df[[var]], useNA = "ifany"))
    }
  }
  
  return(list(
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    numeric_summary = if(length(numeric_vars) > 0) numeric_summary else NULL
  ))
}

# 3. Motion Data Analysis
analyze_motion_data <- function(df, dataset_name) {
  cat("\n--- Motion Data Analysis for", dataset_name, "---\n")
  
  if (!"rsfmri_meanmotion" %in% names(df)) {
    cat("No motion data available\n")
    return(NULL)
  }
  
  # Basic motion statistics
  motion_stats <- df %>%
    summarise(
      n_total = n(),
      n_with_motion = sum(!is.na(rsfmri_meanmotion)),
      n_missing_motion = sum(is.na(rsfmri_meanmotion)),
      motion_mean = mean(rsfmri_meanmotion, na.rm = TRUE),
      motion_sd = sd(rsfmri_meanmotion, na.rm = TRUE),
      motion_median = median(rsfmri_meanmotion, na.rm = TRUE),
      motion_min = min(rsfmri_meanmotion, na.rm = TRUE),
      motion_max = max(rsfmri_meanmotion, na.rm = TRUE),
      motion_q25 = quantile(rsfmri_meanmotion, 0.25, na.rm = TRUE),
      motion_q75 = quantile(rsfmri_meanmotion, 0.75, na.rm = TRUE)
    )
  
  cat("Motion Statistics:\n")
  print(motion_stats)
  
  # Motion by timepoint
  if ("eventname" %in% names(df)) {
    cat("\nMotion by timepoint:\n")
    motion_by_timepoint <- df %>%
      group_by(eventname) %>%
      summarise(
        n = n(),
        motion_mean = mean(rsfmri_meanmotion, na.rm = TRUE),
        motion_sd = sd(rsfmri_meanmotion, na.rm = TRUE),
        motion_median = median(rsfmri_meanmotion, na.rm = TRUE)
      )
    print(motion_by_timepoint)
  }
  
  # Motion by site
  if ("site" %in% names(df)) {
    cat("\nMotion by site (top 10):\n")
    motion_by_site <- df %>%
      group_by(site) %>%
      summarise(
        n = n(),
        motion_mean = mean(rsfmri_meanmotion, na.rm = TRUE),
        motion_sd = sd(rsfmri_meanmotion, na.rm = TRUE)
      ) %>%
      arrange(desc(n)) %>%
      head(10)
    print(motion_by_site)
  }
  
  return(motion_stats)
}

# 4. Exclusion Analysis
analyze_exclusions <- function(df, dataset_name) {
  cat("\n--- Exclusion Analysis for", dataset_name, "---\n")
  
  if (!"exclude_motion" %in% names(df)) {
    cat("No exclusion data available\n")
    return(NULL)
  }
  
  exclusion_summary <- df %>%
    summarise(
      n_total = n(),
      n_excluded = sum(exclude_motion, na.rm = TRUE),
      n_included = sum(exclude_motion == 0, na.rm = TRUE),
      n_missing_exclusion = sum(is.na(exclude_motion)),
      exclusion_rate = round(mean(exclude_motion, na.rm = TRUE) * 100, 1)
    )
  
  cat("Exclusion Summary:\n")
  print(exclusion_summary)
  
  # Exclusions by demographics
  if ("sex" %in% names(df)) {
    cat("\nExclusions by sex:\n")
    exclusions_by_sex <- df %>%
      group_by(sex) %>%
      summarise(
        n = n(),
        n_excluded = sum(exclude_motion, na.rm = TRUE),
        exclusion_rate = round(mean(exclude_motion, na.rm = TRUE) * 100, 1)
      )
    print(exclusions_by_sex)
  }
  
  if ("race_ethnicity_label" %in% names(df)) {
    cat("\nExclusions by race/ethnicity:\n")
    exclusions_by_race <- df %>%
      group_by(race_ethnicity_label) %>%
      summarise(
        n = n(),
        n_excluded = sum(exclude_motion, na.rm = TRUE),
        exclusion_rate = round(mean(exclude_motion, na.rm = TRUE) * 100, 1)
      )
    print(exclusions_by_race)
  }
  
  return(exclusion_summary)
}

# Run comprehensive analysis on all datasets
cat("\nRunning comprehensive quality assessment...\n")

quality_assessment <- list()

# Analyze main dataset
quality_assessment$main <- list(
  missing = analyze_missing_data(df_final, "Main Dataset"),
  distributions = analyze_distributions(df_final, "Main Dataset"),
  motion = analyze_motion_data(df_final, "Main Dataset"),
  exclusions = analyze_exclusions(df_final, "Main Dataset")
)

# Analyze included dataset
quality_assessment$included <- list(
  missing = analyze_missing_data(df_final_included, "Included Dataset"),
  distributions = analyze_distributions(df_final_included, "Included Dataset"),
  motion = analyze_motion_data(df_final_included, "Included Dataset")
)

# Analyze baseline dataset
quality_assessment$baseline <- list(
  missing = analyze_missing_data(df_baseline, "Baseline Dataset"),
  distributions = analyze_distributions(df_baseline, "Baseline Dataset"),
  motion = analyze_motion_data(df_baseline, "Baseline Dataset"),
  exclusions = analyze_exclusions(df_baseline, "Baseline Dataset")
)

# =============================================================================
# Generate Quality Report
# =============================================================================

cat("\n=== Generating Quality Report ===\n")

# Create comprehensive quality report
create_quality_report <- function(quality_assessment) {
  
  report <- list()
  
  # Executive summary
  report$executive_summary <- list(
    timestamp = Sys.time(),
    datasets_analyzed = names(quality_assessment),
    total_records = nrow(df_final),
    unique_subjects = length(unique(df_final$src_subject_id)),
    motion_exclusion_rate = round(mean(df_final$exclude_motion, na.rm = TRUE) * 100, 1),
    overall_missing_rate = round(sum(is.na(df_final)) / (nrow(df_final) * ncol(df_final)) * 100, 2)
  )
  
  # Data quality scores
  report$quality_scores <- list(
    completeness = 100 - report$executive_summary$overall_missing_rate,
    motion_quality = 100 - report$executive_summary$motion_exclusion_rate,
    data_integrity = ifelse(nrow(duplicates) == 0, 100, 90) # Penalize for duplicates
  )
  
  # Recommendations
  report$recommendations <- list()
  
  if (report$quality_scores$completeness < 90) {
    report$recommendations$completeness <- "High missing data rate detected. Consider data imputation or investigate missing data patterns."
  }
  
  if (report$quality_scores$motion_quality < 80) {
    report$recommendations$motion <- "High motion exclusion rate. Consider adjusting motion threshold or investigating motion patterns."
  }
  
  if (report$quality_scores$data_integrity < 100) {
    report$recommendations$integrity <- "Duplicate records detected. Clean data before analysis."
  }
  
  return(report)
}

# Generate the report
quality_report <- create_quality_report(quality_assessment)

# Print executive summary
cat("\n=== Executive Summary ===\n")
cat("Datasets analyzed:", paste(quality_report$executive_summary$datasets_analyzed, collapse = ", "), "\n")
cat("Total records:", quality_report$executive_summary$total_records, "\n")
cat("Unique subjects:", quality_report$executive_summary$unique_subjects, "\n")
cat("Motion exclusion rate:", quality_report$executive_summary$motion_exclusion_rate, "%\n")
cat("Overall missing rate:", quality_report$executive_summary$overall_missing_rate, "%\n")

cat("\nQuality Scores:\n")
cat("Completeness:", quality_report$quality_scores$completeness, "%\n")
cat("Motion Quality:", quality_report$quality_scores$motion_quality, "%\n")
cat("Data Integrity:", quality_report$quality_scores$data_integrity, "%\n")

if (length(quality_report$recommendations) > 0) {
  cat("\nRecommendations:\n")
  for (rec_type in names(quality_report$recommendations)) {
    cat("-", rec_type, ":", quality_report$recommendations[[rec_type]], "\n")
  }
}

# =============================================================================
# Save Quality Assessment Results
# =============================================================================

cat("\n=== Saving Quality Assessment Results ===\n")

# Save the comprehensive quality assessment
quality_file <- file.path(PROJECT_PATHS$data_dir, "01d_quality_assessment.rds")
saveRDS(list(
  quality_assessment = quality_assessment,
  quality_report = quality_report,
  timestamp = Sys.time()
), file = quality_file)

cat("✓ Quality assessment saved to:", quality_file, "\n")
cat("✓ Data quality assessment complete\n\n")
