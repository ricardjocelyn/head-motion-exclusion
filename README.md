# Retention and data exclusion challenges for representative longitudinal neuroimaging in the understanding of addiction

Jocelyn A. Ricard, BS [1,2]*, Russell A. Poldrack, PhD [2], Keith Humphreys, PhD [3,4]

1. Stanford Neurosciences Interdepartmental Program, Stanford University School of Medicine, Stanford, CA, United States
2. Department of Psychology, Stanford University, Stanford, CA, United States
3. Center for Innovation to Implementation, VA Palo Alto Health Care System, Palo Alto, CA, USA  
4. Department of Psychiatry and Behavioral Sciences, Stanford University, Stanford, CA, USA


## contents

- [background](#background)
- [data](#data)
- [code](#code)
- [questions](#questions)


## background

[background]



## data

All data are from the ABCD dataset 5.1 release. Casey, BJ et al., (2018) "The Adolescent Brain Cognitive Development (ABCD) study: Imaging acquisition across 21 sites." Developmental cognitive neuroscience, 32, 43-54.

## abcd data csvs: 
```r
mri_y_qc_motion.csv       #head motion 
myi_y_qc_incl.csv         #head motion 
abcd_p_demo.csv           #demographics
abcd_y_lt.csv             #demographics (family id)
participants.tsv          #demographics (site id)
participants.json         #demographics (site id)

```


## code 

All analyses were performed using *R 4.4.2*

This repository contains code and data processing scripts for the analysis of head motion exclusion and retention in the ABCD Study.

## how to run:

### **Infrastructure Files**
- **`00_setup.R`** - Package management, configuration constants, and utility functions
- **`00_data_utils.R`** - Common data processing functions used across scripts

### **Data Processing Pipeline**
- **`01a_data_loading.R`** - Raw data loading and validation
- **`01b_data_cleaning.R`** - Individual dataset cleaning operations
- **`01c_data_integration.R`** - Dataset merging and finalization
- **`01d_data_quality.R`** - Quality checks and summary statistics

### **Master Scripts**
- **`run_data_pipeline.R`** - executes the complete data processing pipeline
- **`run_dairc_pipeline.R`** - runs full dairc analysis pipeline

## Quick Start

### **Option 1: Run Complete Pipeline**
```r
source("run_data_pipeline.R")
```

### **Option 2: Run Individual Steps**
```r
# Step by step execution
source("01a_data_loading.R")       # load and validate data
source("01b_data_cleaning.R")      # clean individual datasets
source("01c_data_integration.R")   # merge and finalize
source("01d_data_quality.R"        # quality assessment
```

### **Run following steps after option 1 or 2:**
```r
# Step by step execution
source("02_dairc.Rmd")      # dairc criterion analysis
source("03_retention.Rmd")  # retention analysis
source("04_exclusion.Rmd")  # exclusion analysis
```

## Outputs
```r
outputs/html/
├── 02_dairc.html          # dairc model outputs
├── 03_retention.html      # retention model outputs
├── 03_exclusion.html      # exclusion model outputs

```

## Questions

Corresponding author: Jocelyn Ricard, ricard [at] stanford.edu
