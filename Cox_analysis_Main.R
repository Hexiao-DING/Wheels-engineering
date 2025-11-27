# ==============================================================================
#                     COX PROPORTIONAL HAZARDS REGRESSION ANALYSIS
#                     For Lifestyle Risk Factors and Health Outcomes
# ==============================================================================
#
# PURPOSE:
#   This script performs Cox proportional hazards regression analysis to evaluate
#   the association between lifestyle risk factors (e.g., smoking, alcohol, physical
#   inactivity) and time-to-event outcomes (e.g., mortality, disease incidence).
#
# KEY FEATURES:
#   1. Categorical analysis: Compare risk across different exposure levels (0,1,2,3 factors)
#   2. Trend analysis: Test for dose-response relationship (P for trend)
#   3. Pattern analysis: Evaluate specific combinations of risk factors
#   4. Proportional hazards assumption testing
#
# DATA REQUIREMENTS:
#   Your dataset should contain the following variables:
#   +-----------------+----------------------------------------------------------+
#   | Variable Type   | Description                                              |
#   +-----------------+----------------------------------------------------------+
#   | Time            | Follow-up time (numeric, in years or months)             |
#   | Status          | Event indicator (1 = event occurred, 0 = censored)       |
#   | Exposures       | Binary risk factors (0 = healthy, 1 = unhealthy)         |
#   |                 | Examples: Smoke, Alcohol, Inactive                       |
#   | Covariates      | Adjustment variables (Age, Sex, Education, etc.)         |
#   | Strata          | (Optional) Stratification variable for pooled analysis   |
#   +-----------------+----------------------------------------------------------+
#
# OUTPUT:
#   - Hazard Ratios (HR) with 95% confidence intervals
#   - P-values for each exposure level
#   - P for trend (dose-response)
#   - Proportional hazards assumption test results
#
# AUTHOR: [Your Name]
# DATE: [Date]
# VERSION: 1.0
# ==============================================================================


# ==============================================================================
# PART 0: USER CONFIGURATION (MODIFY THIS SECTION)
# ==============================================================================
# INSTRUCTIONS: This is the ONLY section you need to modify.
# Set your data file path and variable mappings below.
# ==============================================================================

# -----------------------------------------------------------------------------
# 0.1 DATA SOURCE SETTINGS
# -----------------------------------------------------------------------------
# Set USE_OWN_DATA to TRUE if you have your own dataset
# Set USE_OWN_DATA to FALSE to use simulated demo data (for testing)

USE_OWN_DATA <- FALSE 

# Path to your data file (CSV format)
# Example: "C:/Users/yourname/data/cohort_data.csv" or "my_cohort_data.csv"
DATA_FILE <- "my_cohort_data.csv" 


# -----------------------------------------------------------------------------
# 0.2 VARIABLE MAPPING CONFIGURATION
# -----------------------------------------------------------------------------
# Map your dataset's column names to the analysis variables.
# Change the values on the RIGHT side to match YOUR column names.

CONFIG <- list(
  # -------------------------------------------------------------------------
  # OUTCOME VARIABLES (Required)
  # -------------------------------------------------------------------------
  TIME   = "Time",      # Column name for follow-up time (numeric)
                        # Unit can be years, months, or days (be consistent)
  
  STATUS = "Status",    # Column name for event status
                        # Must be: 1 = event occurred, 0 = censored (no event)
  
  # -------------------------------------------------------------------------
  # EXPOSURE VARIABLES (Required)
  # -------------------------------------------------------------------------
  # List of binary lifestyle risk factors to analyze
  # Each should be coded as: 0 = healthy/unexposed, 1 = unhealthy/exposed
  # 
  # Examples of risk factors:
  #   - Smoke: 0 = never/former smoker, 1 = current smoker
  #   - Alcohol: 0 = none/moderate, 1 = heavy drinking
  #   - Inactive: 0 = physically active, 1 = physically inactive
  #   - Diet: 0 = healthy diet, 1 = unhealthy diet
  #   - BMI: 0 = normal weight, 1 = obese
  #   - Sleep: 0 = adequate sleep, 1 = poor sleep
  
  EXPOSURES = c("Smoke", "Alcohol", "Inactive"), 
  
  # -------------------------------------------------------------------------
  # COVARIATE VARIABLES (Required for adjustment)
  # -------------------------------------------------------------------------
  # Variables to adjust for in the Cox model
  # Common covariates include:
  #   - Demographic: Age, Sex, Race/Ethnicity
  #   - Socioeconomic: Education, Income, Occupation
  #   - Health: Baseline BMI, Comorbidities
  #
  # Note: Categorical variables should be factors in your data
  
  COVARIATES = c("Age", "Sex", "Edu"),
  
  # -------------------------------------------------------------------------
  # STRATIFICATION VARIABLE (Optional)
  # -------------------------------------------------------------------------
  # For pooled/meta-analysis of multiple cohorts (e.g., multi-center studies)
  # This creates study-specific baseline hazards
  # Leave empty ("") if you have a single cohort
  #
  # Example: If combining HRS, ELSA, CHARLS cohorts, use "Study"
  
  STRATA = "Study"    
)


# ==============================================================================
# PART 1: ENVIRONMENT SETUP AND DATA LOADING
# ==============================================================================
# This section handles package installation and data import.
# No modification needed unless adding new packages.
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Package Installation and Loading
# -----------------------------------------------------------------------------
# Required packages:
#   - survival: Cox proportional hazards models
#   - dplyr: Data manipulation
#   - broom: Tidy model outputs
#   - stringr: String operations

pkg_list <- c("survival", "dplyr", "broom", "stringr")

# Automatically install missing packages
new_pkg <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
if (length(new_pkg)) {
  message(">>> Installing missing packages: ", paste(new_pkg, collapse = ", "))
  install.packages(new_pkg)
}

# Load all packages silently
suppressPackageStartupMessages(
  lapply(pkg_list, library, character.only = TRUE)
)

message(">>> All required packages loaded successfully.")


# -----------------------------------------------------------------------------
# 1.2 Data Loading Function
# -----------------------------------------------------------------------------
#' @title Load Analysis Data
#' @description 
#'   Loads data from CSV file or generates simulated demo data for testing.
#'   The function checks if USE_OWN_DATA is TRUE and loads the specified file,
#'   otherwise generates a realistic simulated dataset.
#'
#' @return A data frame ready for analysis
#' @examples
#'   dt_raw <- load_data()

load_data <- function() {
  
  if (USE_OWN_DATA) {
    # ------ Load User's Own Data ------
    if (!file.exists(DATA_FILE)) {
      stop("ERROR: Cannot find data file: ", DATA_FILE, 
           "\nPlease check the file path and try again.")
    }
    message(">>> Loading user data from: ", DATA_FILE)
    
    # Read CSV with proper encoding
    data <- read.csv(DATA_FILE, stringsAsFactors = TRUE)
    
    # Validate required columns exist
    required_cols <- c(CONFIG$TIME, CONFIG$STATUS, CONFIG$EXPOSURES, CONFIG$COVARIATES)
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      stop("ERROR: Missing required columns: ", paste(missing_cols, collapse = ", "),
           "\nPlease check your CONFIG variable mappings.")
    }
    
    message(">>> Data loaded successfully. N = ", nrow(data), " observations.")
    return(data)
    
  } else {
    # ------ Generate Simulated Demo Data ------
    message(">>> Generating simulated demo data (N = 2000)...")
    message("    NOTE: Set USE_OWN_DATA = TRUE to use your own data.")
    
    set.seed(2023)  # For reproducibility
    n <- 2000
    
    # Create demographic and exposure variables
    dt <- data.frame(
      ID = 1:n,
      
      # Study indicator (for stratified analysis demo)
      Study = sample(c("HRS", "ELSA", "CHARLS"), n, replace = TRUE),
      
      # Demographic covariates
      Age = round(rnorm(n, mean = 65, sd = 10), 0),
      Sex = factor(sample(c(0, 1), n, replace = TRUE), 
                   labels = c("Female", "Male")),
      Edu = factor(sample(c(1, 2, 3), n, replace = TRUE), 
                   labels = c("Low", "Mid", "High")),
      
      # Lifestyle exposure variables (binary: 0 = healthy, 1 = unhealthy)
      Smoke    = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
      Alcohol  = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
      Inactive = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))
    )
    
    # Generate survival outcomes based on exposure
    # Logic: More risk factors -> Higher hazard rate -> Shorter survival time
    risk_score <- rowSums(dt[, c("Smoke", "Alcohol", "Inactive")])
    
    # Exponential survival times with hazard rate dependent on risk score
    # Base hazard rate: 0.02 per year
    # Each risk factor increases hazard by ~65% (exp(0.5) ≈ 1.65)
    dt$Time <- rexp(n, rate = 0.02 * exp(0.5 * risk_score))
    
    # Administrative censoring at 9 years
    max_followup <- 9
    dt$Status <- ifelse(dt$Time < max_followup, 1, 0)
    dt$Time <- pmin(dt$Time, max_followup)
    
    message(">>> Simulated data generated successfully.")
    return(dt)
  }
}

# Execute data loading
dt_raw <- load_data()


# ==============================================================================
# PART 2: CORE ANALYSIS FUNCTIONS
# ==============================================================================
# These functions perform the main analytical procedures.
# Modify only if you need to customize the analysis approach.
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Lifestyle Feature Engineering Function
# -----------------------------------------------------------------------------
#' @title Process Lifestyle Features
#' @description 
#'   This function creates derived variables from individual lifestyle factors:
#'   1. Life_Score: Sum of unhealthy factors (0, 1, 2, 3, ...)
#'   2. Life_Cat: Categorical version of score (factor)
#'   3. Life_Pattern: Specific combination pattern (e.g., "Smoke+Inactive")
#'
#' @param data Input data frame
#' @param vars Character vector of exposure variable names
#' 
#' @return Data frame with added columns: Life_Score, Life_Cat, Life_Pattern
#'
#' @examples
#'   dt_clean <- process_lifestyle_features(dt_raw, vars = c("Smoke", "Alcohol", "Inactive"))

process_lifestyle_features <- function(data, vars) {
  
  # Validate that all exposure variables exist in the data
  missing_vars <- vars[!vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("ERROR: The following exposure variables are not found in the data: ", 
         paste(missing_vars, collapse = ", "))
  }
  
  df <- data
  
  # -------------------------------------------------------------------------
  # STEP 1: Calculate Lifestyle Risk Score (numeric sum)
  # -------------------------------------------------------------------------
  # This sums all binary exposure variables
  # Example: Smoke=1 + Alcohol=0 + Inactive=1 = Score of 2
  df$Life_Score <- rowSums(df[, vars], na.rm = TRUE)
  
  # -------------------------------------------------------------------------
  # STEP 2: Create Categorical Score Variable (factor)
  # -------------------------------------------------------------------------
  # Used for categorical Cox regression (comparing each level to reference)
  # Levels are sorted numerically: 0, 1, 2, 3, ...
  # Level 0 (no risk factors) is automatically the reference group
  df$Life_Cat <- factor(df$Life_Score, 
                        levels = sort(unique(df$Life_Score)))
  
  # -------------------------------------------------------------------------
  # STEP 3: Generate Combination Pattern Variable (factor)
  # -------------------------------------------------------------------------
  # Creates specific pattern labels like "Smoke+Alcohol", "Inactive", "None"
  # Useful for identifying which specific combinations are most harmful
  df$Life_Pattern <- apply(df[, vars], 1, function(row) {
    # Identify which risk factors are present (value = 1)
    risk_factors <- names(row)[row == 1]
    
    # If no risk factors, label as "None"
    if (length(risk_factors) == 0) {
      return("None")
    }
    
    # Concatenate present risk factors with "+"
    return(paste(sort(risk_factors), collapse = "+"))
  })
  
  # Convert to factor and set "None" as reference level
  df$Life_Pattern <- factor(df$Life_Pattern)
  if ("None" %in% levels(df$Life_Pattern)) {
    df$Life_Pattern <- relevel(df$Life_Pattern, ref = "None")
  }
  
  message(">>> Feature engineering complete:")
  message("    - Life_Score range: ", min(df$Life_Score), " to ", max(df$Life_Score))
  message("    - Life_Pattern levels: ", length(unique(df$Life_Pattern)))
  
  return(df)
}


# -----------------------------------------------------------------------------
# 2.2 Cox Regression Analysis Function
# -----------------------------------------------------------------------------
#' @title Run Cox Proportional Hazards Regression
#' @description 
#'   Fits a Cox model with specified exposure variable, adjusting for covariates
#'   and optionally stratifying by study/site.
#'   
#'   Model formula: Surv(Time, Status) ~ Exposure + Covariates + strata(Study)
#'
#' @param data Analysis data frame (must contain all required variables)
#' @param exposure_var Name of the exposure variable to analyze (character)
#' @param config Configuration list containing TIME, STATUS, COVARIATES, STRATA
#'
#' @return A list containing:
#'   - model: The fitted coxph object
#'   - table: A data frame with formatted HR, 95% CI, and p-values
#'
#' @examples
#'   result <- run_cox_analysis(dt_clean, "Life_Cat", CONFIG)
#'   print(result$table)

run_cox_analysis <- function(data, exposure_var, config) {
  
  # -------------------------------------------------------------------------
  # STEP 1: Construct the Cox Model Formula
  # -------------------------------------------------------------------------
  # Base formula: Surv(time, status) ~ exposure + covariate1 + covariate2 + ...
  
  cov_str <- paste(config$COVARIATES, collapse = " + ")
  
  formula_str <- paste0(
    "Surv(", config$TIME, ", ", config$STATUS, ") ~ ", 
    exposure_var, " + ", cov_str
  )
  
  # Add stratification term if specified (for pooled multi-cohort analysis)
  # This allows each stratum to have its own baseline hazard function
  if (!is.null(config$STRATA) && config$STRATA != "") {
    formula_str <- paste0(formula_str, " + strata(", config$STRATA, ")")
  }
  
  message("    Model formula: ", formula_str)
  
  # -------------------------------------------------------------------------
  # STEP 2: Fit the Cox Proportional Hazards Model
  # -------------------------------------------------------------------------
  # Using tryCatch to handle potential errors gracefully
  
  fit <- tryCatch({
    coxph(as.formula(formula_str), data = data)
  }, error = function(e) {
    message("    ! Model fitting failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # -------------------------------------------------------------------------
  # STEP 3: Extract and Format Results
  # -------------------------------------------------------------------------
  # Using broom::tidy() to create a clean results table
  # exponentiate = TRUE converts log(HR) to HR
  
  res_table <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    # Keep only rows related to the exposure variable
    filter(grepl(exposure_var, term)) %>%
    mutate(
      # Format HR with 95% CI: "1.25 (1.05-1.48)"
      HR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      
      # Format p-value to 3 decimal places
      P_Value = sprintf("%.3f", p.value),
      
      # Clean up term labels (remove variable name prefix)
      Term_Label = gsub(exposure_var, "", term)
    ) %>%
    # Select and rename columns for final output
    select(Term_Label, HR_CI, P_Value)
  
  # Add meaningful column names
  colnames(res_table) <- c("Level", "HR (95% CI)", "P-value")
  
  return(list(model = fit, table = as.data.frame(res_table)))
}


# ==============================================================================
# PART 3: ANALYSIS PIPELINE EXECUTION
# ==============================================================================
# This section runs the complete analysis workflow.
# Results are printed to console and stored in result objects.
# ==============================================================================

message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("                    STARTING COX REGRESSION ANALYSIS")
message("=" %>% rep(70) %>% paste(collapse = ""))

# -----------------------------------------------------------------------------
# 3.1 Data Preprocessing: Create Lifestyle Features
# -----------------------------------------------------------------------------
message("\n>>> STEP 1: Data Preprocessing - Creating Lifestyle Features")
message("-" %>% rep(50) %>% paste(collapse = ""))

dt_clean <- process_lifestyle_features(dt_raw, vars = CONFIG$EXPOSURES)

# Display sample of processed data for verification
message("\n    Preview of processed data (first 6 rows):")
print(head(dt_clean[, c("ID", "Life_Score", "Life_Pattern", CONFIG$EXPOSURES)]))


# -----------------------------------------------------------------------------
# 3.2 MODEL A: Categorical Analysis (Risk Score Categories)
# -----------------------------------------------------------------------------
# PURPOSE: Compare hazard across discrete exposure categories
#          Reference group = 0 risk factors (healthiest)
#          Results show HR for 1, 2, 3 factors vs. 0 factors

message("\n>>> STEP 2: Model A - Categorical Analysis (Score Categories)")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    Comparing risk levels: 1, 2, 3+ factors vs. 0 factors (reference)")

res_cat <- run_cox_analysis(dt_clean, "Life_Cat", CONFIG)

if (!is.null(res_cat)) {
  message("\n    Results - Hazard Ratios by Number of Risk Factors:")
  print(res_cat$table)
}


# -----------------------------------------------------------------------------
# 3.3 MODEL B: Trend Analysis (P for Trend)
# -----------------------------------------------------------------------------
# PURPOSE: Test for linear dose-response relationship
#          Uses continuous score variable
#          HR represents change per 1-unit increase in score

message("\n>>> STEP 3: Model B - Trend Analysis (P for Trend)")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    Testing dose-response: HR per 1-unit increase in risk score")

res_trend <- run_cox_analysis(dt_clean, "Life_Score", CONFIG)

if (!is.null(res_trend)) {
  message("\n    Results - Hazard Ratio per Additional Risk Factor:")
  print(res_trend$table)
}


# -----------------------------------------------------------------------------
# 3.4 MODEL C: Pattern Analysis (Specific Combinations)
# -----------------------------------------------------------------------------
# PURPOSE: Identify which specific combinations of risk factors
#          are most harmful (e.g., "Smoke+Inactive" vs "None")

message("\n>>> STEP 4: Model C - Pattern Analysis (Specific Combinations)")
message("-" %>% rep(50) %>% paste(collapse = ""))

# Filter patterns with sufficient sample size (N >= 10) for stable estimates
pattern_counts <- table(dt_clean$Life_Pattern)
valid_patterns <- names(pattern_counts[pattern_counts >= 10])

message("    Patterns included (N >= 10): ", length(valid_patterns))
message("    Excluded patterns with small sample sizes")

dt_sub <- dt_clean %>% filter(Life_Pattern %in% valid_patterns)

res_pattern <- run_cox_analysis(dt_sub, "Life_Pattern", CONFIG)

if (!is.null(res_pattern)) {
  message("\n    Results - Hazard Ratios by Risk Factor Combination:")
  print(res_pattern$table)
}


# -----------------------------------------------------------------------------
# 3.5 Diagnostic: Proportional Hazards Assumption Test
# -----------------------------------------------------------------------------
# PURPOSE: Validate the key assumption of the Cox model
#          The hazard ratio should be constant over time
#          Global P > 0.05 suggests assumption is met

message("\n>>> STEP 5: Diagnostics - Proportional Hazards Assumption Test")
message("-" %>% rep(50) %>% paste(collapse = ""))

if (!is.null(res_cat$model)) {
  # Schoenfeld residual test
  ph_test <- cox.zph(res_cat$model)
  
  message("\n    Schoenfeld Residual Test Results:")
  print(ph_test)
  
  # Interpret the global test result
  global_p <- ph_test$table["GLOBAL", "p"]
  
  message("\n    INTERPRETATION:")
  if (global_p > 0.05) {
    message("    [PASS] Proportional hazards assumption is satisfied (Global P = ", 
            round(global_p, 3), " > 0.05)")
    message("    The Cox model results are valid.")
  } else {
    message("    [WARNING] Proportional hazards assumption may be violated (Global P = ", 
            round(global_p, 3), " < 0.05)")
    message("    Consider: time-varying coefficients, stratification, or")
    message("    restricted time windows for analysis.")
  }
}


# -----------------------------------------------------------------------------
# 3.6 Summary and Output
# -----------------------------------------------------------------------------
message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("                    ANALYSIS COMPLETED SUCCESSFULLY")
message("=" %>% rep(70) %>% paste(collapse = ""))

message("\n>>> SUMMARY OF RESULTS OBJECTS:")
message("    - res_cat$table    : Categorical analysis results")
message("    - res_trend$table  : Trend analysis results")
message("    - res_pattern$table: Pattern analysis results")
message("    - res_cat$model    : Fitted Cox model (for further diagnostics)")

message("\n>>> TO EXPORT RESULTS TO CSV:")
message('    write.csv(res_cat$table, "cox_categorical_results.csv", row.names = FALSE)')
message('    write.csv(res_pattern$table, "cox_pattern_results.csv", row.names = FALSE)')

message("\n>>> NEXT STEPS:")
message("    1. Review HR estimates and confidence intervals")
message("    2. Check P for trend for dose-response relationship")
message("    3. Identify high-risk combinations from pattern analysis")
message("    4. Consider sensitivity analyses (e.g., excluding early events)")


# ==============================================================================
# END OF SCRIPT
# ==============================================================================
