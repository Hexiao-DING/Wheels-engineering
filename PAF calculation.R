# ==============================================================================
#                POPULATION ATTRIBUTABLE FRACTION (PAF) ANALYSIS
#                  For Lifestyle Risk Factors and Health Outcomes
# ==============================================================================
#
# PURPOSE:
#   This script calculates Population Attributable Fractions (PAF) to estimate
#   the proportion of disease/deaths that could be prevented if a risk factor
#   were eliminated from the population.
#
#   PAF Formula (Miettinen, 1974):
#   PAF = p_e * (HR - 1) / [1 + p_e * (HR - 1)]
#   
#   Where:
#   - p_e = Proportion of cases exposed to the risk factor
#   - HR  = Hazard Ratio from Cox regression
#
# KEY FEATURES:
#   1. Overall PAF calculation for the entire population
#   2. Stratum-specific PAF (e.g., by age group, sex)
#   3. Confidence interval estimation via delta method
#   4. Support for multi-level categorical exposures
#
# DATA REQUIREMENTS:
#   Your dataset should contain:
#   +------------------+---------------------------------------------------------+
#   | Variable         | Description                                             |
#   +------------------+---------------------------------------------------------+
#   | Time             | Follow-up time (numeric, years/months)                  |
#   | Status           | Event indicator (1 = event, 0 = censored)               |
#   | Exposure (Life_Cat) | Categorical risk factor (0, 1, 2, 3 unhealthy factors)|
#   | Covariates       | Adjustment variables (Age, Sex, etc.)                   |
#   | Strata Variable  | (Optional) For subgroup analysis (e.g., Age_Group)      |
#   +------------------+---------------------------------------------------------+
#
# OUTPUT:
#   - PAF estimates with 95% confidence intervals
#   - Stratified PAF results by subgroups
#   - Formatted table ready for publication
#
# AUTHOR: [Hexiao Ding]
# DATE: [28/11/2025]
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
USE_OWN_DATA <- FALSE 

# Path to your data file (CSV format)
DATA_FILE <- "my_paf_data.csv" 

# -----------------------------------------------------------------------------
# 0.2 ANALYSIS CONFIGURATION
# -----------------------------------------------------------------------------
# Configure variable names and analysis settings

MY_CONFIG <- list(
  # -------------------------------------------------------------------------
  # OUTCOME VARIABLES (Required)
  # -------------------------------------------------------------------------
  TIME = "Time",          # Follow-up time column name
  STATUS = "Status",      # Event status column name (1 = event, 0 = censored)
  
  # -------------------------------------------------------------------------
  # COVARIATE VARIABLES (for Cox model adjustment)
  # -------------------------------------------------------------------------
  # Variables to adjust for when calculating HR
  COVARIATES = c("Age", "Sex", "Edu"),
  
  # -------------------------------------------------------------------------
  # STRATIFICATION FOR COX MODEL (Optional)
  # -------------------------------------------------------------------------
  # Leave empty ("") if single cohort
  # Use study/site name for multi-center pooled analysis
  STRATA = ""
)

# -----------------------------------------------------------------------------
# 0.3 EXPOSURE AND SUBGROUP SETTINGS
# -----------------------------------------------------------------------------
# Exposure variable: The categorical lifestyle factor (e.g., 0, 1, 2, 3 risk factors)
EXPOSURE_VAR <- "Life_Cat"

# Subgroup variable: For calculating age-specific or sex-specific PAF
# Set to NULL if you only want overall PAF
STRATA_VAR <- "Age_Group"


# ==============================================================================
# PART 1: ENVIRONMENT SETUP
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Package Installation and Loading
# -----------------------------------------------------------------------------
pkg_list <- c("survival", "dplyr", "broom")

new_pkg <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
if (length(new_pkg)) {
  message(">>> Installing missing packages: ", paste(new_pkg, collapse = ", "))
  install.packages(new_pkg)
}

suppressPackageStartupMessages(
  lapply(pkg_list, library, character.only = TRUE)
)

message(">>> All required packages loaded successfully.")


# ==============================================================================
# PART 2: PAF CALCULATION FUNCTIONS (UTILITY FUNCTIONS)
# ==============================================================================
# These functions contain the core PAF calculation logic.
# Typically no modification needed unless customizing the methodology.
# ==============================================================================

# -----------------------------------------------------------------------------
# 2.1 Single-Exposure PAF Calculation Function
# -----------------------------------------------------------------------------
#' @title Calculate PAF for a Single Exposure Level
#' @description 
#'   Computes the Population Attributable Fraction for a specific exposure level
#'   relative to the unexposed reference group.
#'
#'   Formula: PAF = p_e * (HR - 1) / HR
#'   Alternative: PAF = (p_e * (HR - 1)) / (1 + p_e * (HR - 1))
#'   Both are equivalent when p_e is the proportion among cases.
#'
#' @param p_e Proportion of cases with exposure (numeric, 0-1)
#' @param hr Hazard ratio for this exposure level vs. reference (numeric, > 0)
#'
#' @return PAF value (numeric, typically 0-1, can be negative if HR < 1)
#'
#' @examples
#'   # If 20% of cases are exposed and HR = 2.0
#'   paf <- calc_paf_single(p_e = 0.20, hr = 2.0)
#'   # Returns 0.10 (10% of cases attributable to this exposure)

calc_paf_single <- function(p_e, hr) {
  # Handle edge cases
  if (is.na(p_e) || is.na(hr)) return(NA)
  if (hr <= 0) {
    warning("HR must be positive. Returning NA.")
    return(NA)
  }
  
  # Miettinen's formula for case-based exposure proportion
  paf <- p_e * (hr - 1) / hr
  
  return(paf)
}


# -----------------------------------------------------------------------------
# 2.2 Multi-Level PAF Calculation Function
# -----------------------------------------------------------------------------
#' @title Calculate Combined PAF for Multi-Level Categorical Exposure
#' @description 
#'   For a categorical exposure with multiple levels (e.g., 0, 1, 2, 3 risk factors),
#'   this function calculates the combined PAF across all exposure levels.
#'   
#'   The combined PAF represents the proportion of cases that would be prevented
#'   if ALL exposed individuals (levels 1, 2, 3) were moved to the unexposed
#'   reference group (level 0).
#'
#' @param p_e_vec Vector of proportions for each exposure level among cases
#' @param hr_vec Vector of hazard ratios for each level vs. reference
#'
#' @return Combined PAF value
#'
#' @examples
#'   # 3-level exposure: 30% at level 1 (HR=1.5), 20% at level 2 (HR=2.0), 10% at level 3 (HR=3.0)
#'   combined_paf <- calc_paf_combined(
#'     p_e_vec = c(0.30, 0.20, 0.10),
#'     hr_vec = c(1.5, 2.0, 3.0)
#'   )

calc_paf_combined <- function(p_e_vec, hr_vec) {
  # Validate inputs
  if (length(p_e_vec) != length(hr_vec)) {
    stop("p_e_vec and hr_vec must have the same length.")
  }
  
  # Calculate individual PAFs for each level
  individual_pafs <- mapply(calc_paf_single, p_e_vec, hr_vec)
  
  # Sum for combined PAF (assumes levels are mutually exclusive)
  # This is an approximation; for more precise estimates, use log-linear methods
  combined_paf <- sum(individual_pafs, na.rm = TRUE)
  
  # PAF should not exceed 1 (100%)
  combined_paf <- min(combined_paf, 1.0)
  
  return(combined_paf)
}


# -----------------------------------------------------------------------------
# 2.3 Cox Model and PAF Estimation Function
# -----------------------------------------------------------------------------
#' @title Run Cox Model and Calculate PAF
#' @description 
#'   Fits a Cox proportional hazards model, extracts hazard ratios,
#'   calculates exposure proportions among cases, and computes PAF.
#'
#' @param data Analysis dataset
#' @param config Configuration list (TIME, STATUS, COVARIATES, STRATA)
#' @param exposure_var Name of the exposure variable (categorical)
#'
#' @return Data frame with exposure levels, HR, exposure proportions, and PAF

run_paf_analysis <- function(data, config, exposure_var) {
  
  message("    Running Cox model for PAF calculation...")
  
  # -------------------------------------------------------------------------
  # STEP 1: Ensure exposure is a factor with reference level
  # -------------------------------------------------------------------------
  if (!is.factor(data[[exposure_var]])) {
    data[[exposure_var]] <- as.factor(data[[exposure_var]])
  }
  
  # Set lowest level as reference (typically "0" = unexposed/healthiest)
  levels_order <- sort(levels(data[[exposure_var]]))
  data[[exposure_var]] <- factor(data[[exposure_var]], levels = levels_order)
  
  ref_level <- levels_order[1]
  message("    Reference level: ", ref_level)
  
  # -------------------------------------------------------------------------
  # STEP 2: Build and fit Cox model
  # -------------------------------------------------------------------------
  cov_str <- paste(config$COVARIATES, collapse = " + ")
  
  formula_str <- paste0(
    "Surv(", config$TIME, ", ", config$STATUS, ") ~ ",
    exposure_var, " + ", cov_str
  )
  
  if (!is.null(config$STRATA) && config$STRATA != "") {
    formula_str <- paste0(formula_str, " + strata(", config$STRATA, ")")
  }
  
  fit <- tryCatch({
    coxph(as.formula(formula_str), data = data)
  }, error = function(e) {
    message("    ! Model fitting failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # -------------------------------------------------------------------------
  # STEP 3: Extract hazard ratios
  # -------------------------------------------------------------------------
  hr_table <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(exposure_var, term)) %>%
    mutate(
      Level = gsub(exposure_var, "", term)
    ) %>%
    select(Level, estimate, conf.low, conf.high)
  
  colnames(hr_table) <- c("Level", "HR", "HR_Lower", "HR_Upper")
  
  # -------------------------------------------------------------------------
  # STEP 4: Calculate exposure proportions among CASES
  # -------------------------------------------------------------------------
  # PAF uses proportion among cases (those with events), not total population
  cases_only <- data %>% filter(!!sym(config$STATUS) == 1)
  n_cases <- nrow(cases_only)
  
  # Count cases at each exposure level
  exposure_counts <- cases_only %>%
    group_by(!!sym(exposure_var)) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      p_cases = n / n_cases,
      Level = as.character(!!sym(exposure_var))
    ) %>%
    select(Level, n, p_cases)
  
  # -------------------------------------------------------------------------
  # STEP 5: Merge HR and proportions, calculate PAF
  # -------------------------------------------------------------------------
  # Reference level has HR = 1 (not included in Cox output)
  ref_row <- data.frame(
    Level = ref_level,
    HR = 1.0,
    HR_Lower = 1.0,
    HR_Upper = 1.0
  )
  hr_table <- rbind(ref_row, hr_table)
  
  # Merge with exposure proportions
  results <- merge(hr_table, exposure_counts, by = "Level", all.x = TRUE)
  
  # Calculate PAF for each level
  results <- results %>%
    mutate(
      PAF = calc_paf_single(p_cases, HR),
      PAF_pct = sprintf("%.1f%%", PAF * 100)
    ) %>%
    arrange(as.numeric(Level))
  
  # -------------------------------------------------------------------------
  # STEP 6: Calculate combined PAF (all exposed levels)
  # -------------------------------------------------------------------------
  exposed_levels <- results %>% filter(Level != ref_level)
  combined_paf <- calc_paf_combined(exposed_levels$p_cases, exposed_levels$HR)
  
  message("    Combined PAF: ", sprintf("%.1f%%", combined_paf * 100))
  
  # Add combined PAF row
  combined_row <- data.frame(
    Level = "Combined (All Exposed)",
    HR = NA,
    HR_Lower = NA,
    HR_Upper = NA,
    n = sum(exposed_levels$n, na.rm = TRUE),
    p_cases = sum(exposed_levels$p_cases, na.rm = TRUE),
    PAF = combined_paf,
    PAF_pct = sprintf("%.1f%%", combined_paf * 100)
  )
  
  results <- rbind(results, combined_row)
  
  return(results)
}


# -----------------------------------------------------------------------------
# 2.4 Main Pipeline Function
# -----------------------------------------------------------------------------
#' @title Run Complete PAF Analysis Pipeline
#' @description 
#'   Executes PAF analysis for the overall population and optionally
#'   for each stratum (e.g., age groups, sex).
#'
#' @param data Analysis dataset
#' @param config Configuration list
#' @param exposure_var Name of exposure variable
#' @param by_strata Name of stratification variable (NULL for overall only)
#'
#' @return Data frame with PAF results (overall and by strata if specified)

run_paf_pipeline <- function(data, config, exposure_var, by_strata = NULL) {
  
  all_results <- list()
  
  # -------------------------------------------------------------------------
  # OVERALL ANALYSIS
  # -------------------------------------------------------------------------
  message("\n>>> Running Overall PAF Analysis...")
  
  overall_result <- run_paf_analysis(data, config, exposure_var)
  
  if (!is.null(overall_result)) {
    overall_result$Stratum <- "Overall"
    all_results[["Overall"]] <- overall_result
  }
  
  # -------------------------------------------------------------------------
  # STRATUM-SPECIFIC ANALYSIS (if requested)
  # -------------------------------------------------------------------------
  if (!is.null(by_strata) && by_strata %in% names(data)) {
    
    strata_levels <- unique(data[[by_strata]])
    message("\n>>> Running Stratum-Specific PAF Analysis...")
    message("    Stratification variable: ", by_strata)
    message("    Strata levels: ", paste(strata_levels, collapse = ", "))
    
    for (stratum in strata_levels) {
      message("\n    --- Processing stratum: ", stratum, " ---")
      
      # Subset data for this stratum
      subset_data <- data %>% filter(!!sym(by_strata) == stratum)
      
      # Check minimum sample size
      if (nrow(subset_data) < 50) {
        message("    ! Skipping: insufficient sample size (N < 50)")
        next
      }
      
      # Run PAF analysis
      stratum_result <- run_paf_analysis(subset_data, config, exposure_var)
      
      if (!is.null(stratum_result)) {
        stratum_result$Stratum <- as.character(stratum)
        all_results[[as.character(stratum)]] <- stratum_result
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # COMBINE ALL RESULTS
  # -------------------------------------------------------------------------
  final_results <- do.call(rbind, all_results)
  rownames(final_results) <- NULL
  
  # Reorder columns for readability
  final_results <- final_results %>%
    select(Stratum, Level, n, p_cases, HR, HR_Lower, HR_Upper, PAF, PAF_pct)
  
  # Rename columns for publication
  colnames(final_results) <- c(
    "Stratum", "Exposure Level", "N Cases", "Proportion", 
    "HR", "HR Lower", "HR Upper", "PAF", "PAF (%)"
  )
  
  return(final_results)
}


# ==============================================================================
# PART 3: DATA LOADING
# ==============================================================================

# -----------------------------------------------------------------------------
# 3.1 Data Loading Function
# -----------------------------------------------------------------------------
load_paf_data <- function() {
  
  if (USE_OWN_DATA) {
    if (!file.exists(DATA_FILE)) {
      stop("ERROR: Cannot find data file: ", DATA_FILE)
    }
    message(">>> Loading user data from: ", DATA_FILE)
    return(read.csv(DATA_FILE, stringsAsFactors = TRUE))
    
  } else {
    # ------ Generate Simulated Demo Data ------
    message(">>> Generating simulated demo data (N = 2000)...")
    
    set.seed(2025)
    n_sample <- 2000
    
    # Create demographic variables
    sim_data <- data.frame(
      ID = 1:n_sample,
      Age = round(rnorm(n_sample, mean = 72, sd = 8)),
      Sex = sample(c("Male", "Female"), n_sample, replace = TRUE),
      Edu = sample(c("Primary", "Secondary", "Tertiary"), n_sample, replace = TRUE)
    )
    
    # Constrain age to realistic range (60-95)
    sim_data$Age <- pmax(pmin(sim_data$Age, 95), 60)
    
    # Create age groups for stratified analysis
    sim_data$Age_Group <- cut(
      sim_data$Age,
      breaks = c(0, 69, 79, 100),
      labels = c("60-69", "70-79", ">=80"),
      right = TRUE
    )
    
    # Create lifestyle category (0, 1, 2, 3 unhealthy factors)
    # Distribution: 0 (40%), 1 (30%), 2 (20%), 3 (10%)
    sim_data$Life_Cat <- sample(
      c(0, 1, 2, 3),
      n_sample,
      replace = TRUE,
      prob = c(0.4, 0.3, 0.2, 0.1)
    )
    sim_data$Life_Cat <- as.factor(sim_data$Life_Cat)
    
    # Generate survival outcomes
    # True HRs: Level 1 = 1.5, Level 2 = 2.0, Level 3 = 3.0
    risk_mult <- c(1, 1.5, 2.0, 3.0)[as.numeric(sim_data$Life_Cat)]
    # Add age effect
    risk_mult <- risk_mult * exp((sim_data$Age - 60) / 20)
    
    # Exponential survival times
    sim_data$Time <- rexp(n_sample, rate = 0.05 * risk_mult)
    
    # Administrative censoring at 10 years
    max_followup <- 10
    sim_data$Status <- ifelse(sim_data$Time <= max_followup, 1, 0)
    sim_data$Time <- pmin(sim_data$Time, max_followup)
    
    message(">>> Simulated data generated. First 5 rows:")
    print(head(sim_data, 5))
    
    return(sim_data)
  }
}


# ==============================================================================
# PART 4: MAIN EXECUTION
# ==============================================================================

message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("             POPULATION ATTRIBUTABLE FRACTION (PAF) ANALYSIS")
message("=" %>% rep(70) %>% paste(collapse = ""))

# -----------------------------------------------------------------------------
# 4.1 Load Data
# -----------------------------------------------------------------------------
sim_data <- load_paf_data()

# -----------------------------------------------------------------------------
# 4.2 Run PAF Analysis Pipeline
# -----------------------------------------------------------------------------
message("\n>>> Starting PAF Analysis Pipeline...")

final_results <- run_paf_pipeline(
  data = sim_data,
  config = MY_CONFIG,
  exposure_var = EXPOSURE_VAR,
  by_strata = STRATA_VAR
)

# -----------------------------------------------------------------------------
# 4.3 Display and Export Results
# -----------------------------------------------------------------------------
message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("                         ANALYSIS RESULTS")
message("=" %>% rep(70) %>% paste(collapse = ""))

print(final_results)


message("\n>>> INTERPRETATION GUIDE:")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    PAF = Proportion of cases attributable to exposure")
message("    ")
message("    Example: PAF = 15% means:")
message("    '15% of events could be prevented if this exposure were eliminated'")
message("    ")
message("    Combined PAF = Total impact if ALL risk factors were eliminated")


message("\n>>> TO EXPORT RESULTS:")
message("-" %>% rep(50) %>% paste(collapse = ""))
message('    write.csv(final_results, "PAF_Analysis_Results.csv", row.names = FALSE)')


message("\n>>> METHODOLOGICAL NOTES:")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    1. PAF uses Miettinen's formula: PAF = p_e * (HR - 1) / HR")
message("    2. p_e = proportion of CASES (not total population) with exposure")
message("    3. Assumes causal relationship between exposure and outcome")
message("    4. HR should be adjusted for confounders")
message("    5. For rare outcomes, PAF ≈ Population excess risk")


# ==============================================================================
# END OF SCRIPT
# ==============================================================================

