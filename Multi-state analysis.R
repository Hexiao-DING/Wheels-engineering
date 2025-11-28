# ==============================================================================
# Multi-State Survival Model with Competing Risk of Death
# For Physical, Psychological, and Cognitive Multimorbidity Analysis
# ==============================================================================
#
# PURPOSE:
#   Analyze health state transitions in longitudinal cohort studies
#   with death as a competing risk. Based on HRS/ELSA/CHARLS methodology.
#
# TWO MODELS:
#   Model 1 (Quantity): Baseline -> 2-Morbidity -> 3-Morbidity -> Death
#   Model 2 (Pattern):  Baseline -> P1P2/P1C/P2C -> P1P2C -> Death
#
# OUTPUT:
#   1. Hazard Ratios with 95% CI for each transition
#   2. Transition counts and percentages
#   3. CSV files saved to OUTPUT_DIR
#
# ==============================================================================


# ==============================================================================
# SECTION 0: USER CONFIGURATION (MODIFY HERE)
# ==============================================================================

rm(list = ls())

# --- Output directory (where CSV files will be saved) ---
OUTPUT_DIR <- getwd()
# OUTPUT_DIR <- "C:/Users/user/Desktop/Results"  # Or specify your path

# --- Data source ---
USE_SIMULATED_DATA <- TRUE    # TRUE = use simulated data, FALSE = load your file
DATA_FILE_PATH <- "your_data.csv"
SIMULATED_SAMPLE_SIZE <- 8000

# --- Variables ---
EXPOSURE_VARIABLES <- c("child_sles", "adult_sles")
CONFOUNDER_VARIABLES <- c("age", "sex")

# --- Which models to run ---
RUN_QUANTITY_MODEL <- TRUE
RUN_PATTERN_MODEL <- TRUE


# ==============================================================================
# SECTION 1: LOAD PACKAGES
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(mstate, survival, dplyr, tidyr, stringr)

message("Packages loaded.")


# ==============================================================================
# SECTION 2: REQUIRED DATA STRUCTURE
# ==============================================================================
#
# Your data must have these columns:
#
# IDENTIFIERS:
#   id              - Subject ID (integer)
#
# EXPOSURES (factor with "0" as reference):
#   child_sles      - Childhood SLEs: "0", "1", "2"
#   adult_sles      - Adulthood SLEs: "0", "1", "2"
#
# CONFOUNDERS:
#   age             - Age at baseline (continuous)
#   sex             - Sex (0/1)
#
# TIME VARIABLES (years):
#   time_p1p2       - Time to Physical-Psychological multimorbidity
#   time_p1c        - Time to Physical-Cognitive multimorbidity
#   time_p2c        - Time to Psychological-Cognitive multimorbidity
#   time_p1p2c      - Time to triple multimorbidity
#   time_death      - Time to death or censoring
#   time_2morb      - Time to any 2-morbidity (for Model 1)
#   time_3morb      - Time to 3-morbidity (for Model 1)
#
# STATUS VARIABLES (1=event, 0=censored):
#   stat_p1p2, stat_p1c, stat_p2c, stat_p1p2c, stat_death
#   stat_2morb, stat_3morb
#
# ==============================================================================


# ==============================================================================
# SECTION 3: TRANSITION MATRIX DEFINITIONS
# ==============================================================================

# Model 1: Quantity Model
# States: Baseline(1) -> Two_Morb(2) -> Three_Morb(3) -> Death(4)
# Transitions:
#   1: Baseline -> Two_Morb
#   2: Baseline -> Three_Morb
#   3: Baseline -> Death
#   4: Two_Morb -> Three_Morb
#   5: Two_Morb -> Death
#   6: Three_Morb -> Death

get_tmat_quantity <- function() {
  transMat(
    x = list(
      c(2, 3, 4),  # From Baseline
      c(3, 4),     # From Two_Morb
      c(4),        # From Three_Morb
      c()          # Death (absorbing)
    ),
    names = c("Baseline", "Two_Morb", "Three_Morb", "Death")
  )
}

# Model 2: Pattern Model
# States: Baseline(1) -> P1P2(2)/P1C(3)/P2C(4) -> P1P2C(5) -> Death(6)
# Transitions:
#   1: Baseline -> P1P2      6: P1P2 -> P1P2C     10: P2C -> P1P2C
#   2: Baseline -> P1C       7: P1P2 -> Death     11: P2C -> Death
#   3: Baseline -> P2C       8: P1C -> P1P2C      12: P1P2C -> Death
#   4: Baseline -> P1P2C     9: P1C -> Death
#   5: Baseline -> Death

get_tmat_pattern <- function() {
  transMat(
    x = list(
      c(2, 3, 4, 5, 6),  # From Baseline
      c(5, 6),           # From P1P2
      c(5, 6),           # From P1C
      c(5, 6),           # From P2C
      c(6),              # From P1P2C
      c()                # Death (absorbing)
    ),
    names = c("Baseline", "P1P2", "P1C", "P2C", "P1P2C", "Death")
  )
}


# ==============================================================================
# SECTION 4: TRANSITION STATISTICS
# ==============================================================================

calculate_transition_stats <- function(msdata, tmat, original_n) {
  trans_map <- to.trans2(tmat)
  
  trans_counts <- msdata %>%
    filter(status == 1) %>%
    group_by(trans) %>%
    summarise(N = n(), .groups = "drop")
  
  state_counts <- msdata %>%
    group_by(from) %>%
    summarise(N_at_risk = n_distinct(id), .groups = "drop")
  
  result <- trans_map %>%
    left_join(trans_counts, by = "trans") %>%
    left_join(state_counts, by = "from") %>%
    mutate(
      N = replace_na(N, 0),
      N_at_risk = replace_na(N_at_risk, 0),
      Pct_of_total = round(100 * N / original_n, 1),
      Pct_of_origin = ifelse(N_at_risk > 0, round(100 * N / N_at_risk, 1), 0)
    ) %>%
    select(
      Trans_ID = trans,
      From = fromname,
      To = toname,
      N, N_at_risk,
      Pct_of_total,
      Pct_of_origin
    )
  
  return(result)
}


# ==============================================================================
# SECTION 5: SIMULATED DATA GENERATOR
# ==============================================================================

generate_simulated_data <- function(n = 8000, seed = 2025, max_followup = 12) {
  
  message("Generating simulated data (N = ", n, ")...")
  set.seed(seed)
  
  df <- data.frame(id = 1:n)
  
  # Demographics
  df$age <- rnorm(n, mean = 65, sd = 8)
  df$sex <- rbinom(n, 1, 0.5)
  
  # Exposures
  df$child_sles <- factor(sample(c("0", "1", "2"), n, replace = TRUE, 
                                 prob = c(0.5, 0.3, 0.2)), levels = c("0", "1", "2"))
  df$adult_sles <- factor(sample(c("0", "1", "2"), n, replace = TRUE, 
                                 prob = c(0.4, 0.4, 0.2)), levels = c("0", "1", "2"))
  
  # Death
  df$time_death <- runif(n, 3, max_followup)
  df$stat_death <- rbinom(n, 1, 0.3)
  df$time_death[df$stat_death == 0] <- max_followup
  
  # Initialize multimorbidity
  for (col in c("time_p1p2", "time_p1c", "time_p2c", "time_p1p2c")) {
    df[[col]] <- df$time_death
  }
  for (col in c("stat_p1p2", "stat_p1c", "stat_p2c", "stat_p1p2c")) {
    df[[col]] <- 0
  }
  
  # Simulate disease paths
  path_types <- sample(c("A", "B", "C", "D", "E"), n, replace = TRUE, 
                       prob = c(0.15, 0.15, 0.15, 0.05, 0.50))
  
  for (i in 1:n) {
    t_end <- df$time_death[i]
    path <- path_types[i]
    
    if (path != "E" && t_end > 1.0) {
      t1 <- runif(1, 0.5, t_end - 0.5)
      
      if (path == "A") { df$time_p1p2[i] <- t1; df$stat_p1p2[i] <- 1 }
      if (path == "B") { df$time_p1c[i] <- t1; df$stat_p1c[i] <- 1 }
      if (path == "C") { df$time_p2c[i] <- t1; df$stat_p2c[i] <- 1 }
      if (path == "D") { df$time_p1p2c[i] <- t1; df$stat_p1p2c[i] <- 1 }
      
      if (path %in% c("A", "B", "C") && rbinom(1, 1, 0.5) == 1 && (t_end - t1) > 0.2) {
        t2 <- runif(1, t1 + 0.1, t_end)
        df$time_p1p2c[i] <- t2
        df$stat_p1p2c[i] <- 1
      }
    }
  }
  
  # Derive 2-morb (earliest of P1P2, P1C, P2C)
  df$time_2morb <- df$time_death
  df$stat_2morb <- 0
  
  for (i in 1:n) {
    times <- c()
    if (df$stat_p1p2[i] == 1) times <- c(times, df$time_p1p2[i])
    if (df$stat_p1c[i] == 1) times <- c(times, df$time_p1c[i])
    if (df$stat_p2c[i] == 1) times <- c(times, df$time_p2c[i])
    
    if (length(times) > 0) {
      df$time_2morb[i] <- min(times)
      df$stat_2morb[i] <- 1
    }
  }
  
  # 3-morb = P1P2C
  df$time_3morb <- df$time_p1p2c
  df$stat_3morb <- df$stat_p1p2c
  
  message("Data generated: ", n, " subjects, ", 
          sum(df$stat_death), " deaths (", round(100*mean(df$stat_death), 1), "%)")
  
  return(df)
}


# ==============================================================================
# SECTION 6: MAIN ANALYSIS FUNCTION
# ==============================================================================

run_multistate_analysis <- function(data, model_type, exposures, confounders) {
  
  message("\n=== Running ", toupper(model_type), " model ===")
  
  # Step 1: Set up transition matrix
  if (model_type == "quantity") {
    tmat <- get_tmat_quantity()
    time_cols <- c(NA, "time_2morb", "time_3morb", "time_death")
    stat_cols <- c(NA, "stat_2morb", "stat_3morb", "stat_death")
  } else {
    tmat <- get_tmat_pattern()
    time_cols <- c(NA, "time_p1p2", "time_p1c", "time_p2c", "time_p1p2c", "time_death")
    stat_cols <- c(NA, "stat_p1p2", "stat_p1c", "stat_p2c", "stat_p1p2c", "stat_death")
  }
  
  # Step 2: Prepare multi-state data
  msdata <- msprep(
    time = time_cols,
    status = stat_cols,
    data = data,
    trans = tmat,
    keep = c(exposures, confounders),
    id = data$id
  )
  message("Multi-state data: ", nrow(msdata), " rows")
  
  # Step 3: Expand covariates
  msdata_exp <- expand.covs(msdata, c(exposures, confounders), 
                            append = TRUE, longnames = FALSE)
  
  # Step 4: Build formula
  all_cols <- names(msdata_exp)
  base_cols <- c("id", "from", "to", "trans", "Tstart", "Tstop", 
                 "time", "status", exposures, confounders)
  expanded_covs <- setdiff(all_cols, base_cols)
  
  final_covs <- c()
  for (var in c(exposures, confounders)) {
    matched <- expanded_covs[str_detect(expanded_covs, fixed(var))]
    final_covs <- c(final_covs, matched)
  }
  
  formula <- as.formula(
    paste("Surv(Tstart, Tstop, status) ~ strata(trans) +", 
          paste(final_covs, collapse = " + "))
  )
  
  # Step 5: Fit Cox model
  fit <- coxph(formula, data = msdata_exp, method = "breslow")
  message("Cox model fitted.")
  
  # Step 6: Extract hazard ratios
  model_summary <- summary(fit)
  coef_df <- as.data.frame(model_summary$coefficients)
  conf_df <- as.data.frame(model_summary$conf.int)
  coef_names <- rownames(coef_df)
  
  trans_map <- to.trans2(tmat)
  all_trans <- sort(unique(msdata$trans[!is.na(msdata$trans)]))
  
  hr_table <- data.frame()
  
  for (coef_name in coef_names) {
    found_exp <- NULL; found_lvl <- NULL; found_trans <- NULL
    
    for (exp_var in exposures) {
      if (str_detect(coef_name, exp_var)) {
        found_exp <- exp_var
        
        for (tid in all_trans) {
          if (str_detect(coef_name, paste0("\\.", tid, "$")) ||
              str_detect(coef_name, paste0("\\.", tid))) {
            found_trans <- tid
            break
          }
        }
        
        levels_vec <- levels(data[[exp_var]])[-1]
        for (lvl in levels_vec) {
          if (str_detect(coef_name, lvl)) {
            found_lvl <- lvl
            break
          }
        }
      }
    }
    
    if (!is.null(found_exp) && !is.null(found_lvl) && !is.null(found_trans)) {
      hr <- coef_df[coef_name, "exp(coef)"]
      p_val <- coef_df[coef_name, "Pr(>|z|)"]
      lower <- conf_df[coef_name, "lower .95"]
      upper <- conf_df[coef_name, "upper .95"]
      
      from_state <- trans_map$fromname[trans_map$trans == found_trans]
      to_state <- trans_map$toname[trans_map$trans == found_trans]
      
      hr_table <- rbind(hr_table, data.frame(
        Exposure = found_exp,
        Level = paste0(found_lvl, " vs 0"),
        Transition = paste0(from_state, " -> ", to_state),
        HR = round(hr, 2),
        CI_Lower = round(lower, 2),
        CI_Upper = round(upper, 2),
        P = round(p_val, 4),
        Sig = ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", 
                                                  ifelse(p_val < 0.05, "*", "")))
      ))
    }
  }
  
  if (nrow(hr_table) > 0) {
    hr_table <- hr_table %>% arrange(Exposure, Level, Transition)
  }
  message("Extracted ", nrow(hr_table), " hazard ratios.")
  
  # Step 7: Transition statistics
  trans_stats <- calculate_transition_stats(msdata, tmat, nrow(data))
  
  return(list(
    hr_table = hr_table,
    transition_stats = trans_stats,
    model_fit = fit,
    tmat = tmat,
    n_total = nrow(data)
  ))
}


# ==============================================================================
# SECTION 7: EXPORT RESULTS
# ==============================================================================

export_results <- function(results, prefix, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  hr_file <- file.path(output_dir, paste0(prefix, "_HazardRatios.csv"))
  trans_file <- file.path(output_dir, paste0(prefix, "_TransitionStats.csv"))
  
  write.csv(results$hr_table, hr_file, row.names = FALSE)
  write.csv(results$transition_stats, trans_file, row.names = FALSE)
  
  message("Files saved to: ", output_dir)
  message("  - ", basename(hr_file))
  message("  - ", basename(trans_file))
}


# ==============================================================================
# SECTION 8: SUMMARY FUNCTION
# ==============================================================================

print_summary <- function(results_quantity = NULL, results_pattern = NULL) {
  
  cat("\n")
  cat("================================================================\n")
  cat("                    ANALYSIS SUMMARY                            \n")
  cat("================================================================\n")
  
  # --- QUANTITY MODEL ---
  if (!is.null(results_quantity)) {
    cat("\n")
    cat("----------------------------------------------------------------\n")
    cat("MODEL 1: QUANTITY (By Number of Conditions)\n")
    cat("----------------------------------------------------------------\n")
    cat("Total N:", results_quantity$n_total, "\n\n")
    
    cat("TRANSITION STATISTICS:\n")
    cat("  From            To              N       % of Total  % of Origin\n")
    cat("  --------------  --------------  ------  ----------  -----------\n")
    for (i in 1:nrow(results_quantity$transition_stats)) {
      row <- results_quantity$transition_stats[i, ]
      cat(sprintf("  %-14s  %-14s  %6d  %9.1f%%  %10.1f%%\n",
                  row$From, row$To, row$N, row$Pct_of_total, row$Pct_of_origin))
    }
    
    cat("\nHAZARD RATIOS:\n")
    cat("  Exposure     Level     Transition                HR    95% CI          P        \n")
    cat("  -----------  --------  ------------------------  ----  --------------  ---------\n")
    for (i in 1:nrow(results_quantity$hr_table)) {
      row <- results_quantity$hr_table[i, ]
      ci <- paste0(row$CI_Lower, "-", row$CI_Upper)
      sig <- row$Sig
      cat(sprintf("  %-11s  %-8s  %-24s  %4.2f  %-14s  %-.4f %s\n",
                  row$Exposure, row$Level, row$Transition, row$HR, ci, row$P, sig))
    }
  }
  
  # --- PATTERN MODEL ---
  if (!is.null(results_pattern)) {
    cat("\n")
    cat("----------------------------------------------------------------\n")
    cat("MODEL 2: PATTERN (By Specific Condition Combinations)\n")
    cat("----------------------------------------------------------------\n")
    cat("Total N:", results_pattern$n_total, "\n\n")
    
    cat("TRANSITION STATISTICS:\n")
    cat("  From            To              N       % of Total  % of Origin\n")
    cat("  --------------  --------------  ------  ----------  -----------\n")
    for (i in 1:nrow(results_pattern$transition_stats)) {
      row <- results_pattern$transition_stats[i, ]
      cat(sprintf("  %-14s  %-14s  %6d  %9.1f%%  %10.1f%%\n",
                  row$From, row$To, row$N, row$Pct_of_total, row$Pct_of_origin))
    }
    
    cat("\nHAZARD RATIOS:\n")
    cat("  Exposure     Level     Transition                HR    95% CI          P        \n")
    cat("  -----------  --------  ------------------------  ----  --------------  ---------\n")
    for (i in 1:nrow(results_pattern$hr_table)) {
      row <- results_pattern$hr_table[i, ]
      ci <- paste0(row$CI_Lower, "-", row$CI_Upper)
      sig <- row$Sig
      cat(sprintf("  %-11s  %-8s  %-24s  %4.2f  %-14s  %-.4f %s\n",
                  row$Exposure, row$Level, row$Transition, row$HR, ci, row$P, sig))
    }
  }
  
  # --- SIGNIFICANT FINDINGS ---
  cat("\n")
  cat("----------------------------------------------------------------\n")
  cat("SIGNIFICANT FINDINGS (P < 0.05)\n")
  cat("----------------------------------------------------------------\n")
  
  sig_findings <- data.frame()
  
  if (!is.null(results_quantity)) {
    sig_q <- results_quantity$hr_table %>% 
      filter(P < 0.05) %>%
      mutate(Model = "Quantity")
    sig_findings <- rbind(sig_findings, sig_q)
  }
  
  if (!is.null(results_pattern)) {
    sig_p <- results_pattern$hr_table %>% 
      filter(P < 0.05) %>%
      mutate(Model = "Pattern")
    sig_findings <- rbind(sig_findings, sig_p)
  }
  
  if (nrow(sig_findings) > 0) {
    cat("  Model     Exposure     Level     Transition                HR (95% CI)\n")
    cat("  --------  -----------  --------  ------------------------  ------------------\n")
    for (i in 1:nrow(sig_findings)) {
      row <- sig_findings[i, ]
      hr_ci <- paste0(row$HR, " (", row$CI_Lower, "-", row$CI_Upper, ")")
      cat(sprintf("  %-8s  %-11s  %-8s  %-24s  %s %s\n",
                  row$Model, row$Exposure, row$Level, row$Transition, hr_ci, row$Sig))
    }
  } else {
    cat("  No significant findings (P < 0.05)\n")
  }
  
  cat("\n")
  cat("================================================================\n")
  cat("                    END OF SUMMARY                              \n")
  cat("================================================================\n\n")
}


# ==============================================================================
# SECTION 9: RUN ANALYSIS
# ==============================================================================

message("\n========== MULTI-STATE MODEL ANALYSIS ==========")
message("Output directory: ", OUTPUT_DIR)

# Load or generate data
if (USE_SIMULATED_DATA) {
  my_data <- generate_simulated_data(n = SIMULATED_SAMPLE_SIZE)
} else {
  message("Loading data from: ", DATA_FILE_PATH)
  my_data <- read.csv(DATA_FILE_PATH)
  for (v in EXPOSURE_VARIABLES) {
    if (v %in% names(my_data)) {
      my_data[[v]] <- factor(my_data[[v]], levels = c("0", "1", "2"))
    }
  }
}

# Initialize results
results_quantity <- NULL
results_pattern <- NULL

# Run analyses
if (RUN_QUANTITY_MODEL) {
  results_quantity <- run_multistate_analysis(
    my_data, "quantity", EXPOSURE_VARIABLES, CONFOUNDER_VARIABLES)
}

if (RUN_PATTERN_MODEL) {
  results_pattern <- run_multistate_analysis(
    my_data, "pattern", EXPOSURE_VARIABLES, CONFOUNDER_VARIABLES)
}

# Export to CSV
message("\n========== EXPORTING RESULTS ==========")
if (RUN_QUANTITY_MODEL) export_results(results_quantity, "Quantity_Model", OUTPUT_DIR)
if (RUN_PATTERN_MODEL) export_results(results_pattern, "Pattern_Model", OUTPUT_DIR)

# Print summary
print_summary(results_quantity, results_pattern)

message("Output files saved to: ", OUTPUT_DIR)
message("Analysis complete.")


# ==============================================================================
# END OF SCRIPT
# ==============================================================================
