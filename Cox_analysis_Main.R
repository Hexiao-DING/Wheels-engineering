# ==============================================================================
# ----------------------  PART 0: 全局配置 (USER CONFIGURATION) ----------------
# ==============================================================================
# [???明]: ???是您唯一需要修改的部分。???此???替??????您自己的???据和???量名即可。
# ==============================================================================

# 1. ???据源???置
# 如果使用自己的???据，??????置??? TRUE，并填???文件名
USE_OWN_DATA <- FALSE 
DATA_FILE    <- "my_cohort_data.csv" 

# 2. ???量映射 (Variable Mapping)
# ??????引??????的名???替??????您 CSV 文件中??????的列名
CONFIG <- list(
  # --- ???局???量 ---
  TIME   = "Time",    # ???????????? (年/月)
  STATUS = "Status",  # ???局?????? (1=???生事件, 0=???失)
  
  # --- 暴露???量 (生活方式因子) ---
  # 注意: 必???是二分??????量 (0=健康/???, 1=??????/有)
  EXPOSURES = c("Smoke", "Alcohol", "Inactive"), 
  
  # --- ??????量 (用于???整) ---
  # [cite_start]依据文???: 年???、性???、教育 [cite: 22]
  COVARIATES = c("Age", "Sex", "Edu"),
  
  # --- 分??????量 (多???列研究必需) ---
  # [cite_start]依据文???: 使用分??? Cox 模型???理多???列异???性 [cite: 15, 19]
  STRATA = "Study"    
)

# ==============================================================================
# ----------------------  PART 1: ???境准???与???据加??? ---------------------------
# ==============================================================================

# 1.1 自???加???/安???依???包
pkg_list <- c("survival", "dplyr", "broom", "stringr")
new_pkg  <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
if (length(new_pkg)) install.packages(new_pkg)
suppressPackageStartupMessages(lapply(pkg_list, library, character.only = TRUE))

# 1.2 ???据加?????????
load_data <- function() {
  if (USE_OWN_DATA) {
    if (!file.exists(DATA_FILE)) stop("??????: 找不到文件 ", DATA_FILE)
    message(">>> 正在加???用??????据: ", DATA_FILE)
    return(read.csv(DATA_FILE))
  } else {
    message(">>> 正在生成模???演示???据 (N=2000)...")
    set.seed(2023)
    n <- 2000
    dt <- data.frame(
      ID = 1:n,
      Study = sample(c("HRS", "ELSA", "CHARLS"), n, replace = TRUE),
      Age = round(rnorm(n, 65, 10), 0),
      Sex = factor(sample(c(0, 1), n, replace = TRUE), labels = c("Female", "Male")),
      Edu = factor(sample(c(1, 2, 3), n, replace = TRUE), labels = c("Low", "Mid", "High")),
      Smoke = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
      Alcohol = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
      Inactive = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))
    )
    # 模??????局: ??????越高，??????越短
    risk_score <- rowSums(dt[, c("Smoke", "Alcohol", "Inactive")])
    dt$Time <- rexp(n, rate = 0.02 * exp(0.5 * risk_score))
    dt$Status <- ifelse(dt$Time < 9, 1, 0)
    dt$Time <- pmin(dt$Time, 9)
    return(dt)
  }
}

# ???行加???
dt_raw <- load_data()

# ==============================================================================
# ----------------------  PART 2: 通用分析函???封??? -----------------------------
# ==============================================================================

#' @title 特征工程：生活方式???据???理
#' @description 自??????算?????????分(Score)并生成???合??????(Pattern)
#' @param data ???入???据框
#' @param vars 暴露???量名向量
#' @return 清洗后的???据框
process_lifestyle_features <- function(data, vars) {
  # ???查???量是否存在
  if (!all(vars %in% names(data))) stop("??????: ???据中找不到指定的暴露???量。")
  
  df <- data
  
  # 1. ???算累?????????分??? (Score)
  df$Life_Score <- rowSums(df[, vars], na.rm = TRUE)
  
  # 2. 生成分??????量 (Factor化) - 用于 Dose-Response 分析
  # 自???根据分???的范???生成 Level
  df$Life_Cat <- factor(df$Life_Score, levels = sort(unique(df$Life_Score)))
  
  # 3. 生成???合模式 (Pattern) - 用于具体???合分析
  # ??????: ???值???1的列名拼接，例如 "Smoke+Inactive"
  df$Life_Pattern <- apply(df[, vars], 1, function(row) {
    risk_factors <- names(row)[row == 1]
    if (length(risk_factors) == 0) return("None")
    return(paste(risk_factors, collapse = "+"))
  })
  
  # ???置 "None" ????????????照??? (Reference Level)
  df$Life_Pattern <- factor(df$Life_Pattern)
  if ("None" %in% levels(df$Life_Pattern)) {
    df$Life_Pattern <- relevel(df$Life_Pattern, ref = "None")
  }
  
  return(df)
}

#' @title 核心??????：Pooled Cox 回???模型
#' @description 自???构建公式、???行模型并???出整洁的 HR 表格
#' @param data 分析???据集
#' @param exposure_var ???前要分析的暴露???量名
#' @param config 配置列表(包含 Time, Status, Covariates, Strata)
#' @return List(model???象, ???果表格dataframe)
run_cox_analysis <- function(data, exposure_var, config) {
  
  # 1. ??????构建回???公式
  # 公式: Surv(Time, Status) ~ Exposure + Covariates + strata(Study)
  cov_str <- paste(config$COVARIATES, collapse = " + ")
  
  # 基???公式
  formula_str <- paste0(
    "Surv(", config$TIME, ", ", config$STATUS, ") ~ ", 
    exposure_var, " + ", cov_str
  )
  
  # 添加分??? (如果有)
  if (!is.null(config$STRATA) && config$STRATA != "") {
    formula_str <- paste0(formula_str, " + strata(", config$STRATA, ")")
  }
  
  # 2. ???行 Cox 模型
  # 使用 tryCatch 防止某???稀有???合???致程序崩???
  fit <- tryCatch({
    coxph(as.formula(formula_str), data = data)
  }, error = function(e) {
    message("  ! 模型???合失???: ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # 3. ???果整理 (Broom Tidy)
  res_table <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(exposure_var, term)) %>% # 只提取暴露相???的行
    mutate(
      # 格式化: 1.25 (1.05-1.48)
      HR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
      P_Value = sprintf("%.3f", p.value),
      Term_Label = gsub(exposure_var, "", term) # 去掉???量名前???，只留 Level
    ) %>%
    select(Term_Label, HR_CI, P_Value) # 只保留核心列
  
  return(list(model = fit, table = as.data.frame(res_table)))
}

# ==============================================================================
# ----------------------  PART 3: ???行分析流水??? (PIPELINE) --------------------
# ==============================================================================

# 3.1 ???据清洗与特征构建
message("\n>>> STEP 1: ???据清洗与特征构建...")
dt_clean <- process_lifestyle_features(dt_raw, vars = CONFIG$EXPOSURES)

# 打印???据快照，确保??????正确
print(head(dt_clean[, c("ID", "Life_Score", "Life_Pattern", CONFIG$EXPOSURES)]))


# 3.2 模型 A: 累???效???分析 (Categorical Score)
# -----------------------------------------------------------
# 目的: 比?????????因素?????? (1???, 2???, 3??? vs 0???) 的??????差异
message("\n>>> STEP 2: ???行模型 A [累???效??? - 分???]")
res_cat <- run_cox_analysis(dt_clean, "Life_Cat", CONFIG)
print(res_cat$table)


# 3.3 模型 B: ???????????? (P for Trend)
# -----------------------------------------------------------
# 目的: ???分??????????????????量，每增加1???因素，??????增加多少?
message("\n>>> STEP 3: ???行模型 B [???????????? - ??????]")
res_trend <- run_cox_analysis(dt_clean, "Life_Score", CONFIG)
print(res_trend$table)


# 3.4 模型 C: 具体???合模式分析 (Pattern Analysis)
# -----------------------------------------------------------
# 目的: 探索 "吸???+???酒" 具体???合的??????
message("\n>>> STEP 4: ???行模型 C [具体???合模式]")

# [??????]: ???保留???本量 >= 10 的???合，防止模型不收???
pattern_counts <- table(dt_clean$Life_Pattern)
valid_patterns <- names(pattern_counts[pattern_counts >= 10])
dt_sub <- dt_clean %>% filter(Life_Pattern %in% valid_patterns)

message("  - 已??????掉???本量不足 10 的稀有???合")
res_pattern <- run_cox_analysis(dt_sub, "Life_Pattern", CONFIG)
print(res_pattern$table)


# 3.5 ???量控制: 比例??????假????????? (PH Assumption)
# -----------------------------------------------------------
# 目的: ?????? Cox 模型的前提假???是否成立 (P > 0.05 ???佳)
message("\n>>> STEP 5: ???量控制 [PH 假?????????]")

if (!is.null(res_cat$model)) {
  ph_test <- cox.zph(res_cat$model)
  print(ph_test)
  
  # ??????的自???解???
  global_p <- ph_test$table["GLOBAL", "p"]
  if (global_p > 0.05) {
    message("  [Result]: 模型???足比例??????假??? (Global P > 0.05), ???果可靠。")
  } else {
    message("  [Warning]: 模型可能???反比例??????假??? (Global P < 0.05), ??????查。")
  }
}

message("\n>>> 全部任??????行完???。")