# ==============================================================================
# 依???包加??? (???确保已安???)
# ==============================================================================
library(tidyverse)
library(ggalluvial)
library(showtext) # ??????：用于加???高???量字体

# 加???系???字体 (???里加??? Arial，保???跨平台兼容性)
font_add_google("Roboto", "roboto") # 如果能???网，使用Google字体Roboto，非常???代
showtext_auto()

# ==============================================================================
# ??????期刊??????思??? Sankey ??????函???
# ==============================================================================

#' @title plot_elite_sankey
#' @description 
#'   融合平面??????思???的Sankey??????制函???。
#'   特???：???网膜???清晰度、??????化配色、空气感排版。
#'
#' @param data ???据框，必???包含基???和?????????列。
#' @param col_base 基???列名 (字符串)。
#' @param col_curr ??????列名 (字符串)。
#' @param title ???表主??????。
#' @param subtitle ???表副??????（用于解???N???或??????量）。
#' @return ggplot???象
plot_elite_sankey <- function(data, 
                              col_base = "Baseline", 
                              col_curr = "Followup",
                              title = "Longitudinal Trajectories",
                              subtitle = "N = 2,450 | 8-Year Follow-up") {
  
  # --- 1. ??????思???：色彩工程 (Color Engineering) ---
  # ??????：
  # - None (健康): 使用极???的冷灰色，代表"背景"和"???事???生"，不???眼。
  # - ???病 (P1/P2/C): 使用高辨???度的 NPG (Nature) 衍生色，???和度适中。
  # - 多病 (Comorbidity): 使用混合后的深色???，??????上更有"重量感"。
  # - Death (死亡): 使用深炭灰色 (不是???黑)，代表??????，??????上有沉淀感。
  
  elite_palette <- c(
    "None"    = "#E4E8F0",  # [背景色] 极淡???灰，???出??????焦???
    
    "P1"      = "#EE4C97",  # [主色1] 活力??? (Physical)
    "P2"      = "#3B86FF",  # [主色2] 科技??? (Psychological)
    "C"       = "#00C16E",  # [主色3] 翡翠??? (Cognitive)
    
    "P1P2"    = "#884CFF",  # [混合色] 紫色
    "P1C"     = "#FF9F1C",  # [混合色] 橙色
    "P2C"     = "#06D6A0",  # [混合色] 青色
    "P1P2C"   = "#9D5C63",  # [混合色] 棕???
    
    "Death"   = "#2B2D42"   # [??????色] 深炭灰 (Charcoal)
  )
  
  # --- 2. ??????排序 (Logical Ordering) ---
  # 必??????制指定 Factor Level，否??????形??????。
  # ???序：健康在???端 -> 疾病在中部 -> 死亡在底端
  order_levels <- c("None", "P1", "P2", "C", "P1P2", "P1C", "P2C", "P1P2C", "Death")
  
  # --- 3. ???据清洗 (Data Wrangling) ---
  # ??????捕???列名
  sym_base <- sym(col_base)
  sym_curr <- sym(col_curr)
  
  plot_data <- data %>%
    filter(!is.na(!!sym_base) & !is.na(!!sym_curr)) %>%
    mutate(
      axis1 = factor(!!sym_base, levels = order_levels),
      axis2 = factor(!!sym_curr, levels = order_levels)
    ) %>%
    # ????????????
    count(axis1, axis2, name = "freq")
  
  # --- 4. ??????构建 (Architecture) ---
  p <- ggplot(plot_data, aes(y = freq, axis1 = axis1, axis2 = axis2)) +
    
    # [???????????? A]: 流?????? (The Flow)
    # 使用 sigmoid 曲???，使得流???更???滑，像??????一???。
    # ??????：color = "white", size = 0.8 -> 加粗的白色描???。
    # ???在平面??????中叫 "Negative Space Separation"，能???重???的???色???清晰分离。
    geom_alluvium(aes(fill = axis1), 
                  width = 1/5, 
                  curve_type = "sigmoid", 
                  alpha = 0.85,          # 提高不透明度，使???色更???，更有???感
                  color = "white",       # 粗白???
                  size = 0.6) +
    
    # [???????????? B]: ??????方??? (The Nodes)
    # 不填充???色，只留一???干???的深色框，或者填白色。
    # ???里??????填白色 + 深色??????，???造"???囊"感。
    geom_stratum(width = 1/5, 
                 fill = "white", 
                 color = "#2B2D42", 
                 size = 0.4) +
    
    # [???????????? C]: 文本排版 (Typography)
    # ??????示 Stratum 的名字。
    # 使用 Roboto/Arial 字体。
    geom_text(stat = "stratum", 
              aes(label = after_stat(stratum)), 
              family = "roboto", 
              fontface = "bold", 
              size = 3.5, 
              color = "#2B2D42") +
    
    # --- 5. ???尺与映射 (Scales) ---
    scale_x_discrete(limits = c("Baseline Assessment", "8-Year Follow-up"), 
                     expand = c(0.08, 0.08)) + # 左右留白，不???格
    scale_fill_manual(values = elite_palette) +
    
    # --- 6. 极???主???修??? (Minimalist Theme) ---
    labs(title = title, 
         subtitle = subtitle,
         y = NULL, x = NULL) +
    
    theme_minimal(base_family = "roboto") +
    theme(
      # 去除??????
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none", # ???种???不需要???例，??????就在???上
      
      # ??????排版 (左??????是???代????????????，或者居中)
      plot.title = element_text(size = 18, face = "bold", color = "#1A1A1A", margin = margin(b=5)),
      plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b=20)),
      
      # X??????????????? (像??????的小??????)
      axis.text.x = element_text(size = 11, face = "bold", color = "#1A1A1A", vjust = 5),
      
      # 整体背景
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  return(p)
}

# ==============================================================================
# 3. ?????????行
# ==============================================================================

# --- 生成高仿真???据 ---
set.seed(123)
N <- 3000

# 定?????????空???
states_base <- c("None", "P1", "P2", "C")
states_follow <- c("None", "P1", "P2", "C", "P1P2", "P1C", "P2C", "P1P2C", "Death")

# ???建???据框
demo_df <- data.frame(ID = 1:N)

# 1. 基???分布：大部分人健康
demo_df$Baseline <- sample(states_base, N, replace = TRUE, prob = c(0.55, 0.2, 0.15, 0.1))

# 2. ???????????? (模???疾病???展)
demo_df$Followup <- sapply(demo_df$Baseline, function(b) {
  if(b == "None") {
    # 健康人：80%保持，20%得???病或死
    sample(c("None", "P1", "P2", "Death"), 1, prob = c(0.8, 0.1, 0.05, 0.05))
  } else if (b == "P1") {
    # ???体病：可能保持，可能合并心理(P1P2)，可能死
    sample(c("P1", "P1P2", "P1C", "Death"), 1, prob = c(0.5, 0.2, 0.1, 0.2))
  } else if (b == "P2") {
    sample(c("P2", "P1P2", "P2C", "Death"), 1, prob = c(0.5, 0.2, 0.1, 0.2))
  } else {
    # ???知受???：死亡率高
    sample(c("C", "P1C", "P2C", "Death"), 1, prob = c(0.4, 0.1, 0.1, 0.4))
  }
})

# --- ???用??????函??? ---
final_plot <- plot_elite_sankey(
  data = demo_df,
  col_base = "Baseline",
  col_curr = "Followup",
  title = "Evolution of Multimorbidity Clusters",
  subtitle = paste0("Cohort Study (N=", N, ") | 2011 - 2020")
)

# --- 展示与保存 ---
print(final_plot)

# 保存建???：PDF格式能保留矢量??????，最适合投稿
# ggsave("Figure2_Nature_Design.pdf", final_plot, width = 10, height = 7, device = cairo_pdf)
