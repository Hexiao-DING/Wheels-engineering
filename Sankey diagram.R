# ==============================================================================
#                       SANKEY DIAGRAM FOR LONGITUDINAL TRAJECTORIES
#                       Visualizing State Transitions Over Time
# ==============================================================================
#
# PURPOSE:
#   This script creates publication-quality Sankey (alluvial) diagrams to 
#   visualize how study participants transition between health states over time.
#   
#   Common applications:
#   - Disease progression trajectories
#   - Multimorbidity cluster transitions
#   - Health behavior changes over follow-up
#   - Treatment adherence patterns
#
# KEY FEATURES:
#   1. Elegant, minimalist design suitable for Nature/Science publications
#   2. Customizable color palette for different health states
#   3. Automatic handling of state labels and proportions
#   4. Support for baseline → follow-up transitions
#
# DATA REQUIREMENTS:
#   Your dataset should contain:
#   +------------------+---------------------------------------------------------+
#   | Variable         | Description                                             |
#   +------------------+---------------------------------------------------------+
#   | ID               | Unique participant identifier                           |
#   | Baseline         | Health state at baseline (categorical)                  |
#   | Followup         | Health state at follow-up (categorical)                 |
#   +------------------+---------------------------------------------------------+
#
#   Example state categories:
#   - For multimorbidity: "None", "Physical", "Psychological", "Cognitive", "Death"
#   - For frailty: "Robust", "Pre-frail", "Frail", "Death"
#   - For disease: "Healthy", "Stage1", "Stage2", "Stage3", "Death"
#
# OUTPUT:
#   - High-quality Sankey diagram (ggplot2 object)
#   - Can be exported to PDF, PNG, or other formats
#
# AUTHOR: [Hexiao Ding]
# DATE: [28/11/2025]
# VERSION: 1.0
# ==============================================================================


# ==============================================================================
# PART 0: USER CONFIGURATION (MODIFY THIS SECTION)
# ==============================================================================
# INSTRUCTIONS: Customize colors, labels, and data settings below.
# ==============================================================================

# -----------------------------------------------------------------------------
# 0.1 DATA SOURCE SETTINGS
# -----------------------------------------------------------------------------
USE_OWN_DATA <- FALSE

# Path to your data file (CSV format)
DATA_FILE <- "my_trajectory_data.csv"

# Column names in your dataset
COL_BASELINE <- "Baseline"    # Column name for baseline state
COL_FOLLOWUP <- "Followup"    # Column name for follow-up state

# -----------------------------------------------------------------------------
# 0.2 PLOT CUSTOMIZATION
# -----------------------------------------------------------------------------
# Title and subtitle for the plot
PLOT_TITLE <- "Evolution of Multimorbidity Clusters"
PLOT_SUBTITLE <- "Cohort Study | 8-Year Follow-up"

# X-axis labels (left and right time points)
AXIS_LABEL_LEFT <- "Baseline Assessment"
AXIS_LABEL_RIGHT <- "8-Year Follow-up"

# -----------------------------------------------------------------------------
# 0.3 COLOR PALETTE CONFIGURATION
# -----------------------------------------------------------------------------
# Define colors for each health state
# Format: "State_Name" = "#HexColor"
#
# Design principles:
#   - Use muted/light colors for "healthy" or "none" states
#   - Use distinct, saturated colors for disease states
#   - Use dark colors for terminal states (e.g., Death)
#   - Ensure sufficient contrast between adjacent states
#
# Color palette inspiration:
#   - Nature Publishing Group (NPG) colors
#   - Colorblind-friendly palettes (viridis, ColorBrewer)
#   - Your institution's style guide

COLOR_PALETTE <- c(
  # No condition (reference/healthy state)
  "None"    = "#E4E8F0",  # Light gray - represents baseline/healthy
  
  # Single conditions
  "P1"      = "#EE4C97",  # Pink/Magenta - Physical health condition
  "P2"      = "#3B86FF",  # Blue - Psychological condition
  "C"       = "#00C16E",  # Green - Cognitive condition
  
  # Combined conditions (multimorbidity)
  "P1P2"    = "#884CFF",  # Purple - Physical + Psychological
  "P1C"     = "#FF9F1C",  # Orange - Physical + Cognitive
  "P2C"     = "#06D6A0",  # Teal - Psychological + Cognitive
  "P1P2C"   = "#9D5C63",  # Mauve - All three conditions
  
  # Terminal state
  "Death"   = "#2B2D42"   # Dark charcoal - Death/endpoint
)

# -----------------------------------------------------------------------------
# 0.4 STATE ORDERING (Controls visual arrangement)
# -----------------------------------------------------------------------------
# States will appear in this order from top to bottom
# Place healthiest states at top, most severe/terminal at bottom

STATE_ORDER <- c(
  "None",     # Healthiest
  "P1", 
  "P2", 
  "C",
  "P1P2", 
  "P1C", 
  "P2C", 
  "P1P2C",
  "Death"    # Terminal state at bottom
)


# ==============================================================================
# PART 1: ENVIRONMENT SETUP
# ==============================================================================

# -----------------------------------------------------------------------------
# 1.1 Package Installation and Loading
# -----------------------------------------------------------------------------
pkg_list <- c(
  "tidyverse",   # Data manipulation and ggplot2
  "ggalluvial",  # Sankey/alluvial diagrams
  "showtext"     # Custom fonts
)

new_pkg <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
if (length(new_pkg)) {
  message(">>> Installing missing packages: ", paste(new_pkg, collapse = ", "))
  install.packages(new_pkg)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggalluvial)
  library(showtext)
})

message(">>> All required packages loaded successfully.")

# -----------------------------------------------------------------------------
# 1.2 Font Configuration
# -----------------------------------------------------------------------------
# Load Google Fonts for publication-quality typography
# Options: "Roboto", "Open Sans", "Lato", "Source Sans Pro", "Montserrat"
#
# If font loading fails, the script will fall back to system fonts

tryCatch({
  font_add_google("Roboto", "roboto")
  showtext_auto()
  FONT_FAMILY <- "roboto"
  message(">>> Custom font 'Roboto' loaded successfully.")
}, error = function(e) {
  FONT_FAMILY <- "sans"
  message(">>> Note: Using system default font (Google Fonts unavailable).")
})


# ==============================================================================
# PART 2: SANKEY DIAGRAM FUNCTION
# ==============================================================================

#' @title Create Publication-Quality Sankey Diagram
#' @description 
#'   Generates an elegant Sankey (alluvial) diagram showing transitions
#'   between health states at two time points.
#'
#' @param data Data frame with participant IDs and state columns
#' @param col_base Column name for baseline state (character)
#' @param col_curr Column name for follow-up state (character)
#' @param title Plot title (character)
#' @param subtitle Plot subtitle - typically includes sample size and follow-up time
#' @param color_palette Named vector of colors for each state
#' @param state_order Character vector defining state order (top to bottom)
#' @param axis_labels Two-element vector for x-axis labels c("Baseline", "Follow-up")
#' @param font_family Font family name (default: "roboto")
#'
#' @return A ggplot2 object that can be displayed or saved
#'
#' @examples
#'   p <- plot_sankey_diagram(
#'     data = my_data,
#'     col_base = "State_T0",
#'     col_curr = "State_T1",
#'     title = "Disease Progression",
#'     subtitle = "N = 500 | 5-Year Follow-up"
#'   )
#'   print(p)
#'   ggsave("sankey_diagram.pdf", p, width = 10, height = 7)

plot_sankey_diagram <- function(data, 
                                 col_base = "Baseline", 
                                 col_curr = "Followup",
                                 title = "Longitudinal Trajectories",
                                 subtitle = "N = 2,450 | 8-Year Follow-up",
                                 color_palette = COLOR_PALETTE,
                                 state_order = STATE_ORDER,
                                 axis_labels = c(AXIS_LABEL_LEFT, AXIS_LABEL_RIGHT),
                                 font_family = "roboto") {
  
  # ---------------------------------------------------------------------------
  # STEP 1: Data Validation
  # ---------------------------------------------------------------------------
  if (!col_base %in% names(data)) {
    stop("ERROR: Baseline column '", col_base, "' not found in data.")
  }
  if (!col_curr %in% names(data)) {
    stop("ERROR: Follow-up column '", col_curr, "' not found in data.")
  }
  
  message(">>> Preparing Sankey diagram data...")
  
  # ---------------------------------------------------------------------------
  # STEP 2: Data Wrangling
  # ---------------------------------------------------------------------------
  # Convert column names to symbols for tidy evaluation
  sym_base <- sym(col_base)
  sym_curr <- sym(col_curr)
  
  # Prepare plot data
  plot_data <- data %>%
    # Remove rows with missing states
    filter(!is.na(!!sym_base) & !is.na(!!sym_curr)) %>%
    # Convert to factors with specified order
    mutate(
      axis1 = factor(!!sym_base, levels = state_order),
      axis2 = factor(!!sym_curr, levels = state_order)
    ) %>%
    # Count transitions between states
    count(axis1, axis2, name = "freq") %>%
    # Remove NA combinations (states not in state_order)
    filter(!is.na(axis1) & !is.na(axis2))
  
  n_total <- sum(plot_data$freq)
  message("    Total participants: ", format(n_total, big.mark = ","))
  message("    Unique transitions: ", nrow(plot_data))
  
  # ---------------------------------------------------------------------------
  # STEP 3: Build the Sankey Diagram
  # ---------------------------------------------------------------------------
  p <- ggplot(plot_data, aes(y = freq, axis1 = axis1, axis2 = axis2)) +
    
    # -------------------------------------------------------------------------
    # Layer 1: Flow ribbons (alluvium)
    # -------------------------------------------------------------------------
    # These are the flowing ribbons connecting states across time points
    # - fill: Color based on baseline state (axis1)
    # - curve_type: "sigmoid" creates smooth S-curves (alternatives: "linear", "cubic")
    # - alpha: Transparency for overlapping flows
    # - color: White border for visual separation
    geom_alluvium(
      aes(fill = axis1), 
      width = 1/5,              # Width of the flow at the stratum
      curve_type = "sigmoid",   # Smooth curved connections
      alpha = 0.85,             # Slight transparency for depth
      color = "white",          # White borders between flows
      linewidth = 0.6           # Border thickness
    ) +
    
    # -------------------------------------------------------------------------
    # Layer 2: State bars (stratum)
    # -------------------------------------------------------------------------
    # These are the vertical bars representing each state at each time point
    geom_stratum(
      width = 1/5,             # Bar width
      fill = "white",          # White fill for clean look
      color = "#2B2D42",       # Dark border
      linewidth = 0.4          # Border thickness
    ) +
    
    # -------------------------------------------------------------------------
    # Layer 3: State labels
    # -------------------------------------------------------------------------
    # Text labels showing state names on each bar
    geom_text(
      stat = "stratum", 
      aes(label = after_stat(stratum)), 
      family = font_family, 
      fontface = "bold", 
      size = 3.5, 
      color = "#2B2D42"
    ) +
    
    # -------------------------------------------------------------------------
    # Scale configurations
    # -------------------------------------------------------------------------
    # X-axis: Time point labels
    scale_x_discrete(
      limits = axis_labels, 
      expand = c(0.08, 0.08)   # Padding on sides
    ) +
    
    # Fill colors: Apply custom color palette
    scale_fill_manual(
      values = color_palette,
      na.value = "#CCCCCC"     # Gray for undefined states
    ) +
    
    # -------------------------------------------------------------------------
    # Labels and title
    # -------------------------------------------------------------------------
    labs(
      title = title, 
      subtitle = subtitle,
      y = NULL,                 # No y-axis label needed
      x = NULL                  # No x-axis label (using discrete labels)
    ) +
    
    # -------------------------------------------------------------------------
    # Theme: Minimalist publication style
    # -------------------------------------------------------------------------
    theme_minimal(base_family = font_family) +
    theme(
      # Remove grid lines for clean look
      panel.grid = element_blank(),
      
      # Remove y-axis elements (frequencies shown by ribbon width)
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      
      # Hide legend (colors are self-explanatory in Sankey)
      legend.position = "none",
      
      # Title styling
      plot.title = element_text(
        size = 18, 
        face = "bold", 
        color = "#1A1A1A", 
        margin = margin(b = 5)
      ),
      
      # Subtitle styling
      plot.subtitle = element_text(
        size = 10, 
        color = "#666666", 
        margin = margin(b = 20)
      ),
      
      # X-axis labels (time points)
      axis.text.x = element_text(
        size = 11, 
        face = "bold", 
        color = "#1A1A1A", 
        vjust = 5
      ),
      
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)
    )
  
  message(">>> Sankey diagram created successfully.")
  return(p)
}


# ==============================================================================
# PART 3: DATA LOADING
# ==============================================================================

#' @title Load Trajectory Data
#' @description 
#'   Loads user data from CSV or generates simulated demo data.

load_trajectory_data <- function() {
  
  if (USE_OWN_DATA) {
    if (!file.exists(DATA_FILE)) {
      stop("ERROR: Cannot find data file: ", DATA_FILE)
    }
    message(">>> Loading user data from: ", DATA_FILE)
    data <- read.csv(DATA_FILE, stringsAsFactors = TRUE)
    
    # Validate required columns
    if (!COL_BASELINE %in% names(data)) {
      stop("ERROR: Baseline column '", COL_BASELINE, "' not found.")
    }
    if (!COL_FOLLOWUP %in% names(data)) {
      stop("ERROR: Follow-up column '", COL_FOLLOWUP, "' not found.")
    }
    
    return(data)
    
  } else {
    # ------ Generate Simulated Demo Data ------
    message(">>> Generating simulated trajectory data (N = 3000)...")
    
    set.seed(123)
    N <- 3000
    
    # Define possible states at each time point
    states_base <- c("None", "P1", "P2", "C")
    states_follow <- c("None", "P1", "P2", "C", 
                       "P1P2", "P1C", "P2C", "P1P2C", "Death")
    
    # Create data frame
    demo_df <- data.frame(ID = 1:N)
    
    # Baseline: Most participants are healthy (None)
    # Distribution: None (55%), P1 (20%), P2 (15%), C (10%)
    demo_df$Baseline <- sample(
      states_base, 
      N, 
      replace = TRUE, 
      prob = c(0.55, 0.20, 0.15, 0.10)
    )
    
    # Follow-up: Transitions depend on baseline state
    # This creates realistic disease progression patterns
    demo_df$Followup <- sapply(demo_df$Baseline, function(baseline_state) {
      
      if (baseline_state == "None") {
        # Healthy at baseline: most stay healthy, some develop conditions
        sample(
          c("None", "P1", "P2", "Death"), 
          1, 
          prob = c(0.80, 0.10, 0.05, 0.05)
        )
        
      } else if (baseline_state == "P1") {
        # Physical condition: may stay same, progress, or die
        sample(
          c("P1", "P1P2", "P1C", "Death"), 
          1, 
          prob = c(0.50, 0.20, 0.10, 0.20)
        )
        
      } else if (baseline_state == "P2") {
        # Psychological condition
        sample(
          c("P2", "P1P2", "P2C", "Death"), 
          1, 
          prob = c(0.50, 0.20, 0.10, 0.20)
        )
        
      } else {
        # Cognitive condition: higher mortality
        sample(
          c("C", "P1C", "P2C", "Death"), 
          1, 
          prob = c(0.40, 0.10, 0.10, 0.40)
        )
      }
    })
    
    message(">>> Demo data generated with ", N, " participants.")
    return(demo_df)
  }
}


# ==============================================================================
# PART 4: MAIN EXECUTION
# ==============================================================================

message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("                    SANKEY DIAGRAM GENERATION")
message("=" %>% rep(70) %>% paste(collapse = ""))

# -----------------------------------------------------------------------------
# 4.1 Load Data
# -----------------------------------------------------------------------------
trajectory_data <- load_trajectory_data()

# Calculate summary statistics for subtitle
n_participants <- nrow(trajectory_data)
PLOT_SUBTITLE <- paste0("Cohort Study (N = ", 
                        format(n_participants, big.mark = ","), 
                        ") | ", AXIS_LABEL_LEFT, " to ", AXIS_LABEL_RIGHT)

# -----------------------------------------------------------------------------
# 4.2 Create Sankey Diagram
# -----------------------------------------------------------------------------
message("\n>>> Creating Sankey diagram...")

final_plot <- plot_sankey_diagram(
  data = trajectory_data,
  col_base = COL_BASELINE,
  col_curr = COL_FOLLOWUP,
  title = PLOT_TITLE,
  subtitle = PLOT_SUBTITLE,
  color_palette = COLOR_PALETTE,
  state_order = STATE_ORDER
)

# -----------------------------------------------------------------------------
# 4.3 Display Plot
# -----------------------------------------------------------------------------
message("\n>>> Displaying plot...")
print(final_plot)

# -----------------------------------------------------------------------------
# 4.4 Export Instructions
# -----------------------------------------------------------------------------
message("\n")
message("=" %>% rep(70) %>% paste(collapse = ""))
message("                         EXPORT OPTIONS")
message("=" %>% rep(70) %>% paste(collapse = ""))

message("\n>>> TO SAVE THE PLOT:")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    # High-resolution PDF (recommended for publications)")
message('    ggsave("Figure_Sankey_Trajectories.pdf", final_plot,')
message('           width = 10, height = 7, device = cairo_pdf)')
message("")
message("    # PNG format (for presentations/web)")
message('    ggsave("Figure_Sankey_Trajectories.png", final_plot,')
message('           width = 10, height = 7, dpi = 300)')

message("\n>>> CUSTOMIZATION TIPS:")
message("-" %>% rep(50) %>% paste(collapse = ""))
message("    1. Modify COLOR_PALETTE to match your condition names")
message("    2. Update STATE_ORDER to arrange states logically")
message("    3. Change PLOT_TITLE and PLOT_SUBTITLE for your study")
message("    4. Adjust width/height in ggsave() for optimal proportions")

message("\n>>> TRANSITION FREQUENCY TABLE:")
message("-" %>% rep(50) %>% paste(collapse = ""))
# Print transition counts
transition_table <- trajectory_data %>%
  count(!!sym(COL_BASELINE), !!sym(COL_FOLLOWUP)) %>%
  arrange(desc(n))
print(head(transition_table, 15))


# ==============================================================================
# PART 5: ADDITIONAL UTILITY FUNCTIONS
# ==============================================================================

#' @title Generate Transition Matrix
#' @description 
#'   Creates a cross-tabulation of baseline vs. follow-up states
#'   Useful for detailed transition analysis.
#'
#' @param data Data frame with state columns
#' @param col_base Baseline column name
#' @param col_curr Follow-up column name
#'
#' @return Matrix of transition counts

generate_transition_matrix <- function(data, col_base, col_curr) {
  trans_matrix <- table(
    Baseline = data[[col_base]],
    Followup = data[[col_curr]]
  )
  return(trans_matrix)
}

#' @title Calculate Transition Probabilities
#' @description 
#'   Converts transition counts to row-wise probabilities
#'   (probability of transitioning from each baseline state)
#'
#' @param trans_matrix Output from generate_transition_matrix()
#'
#' @return Matrix of transition probabilities (rows sum to 1)

calculate_transition_probs <- function(trans_matrix) {
  prob_matrix <- prop.table(trans_matrix, margin = 1)
  return(round(prob_matrix, 3))
}

# Example usage (uncomment to use):
# trans_matrix <- generate_transition_matrix(trajectory_data, COL_BASELINE, COL_FOLLOWUP)
# print(trans_matrix)
# 
# trans_probs <- calculate_transition_probs(trans_matrix)
# print(trans_probs)


# ==============================================================================
# END OF SCRIPT
# ==============================================================================

