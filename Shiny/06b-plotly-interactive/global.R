# =============================================================================
# Global Settings - Packages, Theme, and Configuration
# =============================================================================

# Load required packages
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(jsonlite)  # For JSON conversion in JS callbacks

# -----------------------------------------------------------------------------
# Theme Configuration
# -----------------------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#212529",
  primary = "#0d6efd",
  secondary = "#6c757d",
  success = "#198754",
  info = "#0dcaf0",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins"),
  code_font = font_google("Fira Code")
)

# -----------------------------------------------------------------------------
# Source all modules and utilities
# -----------------------------------------------------------------------------

# Source utility functions and data configuration
source("R/data_config.R")

# Source Shiny modules
source("R/mod_filter_widget.R")
