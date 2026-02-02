# =============================================================================
# Data Configuration and Preparation
# =============================================================================

# -----------------------------------------------------------------------------
# Prepare base dataset
# -----------------------------------------------------------------------------

#' Prepare the mtcars dataset for the app
#' Converts row names to column and creates factors for categorical variables
prepare_data <- function() {
  mtcars %>%
    tibble::rownames_to_column("car") %>%
    mutate(
      am = factor(am, labels = c("Automatic", "Manual")),
      cyl = factor(cyl),
      gear = factor(gear)
    )
}

# Load and prepare data
base_data <- prepare_data()

# -----------------------------------------------------------------------------
# Filter Configuration
# -----------------------------------------------------------------------------

#' Generate filter configuration from data
#' Returns a list with settings for each filterable variable
create_filter_config <- function(data) {
  list(
    # Categorical variables
    cyl = list(
      label = "Cylinders",
      type = "categorical",
      choices = levels(data$cyl)
    ),
    gear = list(
      label = "Gears",
      type = "categorical",
      choices = levels(data$gear)
    ),
    am = list(
      label = "Transmission",
      type = "categorical",
      choices = levels(data$am)
    ),
    # Numeric variables
    mpg = list(
      label = "MPG",
      type = "numeric",
      min = floor(min(data$mpg)),
      max = ceiling(max(data$mpg)),
      step = 0.5
    ),
    hp = list(
      label = "Horsepower",
      type = "numeric",
      min = floor(min(data$hp)),
      max = ceiling(max(data$hp)),
      step = 5
    ),
    wt = list(
      label = "Weight (1000 lbs)",
      type = "numeric",
      min = floor(min(data$wt) * 10) / 10,
      max = ceiling(max(data$wt) * 10) / 10,
      step = 0.1
    ),
    disp = list(
      label = "Displacement",
      type = "numeric",
      min = floor(min(data$disp)),
      max = ceiling(max(data$disp)),
      step = 10
    ),
    qsec = list(
      label = "1/4 Mile Time",
      type = "numeric",
      min = floor(min(data$qsec)),
      max = ceiling(max(data$qsec)),
      step = 0.5
    ),
    drat = list(
      label = "Rear Axle Ratio",
      type = "numeric",
      min = floor(min(data$drat) * 10) / 10,
      max = ceiling(max(data$drat) * 10) / 10,
      step = 0.1
    )
  )
}

# Create filter configuration
filter_config <- create_filter_config(base_data)

# Create named choices for filter dropdown
filter_choices <- setNames(
  names(filter_config),
  sapply(filter_config, function(x) x$label)
)

# -----------------------------------------------------------------------------
# Plot Configuration
# -----------------------------------------------------------------------------

# Available variables for axes
axis_choices <- list(
  x = c("Weight" = "wt",
        "Horsepower" = "hp",
        "Displacement" = "disp",
        "MPG" = "mpg"),
  y = c("MPG" = "mpg",
        "Weight" = "wt",
        "Horsepower" = "hp",
        "Displacement" = "disp")
)

# Available variables for color grouping
color_choices <- c(
  "Cylinders" = "cyl",
  "Gears" = "gear",
  "Transmission" = "am"
)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Get axis label from variable name
#' @param var_name The variable name (e.g., "wt", "mpg")
#' @param choices Named vector of choices
#' @return The display label for the variable
get_axis_label <- function(var_name, choices) {
  names(choices)[choices == var_name]
}

#' Apply filters to data
#' @param data The data frame to filter
#' @param filters List of active filters
#' @return Filtered data frame
apply_filters <- function(data, filters) {
  for (f in filters) {
    var_name <- f$variable
    value <- f$value

    if (f$type == "categorical") {
      if (length(value) > 0) {
        data <- data %>% filter(!!sym(var_name) %in% value)
      }
    } else {
      # Numeric filter with range
      if (length(value) == 2) {
        data <- data %>%
          filter(!!sym(var_name) >= value[1] & !!sym(var_name) <= value[2])
      }
    }
  }
  data
}
