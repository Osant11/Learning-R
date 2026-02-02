# =============================================================================
# Interactive Data Explorer with Plotly
# Main Application Entry Point
# =============================================================================
#
# This app demonstrates:
# - Modular Shiny application structure
# - Dynamic filtering with a custom module
# - Interactive Plotly visualizations
# - Modern UI with bslib
#
# File Structure:
# ├── app.R          (this file - main app)
# ├── global.R       (packages, theme, sources)
# └── R/
#     ├── data_config.R       (data preparation, filter config)
#     └── mod_filter_widget.R (filter widget module)
#
# =============================================================================

# Source global settings and modules
source("global.R")

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "Interactive Data Explorer",
  theme = app_theme,
  fillable = TRUE,

  # ---------------------------------------------------------------------------
  # Dashboard Page
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),

    layout_sidebar(
      # Sidebar with filter module
      sidebar = filterWidgetSidebar("filters"),

      # Main content
      layout_columns(
        col_widths = c(8, 4),
        row_heights = c(1, 2, 1),

        # Value boxes row
        layout_columns(
          col_widths = c(3, 3, 3, 3),

          value_box(
            title = "Filtered Cars",
            value = textOutput("total_cars"),
            showcase = icon("car"),
            theme = "primary"
          ),

          value_box(
            title = "Selected",
            value = textOutput("selected_count"),
            showcase = icon("hand-pointer"),
            theme = "success"
          ),

          value_box(
            title = "Avg MPG",
            value = textOutput("avg_mpg"),
            showcase = icon("gas-pump"),
            theme = "info"
          ),

          value_box(
            title = "Avg HP",
            value = textOutput("avg_hp"),
            showcase = icon("gauge-high"),
            theme = "warning"
          )
        ),

        # Active filters display
        card(
          card_body(
            class = "p-2",
            uiOutput("active_filters_display")
          )
        ),

        # Main scatter plot
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Interactive Scatter Plot",
            div(
              actionButton("reset_selection", "Reset Selection",
                          class = "btn-sm btn-outline-secondary",
                          icon = icon("rotate-left")),
              popover(
                icon("circle-info"),
                title = "How to select",
                "Use click-and-drag or lasso tool to select multiple points.
                Hold Shift to select multiple groups."
              )
            )
          ),
          card_body(
            plotlyOutput("scatter_plot", height = "100%")
          )
        ),

        # Data table
        card(
          full_screen = TRUE,
          card_header(uiOutput("table_header")),
          card_body(
            DTOutput("data_table")
          )
        ),

        # Plot controls
        card(
          card_header("Plot Controls"),
          card_body(
            selectInput("x_var", "X Axis:",
                       choices = axis_choices$x,
                       selected = "wt"),

            selectInput("y_var", "Y Axis:",
                       choices = axis_choices$y,
                       selected = "mpg"),

            selectInput("color_var", "Color By:",
                       choices = color_choices,
                       selected = "cyl"),

            radioButtons("select_mode", "Selection Mode:",
                        choices = c("Rectangle" = "select",
                                   "Lasso" = "lasso"),
                        selected = "lasso"),

            hr(),

            checkboxInput("show_trend", "Show Trend Line", FALSE)
          )
        ),

        # Summary statistics
        card(
          card_header("Summary Statistics"),
          card_body(
            verbatimTextOutput("summary_stats")
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # About Page
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "About",
    icon = icon("circle-info"),

    card(
      card_header("About This App"),
      card_body(
        markdown("
### Interactive Data Explorer

This Shiny dashboard demonstrates:

- **Modular architecture** with reusable components
- **Dynamic filtering widget** - Add/remove filters on demand
- **Interactive selection** with Plotly
- **Reactive tables** with DT
- **Modern UI** with bslib

#### File Structure:

```
06b-plotly-interactive/
├── app.R              # Main application
├── global.R           # Packages, theme, sources
└── R/
    ├── data_config.R       # Data & filter configuration
    └── mod_filter_widget.R # Filter widget module
```

#### How to use:

1. Click the **+** button in the sidebar to add filters
2. Choose a variable and set filter values
3. Add multiple filters - they work cumulatively
4. Remove individual filters with the **x** button
5. Select points on the scatter plot for detailed view

#### Data:

Built with the `mtcars` dataset from base R.
        ")
      )
    )
  ),

  nav_spacer(),

  nav_item(
    tags$a(
      icon("github"),
      "Source",
      href = "https://github.com",
      target = "_blank"
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Filter Module
  # ---------------------------------------------------------------------------

  # Get active filters from the module
  active_filters <- filterWidgetServer("filters", filter_config, filter_choices)

  # ---------------------------------------------------------------------------
  # Filtered Data
  # ---------------------------------------------------------------------------

  filtered_data <- reactive({
    apply_filters(base_data, active_filters())
  })

  # ---------------------------------------------------------------------------
  # Active Filters Display
  # ---------------------------------------------------------------------------

  output$active_filters_display <- renderUI({
    renderFilterBadges(active_filters())
  })

  # ---------------------------------------------------------------------------
  # Plot Selection Reset
  # ---------------------------------------------------------------------------

  reset_trigger <- reactiveVal(0)

  observeEvent(input$reset_selection, {
    reset_trigger(reset_trigger() + 1)
  })

  # ---------------------------------------------------------------------------
  # Scatter Plot
  # ---------------------------------------------------------------------------

  output$scatter_plot <- renderPlotly({
    # Trigger reset when button clicked
    reset_trigger()

    data <- filtered_data()

    # Handle empty data
    if (nrow(data) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = "No data matches current filters",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    # Create scatter plot
    p <- plot_ly(
      data,
      x = as.formula(paste0("~", input$x_var)),
      y = as.formula(paste0("~", input$y_var)),
      color = as.formula(paste0("~", input$color_var)),
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 12,
        line = list(color = 'white', width = 1)
      ),
      text = ~paste0(
        "<b>", car, "</b><br>",
        "Cylinders: ", cyl, "<br>",
        "MPG: ", mpg, "<br>",
        "HP: ", hp, "<br>",
        "Weight: ", wt
      ),
      hoverinfo = 'text',
      source = "scatter"
    ) %>%
      layout(
        dragmode = input$select_mode,
        hovermode = 'closest',
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white",
        xaxis = list(title = get_axis_label(input$x_var, axis_choices$x)),
        yaxis = list(title = get_axis_label(input$y_var, axis_choices$y))
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("pan2d", "zoomIn2d", "zoomOut2d")
      )

    # Add trend line if requested
    if (input$show_trend && nrow(data) > 1) {
      formula <- as.formula(paste(input$y_var, "~", input$x_var))
      p <- p %>% add_lines(
        x = as.formula(paste0("~", input$x_var)),
        y = fitted(lm(formula, data = data)),
        line = list(color = 'rgba(255, 0, 0, 0.5)', dash = 'dash'),
        name = "Trend",
        showlegend = TRUE,
        hoverinfo = 'skip'
      )
    }

    p
  })

  # ---------------------------------------------------------------------------
  # Selected Data from Plot
  # ---------------------------------------------------------------------------

  selected_data <- reactive({
    s <- event_data("plotly_selected", source = "scatter")
    data <- filtered_data()

    if (is.null(s) || nrow(data) == 0) return(data.frame())

    selected_indices <- s$pointNumber + 1
    valid_indices <- selected_indices[selected_indices <= nrow(data)]

    if (length(valid_indices) == 0) return(data.frame())

    data[valid_indices, ]
  })

  # ---------------------------------------------------------------------------
  # Data Table
  # ---------------------------------------------------------------------------

  output$data_table <- renderDT({
    selected <- selected_data()
    all_data <- filtered_data()

    # Use selected data if available, otherwise show all filtered data
    if (nrow(selected) > 0) {
      df <- selected
    } else {
      df <- all_data
    }

    if (nrow(df) == 0) {
      return(data.frame(Message = "No data matches current filters"))
    }

    datatable(
      df %>% select(car, mpg, cyl, hp, wt, gear, am),
      options = list(
        pageLength = 10,
        dom = 'tip',
        scrollY = "400px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = c('mpg', 'hp', 'wt'), digits = 1)
  })

  # Dynamic table header
  output$table_header <- renderUI({
    selected <- selected_data()
    all_data <- filtered_data()

    if (nrow(selected) > 0) {
      div(
        icon("hand-pointer", class = "text-success me-2"),
        paste0("Selected Data (", nrow(selected), " rows)")
      )
    } else {
      div(
        icon("table", class = "text-primary me-2"),
        paste0("All Data (", nrow(all_data), " rows)")
      )
    }
  })

  # ---------------------------------------------------------------------------
  # Value Boxes
  # ---------------------------------------------------------------------------

  output$total_cars <- renderText({
    paste0(nrow(filtered_data()), " / ", nrow(base_data))
  })

  output$selected_count <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) return("0")
    nrow(df)
  })

  output$avg_mpg <- renderText({
    df <- selected_data()
    data <- filtered_data()

    if (nrow(data) == 0) return("N/A")

    if (nrow(df) == 0) {
      sprintf("%.1f", mean(data$mpg))
    } else {
      sprintf("%.1f", mean(df$mpg))
    }
  })

  output$avg_hp <- renderText({
    df <- selected_data()
    data <- filtered_data()

    if (nrow(data) == 0) return("N/A")

    if (nrow(df) == 0) {
      sprintf("%.0f", mean(data$hp))
    } else {
      sprintf("%.0f", mean(df$hp))
    }
  })

  # ---------------------------------------------------------------------------
  # Summary Statistics
  # ---------------------------------------------------------------------------

  output$summary_stats <- renderPrint({
    df <- selected_data()
    data <- filtered_data()

    if (nrow(data) == 0) {
      cat("No data matches current filters\n")
      return()
    }

    if (nrow(df) == 0) {
      cat("Filtered Data Summary\n")
      cat("====================\n\n")
      cat("Total cars (filtered):", nrow(data), "/", nrow(base_data), "\n\n")

      cat("MPG Statistics:\n")
      cat("  Min:", min(data$mpg), "\n")
      cat("  Mean:", round(mean(data$mpg), 2), "\n")
      cat("  Max:", max(data$mpg), "\n\n")

      cat("Horsepower Statistics:\n")
      cat("  Min:", min(data$hp), "\n")
      cat("  Mean:", round(mean(data$hp), 2), "\n")
      cat("  Max:", max(data$hp), "\n\n")

      cat("Distribution by Cylinders:\n")
      print(table(data$cyl))

      cat("\nSelect points on the chart for detailed selection stats")
      return()
    }

    cat("Selected Cars Summary\n")
    cat("====================\n\n")
    cat("Number of cars:", nrow(df), "/", nrow(data), "(filtered)\n\n")

    cat("MPG Statistics:\n")
    cat("  Min:", min(df$mpg), "\n")
    cat("  Mean:", round(mean(df$mpg), 2), "\n")
    cat("  Max:", max(df$mpg), "\n\n")

    cat("Horsepower Statistics:\n")
    cat("  Min:", min(df$hp), "\n")
    cat("  Mean:", round(mean(df$hp), 2), "\n")
    cat("  Max:", max(df$hp), "\n\n")

    cat("Distribution by Cylinders:\n")
    print(table(df$cyl))
  })
}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui, server)
