# =============================================================================
# Interactive Data Explorer with Plotly
# Main Application Entry Point
# =============================================================================
#
# This app demonstrates:
# - Modular Shiny application structure
# - Dynamic filtering with a custom module
# - Interactive Plotly visualizations
# - Bidirectional filtering (table ↔ plot)
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

  # JavaScript for dynamic row highlighting
  header = tags$head(
    tags$script(HTML("
      // Handler to update table row highlighting based on plot selection
      Shiny.addCustomMessageHandler('updateTableHighlight', function(message) {
        var selectedCars = message.selectedCars;
        var carColIndex = message.carColIndex;

        // Remove existing highlights
        $('#data_table tbody tr').removeClass('selected-row').css('background-color', '');

        // If no selection or car column not visible, return
        if (!selectedCars || selectedCars.length === 0 || carColIndex < 0) {
          return;
        }

        // Add highlights to selected rows
        $('#data_table tbody tr').each(function() {
          var row = $(this);
          var carName = row.find('td').eq(carColIndex).text();
          if (selectedCars.indexOf(carName) > -1) {
            row.addClass('selected-row').css('background-color', '#d4edda');
          }
        });
      });

      // Initialize handler (placeholder for any setup needed)
      Shiny.addCustomMessageHandler('initTableHighlight', function(message) {
        // Initialization complete
      });
    "))
  ),

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
            uiOutput("plot_header"),
            div(
              actionButton("reset_selection", "Reset Selection",
                          class = "btn-sm btn-outline-secondary",
                          icon = icon("rotate-left")),
              popover(
                icon("circle-info"),
                title = "How to use",
                "• Use column filters in the table to filter the plot
                • Use click-and-drag or lasso to select points
                • Hold Shift to select multiple groups"
              )
            )
          ),
          card_body(
            plotlyOutput("scatter_plot", height = "100%")
          )
        ),

        # Data table with column filters
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            uiOutput("table_header"),
            div(
              # Column selector dropdown
              popover(
                actionButton("col_selector_btn", "",
                            icon = icon("table-columns"),
                            class = "btn-sm btn-outline-primary me-1",
                            title = "Select columns"),
                title = "Select Columns to Display",
                checkboxGroupInput(
                  "table_columns",
                  label = NULL,
                  choices = c("Car" = "car",
                             "MPG" = "mpg",
                             "Cylinders" = "cyl",
                             "Horsepower" = "hp",
                             "Displacement" = "disp",
                             "Rear Axle Ratio" = "drat",
                             "Weight" = "wt",
                             "1/4 Mile Time" = "qsec",
                             "Engine" = "vs",
                             "Transmission" = "am",
                             "Gears" = "gear",
                             "Carburetors" = "carb"),
                  selected = c("car", "mpg", "cyl", "hp", "disp", "drat",
                              "wt", "qsec", "vs", "am", "gear", "carb")
                )
              ),
              actionButton("clear_table_filters", "Clear Filters",
                          class = "btn-sm btn-outline-secondary",
                          icon = icon("filter-circle-xmark"))
            )
          ),
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
- **Bidirectional filtering** - Table filters ↔ Plot selection
- **Interactive selection** with Plotly
- **Reactive tables** with DT

#### Filtering Interactions:

```
Sidebar Filters → Table (with column filters) → Plot → Selection
       ↓                    ↓                    ↓         ↓
   Base filter      Column filters          Shows      Highlights
                    affect plot           filtered     in table
```

#### How to use:

1. Use **sidebar filters** for primary data filtering
2. Use **table column filters** for quick refinement (affects plot)
3. **Select points** on the plot to highlight them in the table
4. All filters work together cumulatively

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

  # Get active filters from the sidebar module
  active_filters <- filterWidgetServer("filters", filter_config, filter_choices)

  # ---------------------------------------------------------------------------
  # Sidebar Filtered Data (base data after sidebar filters)
  # ---------------------------------------------------------------------------

  sidebar_filtered_data <- reactive({
    apply_filters(base_data, active_filters())
  })

  # ---------------------------------------------------------------------------
  # Table Filtered Data (after table column filters)
  # ---------------------------------------------------------------------------

  # Get rows that pass the table's column filters
  table_filtered_rows <- reactive({
    rows <- input$data_table_rows_all
    if (is.null(rows)) {
      # No filtering yet, return all rows
      seq_len(nrow(sidebar_filtered_data()))
    } else {
      rows
    }
  })

  # Data to show in the plot (respects table column filters)
  plot_data <- reactive({
    data <- sidebar_filtered_data()
    rows <- table_filtered_rows()

    if (length(rows) == 0 || nrow(data) == 0) {
      return(data.frame())
    }

    # Return only rows that pass table filters
    data[rows, ]
  })

  # ---------------------------------------------------------------------------
  # Active Filters Display
  # ---------------------------------------------------------------------------

  output$active_filters_display <- renderUI({
    # Count table column filters active
    total_rows <- nrow(sidebar_filtered_data())
    filtered_rows <- length(table_filtered_rows())
    table_filter_active <- filtered_rows < total_rows

    sidebar_badges <- renderFilterBadges(active_filters())

    if (table_filter_active) {
      tagList(
        sidebar_badges,
        tags$span(
          class = "badge bg-warning ms-2",
          icon("table-columns", class = "me-1"),
          paste0("Table filter: ", filtered_rows, "/", total_rows, " rows")
        )
      )
    } else {
      sidebar_badges
    }
  })

  # ---------------------------------------------------------------------------
  # Plot Selection Reset
  # ---------------------------------------------------------------------------

  reset_trigger <- reactiveVal(0)

  observeEvent(input$reset_selection, {
    reset_trigger(reset_trigger() + 1)
  })

  # Clear table column filters by reloading data
  observeEvent(input$clear_table_filters, {
    # Force table to re-render without filters
    # This is done by triggering a proxy reload
    dataTableProxy("data_table") %>% clearSearch()
  })

  # ---------------------------------------------------------------------------
  # Scatter Plot (uses table-filtered data)
  # ---------------------------------------------------------------------------

  output$scatter_plot <- renderPlotly({
    # Trigger reset when button clicked
    reset_trigger()

    data <- plot_data()

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
      source = "scatter",
      customdata = ~car  # Store car name for matching with table
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

  # Plot header showing count
  output$plot_header <- renderUI({
    n <- nrow(plot_data())
    div(
      "Interactive Scatter Plot",
      tags$span(
        class = "badge bg-secondary ms-2",
        paste(n, "points")
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Selected Data from Plot
  # ---------------------------------------------------------------------------

  selected_cars <- reactive({
    s <- event_data("plotly_selected", source = "scatter")
    if (is.null(s)) return(character(0))

    # Get car names from selection
    data <- plot_data()
    if (nrow(data) == 0) return(character(0))

    selected_indices <- s$pointNumber + 1
    valid_indices <- selected_indices[selected_indices <= nrow(data)]

    if (length(valid_indices) == 0) return(character(0))

    data$car[valid_indices]
  })

  selected_data <- reactive({
    cars <- selected_cars()
    if (length(cars) == 0) return(data.frame())

    plot_data() %>% filter(car %in% cars)
  })

  # ---------------------------------------------------------------------------
  # Data Table (with column filters, shows all sidebar-filtered data)
  # ---------------------------------------------------------------------------

  # Get selected columns for the table
  table_display_columns <- reactive({
    cols <- input$table_columns
    if (is.null(cols) || length(cols) == 0) {
      # Default to all columns if none selected
      c("car", "mpg", "cyl", "hp", "disp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
    } else {
      cols
    }
  })

  output$data_table <- renderDT({
    data <- sidebar_filtered_data()
    cols <- table_display_columns()

    if (nrow(data) == 0) {
      return(datatable(data.frame(Message = "No data matches current filters")))
    }

    # Ensure 'car' is always first if selected (for row matching)
    if ("car" %in% cols) {
      cols <- c("car", setdiff(cols, "car"))
    }

    # Prepare display data with selected columns only
    display_df <- data %>% select(all_of(cols))

    # Determine which numeric columns to format
    numeric_cols_to_round <- intersect(cols, c("mpg", "hp", "wt", "drat", "qsec"))

    # Find car column index for row callback (0-based for JS)
    car_col_index <- if ("car" %in% cols) which(cols == "car") - 1 else -1

    # Create datatable with column filters and horizontal scroll
    dt <- datatable(
      display_df,
      filter = 'top',  # Enable column filters at top
      options = list(
        pageLength = 15,
        dom = 'tip',
        scrollX = TRUE,  # Enable horizontal scrolling
        scrollY = "400px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '120px', targets = "_all")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover nowrap'
    )

    # Format numeric columns if they exist
    if (length(numeric_cols_to_round) > 0) {
      dt <- dt %>% formatRound(columns = numeric_cols_to_round, digits = 1)
    }

    dt
  }, server = TRUE)  # Use server-side processing

  # Update row highlighting when selection changes
  observe({
    selected <- selected_cars()
    cols <- table_display_columns()
    car_col_index <- if ("car" %in% cols) which(c("car", setdiff(cols, "car")) == "car") - 1 else -1

    # Send selection to JavaScript to update highlighting
    session$sendCustomMessage("updateTableHighlight", list(
      selectedCars = selected,
      carColIndex = car_col_index
    ))
  })

  # Add JavaScript handler for updating table row highlighting
  observe({
    # Only run once to add the JS handler
    session$sendCustomMessage("initTableHighlight", list())
  })

  # Dynamic table header
  output$table_header <- renderUI({
    total <- nrow(sidebar_filtered_data())
    filtered <- length(table_filtered_rows())
    selected_n <- length(selected_cars())

    filter_active <- filtered < total

    div(
      icon("table", class = "text-primary me-2"),
      paste0("Data Table (", filtered, "/", total, " rows)"),
      if (selected_n > 0) {
        tags$span(
          class = "badge bg-success ms-2",
          paste(selected_n, "selected")
        )
      },
      if (filter_active) {
        tags$span(
          class = "badge bg-warning ms-2",
          icon("filter"),
          " filtered"
        )
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Value Boxes (based on what's visible in plot + selection)
  # ---------------------------------------------------------------------------

  output$total_cars <- renderText({
    paste0(nrow(plot_data()), " / ", nrow(base_data))
  })

  output$selected_count <- renderText({
    n <- length(selected_cars())
    if (n == 0) return("0")
    n
  })

  output$avg_mpg <- renderText({
    selected <- selected_data()
    data <- plot_data()

    if (nrow(data) == 0) return("N/A")

    if (nrow(selected) == 0) {
      sprintf("%.1f", mean(data$mpg))
    } else {
      sprintf("%.1f", mean(selected$mpg))
    }
  })

  output$avg_hp <- renderText({
    selected <- selected_data()
    data <- plot_data()

    if (nrow(data) == 0) return("N/A")

    if (nrow(selected) == 0) {
      sprintf("%.0f", mean(data$hp))
    } else {
      sprintf("%.0f", mean(selected$hp))
    }
  })

  # ---------------------------------------------------------------------------
  # Summary Statistics
  # ---------------------------------------------------------------------------

  output$summary_stats <- renderPrint({
    selected <- selected_data()
    data <- plot_data()

    if (nrow(data) == 0) {
      cat("No data matches current filters\n")
      return()
    }

    if (nrow(selected) == 0) {
      cat("Visible Data Summary\n")
      cat("====================\n\n")
      cat("Total cars (after all filters):", nrow(data), "\n\n")

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

      cat("\nTip: Select points on the chart or filter the table columns")
      return()
    }

    cat("Selected Cars Summary\n")
    cat("====================\n\n")
    cat("Number of cars:", nrow(selected), "/", nrow(data), "\n\n")

    cat("MPG Statistics:\n")
    cat("  Min:", min(selected$mpg), "\n")
    cat("  Mean:", round(mean(selected$mpg), 2), "\n")
    cat("  Max:", max(selected$mpg), "\n\n")

    cat("Horsepower Statistics:\n")
    cat("  Min:", min(selected$hp), "\n")
    cat("  Mean:", round(mean(selected$hp), 2), "\n")
    cat("  Max:", max(selected$hp), "\n\n")

    cat("Distribution by Cylinders:\n")
    print(table(selected$cyl))
  })
}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui, server)
