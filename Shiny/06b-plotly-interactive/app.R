# Interactive Data Explorer with Plotly
# Advanced app demonstrating: plotly, bslib, DT, dynamic selection, dynamic filters

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)

# Thème moderne
my_theme <- bs_theme(
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

# Prepare base data
base_data <- mtcars %>%
  tibble::rownames_to_column("car") %>%
  mutate(
    am = factor(am, labels = c("Automatic", "Manual")),
    cyl = factor(cyl),
    gear = factor(gear)
  )

# Define filter configurations
numeric_vars <- list(
  mpg = list(label = "MPG", min = floor(min(base_data$mpg)), max = ceiling(max(base_data$mpg))),
  hp = list(label = "Horsepower", min = floor(min(base_data$hp)), max = ceiling(max(base_data$hp))),
  wt = list(label = "Weight (1000 lbs)", min = floor(min(base_data$wt) * 10) / 10, max = ceiling(max(base_data$wt) * 10) / 10),
  disp = list(label = "Displacement", min = floor(min(base_data$disp)), max = ceiling(max(base_data$disp))),
  drat = list(label = "Rear Axle Ratio", min = floor(min(base_data$drat) * 10) / 10, max = ceiling(max(base_data$drat) * 10) / 10),
  qsec = list(label = "1/4 Mile Time", min = floor(min(base_data$qsec)), max = ceiling(max(base_data$qsec)))
)

categorical_vars <- list(
  cyl = list(label = "Cylinders", choices = levels(base_data$cyl)),
  gear = list(label = "Gears", choices = levels(base_data$gear)),
  am = list(label = "Transmission", choices = levels(base_data$am))
)

# Create sidebar filters UI
sidebar_filters <- sidebar(
  title = "Data Filters",
  width = 300,

  # Filter status
  card(
    card_body(
      class = "p-2",
      div(
        class = "d-flex justify-content-between align-items-center",
        span(
          icon("filter"),
          textOutput("filter_status", inline = TRUE)
        ),
        actionButton("reset_filters", "Reset All",
                    class = "btn-sm btn-outline-danger",
                    icon = icon("xmark"))
      )
    )
  ),

  hr(),

  # Categorical filters
  h6(icon("tags"), "Categorical Filters", class = "text-muted"),

  selectInput(
    "filter_cyl",
    "Cylinders:",
    choices = categorical_vars$cyl$choices,
    selected = categorical_vars$cyl$choices,
    multiple = TRUE
  ),

  selectInput(
    "filter_gear",
    "Gears:",
    choices = categorical_vars$gear$choices,
    selected = categorical_vars$gear$choices,
    multiple = TRUE
  ),

  selectInput(
    "filter_am",
    "Transmission:",
    choices = categorical_vars$am$choices,
    selected = categorical_vars$am$choices,
    multiple = TRUE
  ),

  hr(),

  # Numeric filters
  h6(icon("sliders"), "Numeric Filters", class = "text-muted"),

  sliderInput(
    "filter_mpg",
    "MPG:",
    min = numeric_vars$mpg$min,
    max = numeric_vars$mpg$max,
    value = c(numeric_vars$mpg$min, numeric_vars$mpg$max),
    step = 0.5
  ),

  sliderInput(
    "filter_hp",
    "Horsepower:",
    min = numeric_vars$hp$min,
    max = numeric_vars$hp$max,
    value = c(numeric_vars$hp$min, numeric_vars$hp$max),
    step = 5
  ),

  sliderInput(
    "filter_wt",
    "Weight (1000 lbs):",
    min = numeric_vars$wt$min,
    max = numeric_vars$wt$max,
    value = c(numeric_vars$wt$min, numeric_vars$wt$max),
    step = 0.1
  ),

  sliderInput(
    "filter_disp",
    "Displacement:",
    min = numeric_vars$disp$min,
    max = numeric_vars$disp$max,
    value = c(numeric_vars$disp$min, numeric_vars$disp$max),
    step = 10
  ),

  sliderInput(
    "filter_qsec",
    "1/4 Mile Time:",
    min = numeric_vars$qsec$min,
    max = numeric_vars$qsec$max,
    value = c(numeric_vars$qsec$min, numeric_vars$qsec$max),
    step = 0.5
  )
)

ui <- page_navbar(
  title = "Interactive Data Explorer",
  theme = my_theme,
  fillable = TRUE,

  # Page principale with sidebar

nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar_filters,

      # Main content
      layout_columns(
        col_widths = c(8, 4),
        row_heights = c(1, 2, 1),

        # Header avec métriques
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

        # Graphique principal
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Interactive Scatter Plot",
            div(
              actionButton("reset", "Reset Selection",
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
            plotlyOutput("scatter", height = "100%")
          )
        ),

        # Tableau des données sélectionnées
        card(
          full_screen = TRUE,
          card_header("Selected Data"),
          card_body(
            DTOutput("table")
          )
        ),

        # Panneau de contrôle
        card(
          card_header("Plot Controls"),
          card_body(
            selectInput(
              "x_var",
              "X Axis:",
              choices = c("Weight" = "wt",
                         "Horsepower" = "hp",
                         "Displacement" = "disp",
                         "MPG" = "mpg"),
              selected = "wt"
            ),

            selectInput(
              "y_var",
              "Y Axis:",
              choices = c("MPG" = "mpg",
                         "Weight" = "wt",
                         "Horsepower" = "hp",
                         "Displacement" = "disp"),
              selected = "mpg"
            ),

            selectInput(
              "color_var",
              "Color By:",
              choices = c("Cylinders" = "cyl",
                         "Gears" = "gear",
                         "Transmission" = "am"),
              selected = "cyl"
            ),

            radioButtons(
              "select_mode",
              "Selection Mode:",
              choices = c("Rectangle" = "select", "Lasso" = "lasso"),
              selected = "lasso"
            ),

            hr(),

            checkboxInput("show_trend", "Show Trend Line", FALSE)
          )
        ),

        # Statistiques
        card(
          card_header("Summary Statistics"),
          card_body(
            verbatimTextOutput("stats")
          )
        )
      )
    )
  ),

  # Page à propos
  nav_panel(
    title = "About",
    icon = icon("circle-info"),

    card(
      card_header("About This App"),
      card_body(
        markdown("
### Interactive Data Explorer

This Shiny dashboard demonstrates:

- **Dynamic filtering** with sliders and select inputs
- **Interactive selection** with Plotly
- **Reactive tables** with DT
- **Modern UI** with bslib
- **Value boxes** for key metrics
- **Customizable visualizations**

#### How to use:

1. Use the sidebar filters to narrow down the data
2. Select points on the scatter plot using click-and-drag
3. Use lasso mode for free-form selection
4. View selected data in the table
5. Adjust variables and colors in the controls panel

#### Filtering:

- **Categorical filters**: Select one or more categories to include
- **Numeric filters**: Adjust sliders to set min/max ranges
- All filters are cumulative and work together

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

server <- function(input, output, session) {

  # Reset all filters
  observeEvent(input$reset_filters, {
    # Reset categorical filters
    updateSelectInput(session, "filter_cyl", selected = categorical_vars$cyl$choices)
    updateSelectInput(session, "filter_gear", selected = categorical_vars$gear$choices)
    updateSelectInput(session, "filter_am", selected = categorical_vars$am$choices)

    # Reset numeric filters
    updateSliderInput(session, "filter_mpg", value = c(numeric_vars$mpg$min, numeric_vars$mpg$max))
    updateSliderInput(session, "filter_hp", value = c(numeric_vars$hp$min, numeric_vars$hp$max))
    updateSliderInput(session, "filter_wt", value = c(numeric_vars$wt$min, numeric_vars$wt$max))
    updateSliderInput(session, "filter_disp", value = c(numeric_vars$disp$min, numeric_vars$disp$max))
    updateSliderInput(session, "filter_qsec", value = c(numeric_vars$qsec$min, numeric_vars$qsec$max))
  })

  # Reactive filtered data based on all filters
  filtered_data <- reactive({
    data <- base_data

    # Apply categorical filters
    if (!is.null(input$filter_cyl) && length(input$filter_cyl) > 0) {
      data <- data %>% filter(cyl %in% input$filter_cyl)
    }

    if (!is.null(input$filter_gear) && length(input$filter_gear) > 0) {
      data <- data %>% filter(gear %in% input$filter_gear)
    }

    if (!is.null(input$filter_am) && length(input$filter_am) > 0) {
      data <- data %>% filter(am %in% input$filter_am)
    }

    # Apply numeric filters
    if (!is.null(input$filter_mpg)) {
      data <- data %>% filter(mpg >= input$filter_mpg[1] & mpg <= input$filter_mpg[2])
    }

    if (!is.null(input$filter_hp)) {
      data <- data %>% filter(hp >= input$filter_hp[1] & hp <= input$filter_hp[2])
    }

    if (!is.null(input$filter_wt)) {
      data <- data %>% filter(wt >= input$filter_wt[1] & wt <= input$filter_wt[2])
    }

    if (!is.null(input$filter_disp)) {
      data <- data %>% filter(disp >= input$filter_disp[1] & disp <= input$filter_disp[2])
    }

    if (!is.null(input$filter_qsec)) {
      data <- data %>% filter(qsec >= input$filter_qsec[1] & qsec <= input$filter_qsec[2])
    }

    data
  })

  # Count active filters
  active_filter_count <- reactive({
    count <- 0

    # Check categorical filters
    if (length(input$filter_cyl) < length(categorical_vars$cyl$choices)) count <- count + 1
    if (length(input$filter_gear) < length(categorical_vars$gear$choices)) count <- count + 1
    if (length(input$filter_am) < length(categorical_vars$am$choices)) count <- count + 1

    # Check numeric filters
    if (!is.null(input$filter_mpg) &&
        (input$filter_mpg[1] > numeric_vars$mpg$min || input$filter_mpg[2] < numeric_vars$mpg$max)) count <- count + 1
    if (!is.null(input$filter_hp) &&
        (input$filter_hp[1] > numeric_vars$hp$min || input$filter_hp[2] < numeric_vars$hp$max)) count <- count + 1
    if (!is.null(input$filter_wt) &&
        (input$filter_wt[1] > numeric_vars$wt$min || input$filter_wt[2] < numeric_vars$wt$max)) count <- count + 1
    if (!is.null(input$filter_disp) &&
        (input$filter_disp[1] > numeric_vars$disp$min || input$filter_disp[2] < numeric_vars$disp$max)) count <- count + 1
    if (!is.null(input$filter_qsec) &&
        (input$filter_qsec[1] > numeric_vars$qsec$min || input$filter_qsec[2] < numeric_vars$qsec$max)) count <- count + 1

    count
  })

  # Filter status text
  output$filter_status <- renderText({
    count <- active_filter_count()
    if (count == 0) {
      "No filters active"
    } else {
      paste(count, "filter(s) active")
    }
  })

  # Active filters display
  output$active_filters_display <- renderUI({
    filters <- list()

    # Check categorical filters
    if (length(input$filter_cyl) < length(categorical_vars$cyl$choices) && length(input$filter_cyl) > 0) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-primary me-1", paste("Cyl:", paste(input$filter_cyl, collapse = ", ")))
      ))
    }
    if (length(input$filter_gear) < length(categorical_vars$gear$choices) && length(input$filter_gear) > 0) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-primary me-1", paste("Gears:", paste(input$filter_gear, collapse = ", ")))
      ))
    }
    if (length(input$filter_am) < length(categorical_vars$am$choices) && length(input$filter_am) > 0) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-primary me-1", paste("Trans:", paste(input$filter_am, collapse = ", ")))
      ))
    }

    # Check numeric filters
    if (!is.null(input$filter_mpg) &&
        (input$filter_mpg[1] > numeric_vars$mpg$min || input$filter_mpg[2] < numeric_vars$mpg$max)) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-info me-1", paste0("MPG: ", input$filter_mpg[1], "-", input$filter_mpg[2]))
      ))
    }
    if (!is.null(input$filter_hp) &&
        (input$filter_hp[1] > numeric_vars$hp$min || input$filter_hp[2] < numeric_vars$hp$max)) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-info me-1", paste0("HP: ", input$filter_hp[1], "-", input$filter_hp[2]))
      ))
    }
    if (!is.null(input$filter_wt) &&
        (input$filter_wt[1] > numeric_vars$wt$min || input$filter_wt[2] < numeric_vars$wt$max)) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-info me-1", paste0("Weight: ", input$filter_wt[1], "-", input$filter_wt[2]))
      ))
    }
    if (!is.null(input$filter_disp) &&
        (input$filter_disp[1] > numeric_vars$disp$min || input$filter_disp[2] < numeric_vars$disp$max)) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-info me-1", paste0("Disp: ", input$filter_disp[1], "-", input$filter_disp[2]))
      ))
    }
    if (!is.null(input$filter_qsec) &&
        (input$filter_qsec[1] > numeric_vars$qsec$min || input$filter_qsec[2] < numeric_vars$qsec$max)) {
      filters <- c(filters, list(
        tags$span(class = "badge bg-info me-1", paste0("qsec: ", input$filter_qsec[1], "-", input$filter_qsec[2]))
      ))
    }

    if (length(filters) == 0) {
      tags$span(class = "text-muted", icon("info-circle"), " Use sidebar filters to narrow down data")
    } else {
      div(
        tags$span(class = "text-muted me-2", "Active filters:"),
        filters
      )
    }
  })

  # Variable réactive pour reset selection
  reset_trigger <- reactiveVal(0)

  observeEvent(input$reset, {
    reset_trigger(reset_trigger() + 1)
  })

  # Graphique plotly - now uses filtered_data
  output$scatter <- renderPlotly({
    reset_trigger()

    data <- filtered_data()

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

    p <- plot_ly(data,
            x = as.formula(paste0("~", input$x_var)),
            y = as.formula(paste0("~", input$y_var)),
            color = as.formula(paste0("~", input$color_var)),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 12,
                         line = list(color = 'white', width = 1)),
            text = ~paste0("<b>", car, "</b><br>",
                          "Cylinders: ", cyl, "<br>",
                          "MPG: ", mpg, "<br>",
                          "HP: ", hp, "<br>",
                          "Weight: ", wt),
            hoverinfo = 'text',
            source = "select"
    ) %>%
      layout(
        dragmode = input$select_mode,
        hovermode = 'closest',
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white",
        xaxis = list(title = names(which(c("wt" = "Weight",
                                           "hp" = "Horsepower",
                                           "disp" = "Displacement",
                                           "mpg" = "MPG") == input$x_var))),
        yaxis = list(title = names(which(c("mpg" = "MPG",
                                           "wt" = "Weight",
                                           "hp" = "Horsepower",
                                           "disp" = "Displacement") == input$y_var)))
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("pan2d", "zoomIn2d", "zoomOut2d"))

    # Ajouter ligne de tendance si demandé
    if (input$show_trend && nrow(data) > 1) {
      p <- p %>% add_lines(
        x = as.formula(paste0("~", input$x_var)),
        y = fitted(lm(as.formula(paste(input$y_var, "~", input$x_var)), data = data)),
        line = list(color = 'rgba(255, 0, 0, 0.5)', dash = 'dash'),
        name = "Trend",
        showlegend = TRUE,
        hoverinfo = 'skip'
      )
    }

    p
  })

  # Données sélectionnées - now from filtered_data
  selected_data <- reactive({
    s <- event_data("plotly_selected", source = "select")
    data <- filtered_data()

    if (is.null(s) || nrow(data) == 0) return(data.frame())

    selected_indices <- s$pointNumber + 1
    # Make sure indices are valid
    valid_indices <- selected_indices[selected_indices <= nrow(data)]
    if (length(valid_indices) == 0) return(data.frame())

    data[valid_indices, ]
  })

  # Tableau
  output$table <- renderDT({
    df <- selected_data()

    if (nrow(df) == 0) {
      return(data.frame(Message = "Select points on the chart to view details"))
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

  # Value boxes - now use filtered_data
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

  # Statistiques - now use filtered_data
  output$stats <- renderPrint({
    df <- selected_data()
    data <- filtered_data()

    if (nrow(data) == 0) {
      cat("No data matches current filters\n")
      return()
    }

    if (nrow(df) == 0) {
      cat("Filtered Data Summary\n")
      cat("====================\n\n")
      cat("Total cars (filtered):", nrow(data), "\n\n")

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

shinyApp(ui, server)
