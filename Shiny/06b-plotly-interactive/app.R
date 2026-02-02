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

# Define all available filter variables with their configurations
filter_config <- list(
  # Categorical variables
  cyl = list(
    label = "Cylinders",
    type = "categorical",
    choices = levels(base_data$cyl)
  ),
  gear = list(
    label = "Gears",
    type = "categorical",
    choices = levels(base_data$gear)
  ),
 am = list(
    label = "Transmission",
    type = "categorical",
    choices = levels(base_data$am)
  ),
  # Numeric variables
  mpg = list(
    label = "MPG",
    type = "numeric",
    min = floor(min(base_data$mpg)),
    max = ceiling(max(base_data$mpg)),
    step = 0.5
  ),
  hp = list(
    label = "Horsepower",
    type = "numeric",
    min = floor(min(base_data$hp)),
    max = ceiling(max(base_data$hp)),
    step = 5
  ),
  wt = list(
    label = "Weight (1000 lbs)",
    type = "numeric",
    min = floor(min(base_data$wt) * 10) / 10,
    max = ceiling(max(base_data$wt) * 10) / 10,
    step = 0.1
  ),
  disp = list(
    label = "Displacement",
    type = "numeric",
    min = floor(min(base_data$disp)),
    max = ceiling(max(base_data$disp)),
    step = 10
  ),
  qsec = list(
    label = "1/4 Mile Time",
    type = "numeric",
    min = floor(min(base_data$qsec)),
    max = ceiling(max(base_data$qsec)),
    step = 0.5
  ),
  drat = list(
    label = "Rear Axle Ratio",
    type = "numeric",
    min = floor(min(base_data$drat) * 10) / 10,
    max = ceiling(max(base_data$drat) * 10) / 10,
    step = 0.1
  )
)

# Get choices for filter dropdown
filter_choices <- setNames(
  names(filter_config),
  sapply(filter_config, function(x) x$label)
)

# Create sidebar with dynamic filter widget
sidebar_content <- sidebar(
  title = "Data Filters",
  width = 320,

  # Dynamic Filter Widget
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center py-2",
      span(icon("filter"), "Active Filters"),
      div(
        actionButton("add_filter", "",
                    icon = icon("plus"),
                    class = "btn-sm btn-success"),
        actionButton("clear_all_filters", "",
                    icon = icon("trash"),
                    class = "btn-sm btn-outline-danger ms-1",
                    title = "Clear all filters")
      )
    ),
    card_body(
      class = "p-2",
      # Filter count status
      div(
        class = "mb-2 text-muted small",
        textOutput("filter_count_text", inline = TRUE)
      ),

      # Container for dynamically added filters
      div(id = "filter_container",
        uiOutput("dynamic_filters")
      ),

      # Placeholder when no filters
      uiOutput("no_filters_message")
    )
  ),

  hr(),

  # Add filter modal trigger info
  div(
    class = "text-muted small text-center",
    icon("info-circle"),
    "Click", icon("plus", class = "text-success"), "to add a filter"
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
      sidebar = sidebar_content,

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

- **Dynamic filtering widget** - Add/remove filters on demand
- **Interactive selection** with Plotly
- **Reactive tables** with DT
- **Modern UI** with bslib
- **Value boxes** for key metrics

#### How to use:

1. Click the **+** button in the sidebar to add filters
2. Choose a variable and set filter values
3. Add multiple filters - they work cumulatively
4. Remove individual filters with the **x** button
5. Select points on the scatter plot for detailed view

#### Filter Types:

- **Categorical**: Multi-select dropdown (Cylinders, Gears, Transmission)
- **Numeric**: Range slider (MPG, HP, Weight, etc.)

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

  # Reactive values to store active filters
  rv <- reactiveValues(
    filters = list(),        # List of active filter configurations
    filter_counter = 0       # Counter for unique filter IDs
  )

  # Show modal to add new filter
  observeEvent(input$add_filter, {
    # Get variables that are not already filtered
    used_vars <- sapply(rv$filters, function(f) f$variable)
    available_choices <- filter_choices[!filter_choices %in% used_vars]

    if (length(available_choices) == 0) {
      showNotification("All variables are already being filtered.",
                      type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "Add New Filter",
      size = "s",

      selectInput(
        "new_filter_var",
        "Select Variable:",
        choices = available_choices
      ),

      # Dynamic filter value input will be rendered here
      uiOutput("new_filter_value_ui"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_filter", "Add Filter",
                    class = "btn-primary")
      )
    ))
  })

  # Render appropriate input based on selected variable in modal
  output$new_filter_value_ui <- renderUI({
    req(input$new_filter_var)

    var_name <- input$new_filter_var
    config <- filter_config[[var_name]]

    if (config$type == "categorical") {
      selectInput(
        "new_filter_value",
        paste("Select", config$label, ":"),
        choices = config$choices,
        selected = config$choices,
        multiple = TRUE
      )
    } else {
      sliderInput(
        "new_filter_value",
        paste("Select", config$label, "Range:"),
        min = config$min,
        max = config$max,
        value = c(config$min, config$max),
        step = config$step
      )
    }
  })

  # Confirm adding new filter
  observeEvent(input$confirm_add_filter, {
    req(input$new_filter_var, input$new_filter_value)

    var_name <- input$new_filter_var
    config <- filter_config[[var_name]]

    # Increment counter for unique ID
    rv$filter_counter <- rv$filter_counter + 1
    filter_id <- paste0("filter_", rv$filter_counter)

    # Add new filter to the list
    rv$filters[[filter_id]] <- list(
      id = filter_id,
      variable = var_name,
      label = config$label,
      type = config$type,
      value = input$new_filter_value,
      config = config
    )

    removeModal()
  })

  # Remove individual filter
  observeEvent(input$remove_filter, {
    filter_id <- input$remove_filter
    rv$filters[[filter_id]] <- NULL
  })

  # Clear all filters
  observeEvent(input$clear_all_filters, {
    rv$filters <- list()
  })

  # Render dynamic filters in sidebar
  output$dynamic_filters <- renderUI({
    filters <- rv$filters

    if (length(filters) == 0) {
      return(NULL)
    }

    filter_ui_list <- lapply(names(filters), function(filter_id) {
      f <- filters[[filter_id]]

      # Create the filter value input
      if (f$type == "categorical") {
        value_input <- selectInput(
          inputId = paste0(filter_id, "_value"),
          label = NULL,
          choices = f$config$choices,
          selected = f$value,
          multiple = TRUE,
          width = "100%"
        )
      } else {
        value_input <- sliderInput(
          inputId = paste0(filter_id, "_value"),
          label = NULL,
          min = f$config$min,
          max = f$config$max,
          value = f$value,
          step = f$config$step,
          width = "100%"
        )
      }

      # Filter card with remove button
      div(
        class = "card mb-2",
        div(
          class = "card-header py-1 px-2 d-flex justify-content-between align-items-center",
          style = "background-color: #f8f9fa;",
          span(
            class = "small fw-bold",
            if (f$type == "categorical") icon("tags", class = "text-primary me-1")
            else icon("sliders", class = "text-info me-1"),
            f$label
          ),
          tags$button(
            type = "button",
            class = "btn btn-sm btn-link text-danger p-0",
            onclick = sprintf("Shiny.setInputValue('remove_filter', '%s', {priority: 'event'})", filter_id),
            icon("xmark")
          )
        ),
        div(
          class = "card-body py-2 px-2",
          value_input
        )
      )
    })

    tagList(filter_ui_list)
  })

  # Show message when no filters
  output$no_filters_message <- renderUI({
    if (length(rv$filters) == 0) {
      div(
        class = "text-center text-muted py-3",
        icon("filter", class = "fa-2x mb-2 d-block mx-auto", style = "opacity: 0.3;"),
        p(class = "small mb-0", "No active filters"),
        p(class = "small text-muted", "Click + to add one")
      )
    }
  })

  # Filter count text
  output$filter_count_text <- renderText({
    n <- length(rv$filters)
    if (n == 0) {
      "No filters active"
    } else {
      paste(n, "filter(s) active")
    }
  })

  # Update filter values when user changes them
  observe({
    filters <- rv$filters

    for (filter_id in names(filters)) {
      input_id <- paste0(filter_id, "_value")
      new_value <- input[[input_id]]

      if (!is.null(new_value)) {
        # Update the stored value
        rv$filters[[filter_id]]$value <- new_value
      }
    }
  })

  # Reactive filtered data based on all active filters
  filtered_data <- reactive({
    data <- base_data
    filters <- rv$filters

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
          data <- data %>% filter(!!sym(var_name) >= value[1] & !!sym(var_name) <= value[2])
        }
      }
    }

    data
  })

  # Active filters display (badges in main content)
  output$active_filters_display <- renderUI({
    filters <- rv$filters

    if (length(filters) == 0) {
      return(tags$span(class = "text-muted",
                      icon("info-circle"),
                      " Use sidebar to add filters"))
    }

    badges <- lapply(filters, function(f) {
      if (f$type == "categorical") {
        # Check if not all selected
        if (length(f$value) < length(f$config$choices) && length(f$value) > 0) {
          tags$span(
            class = "badge bg-primary me-1 mb-1",
            paste(f$label, ":", paste(f$value, collapse = ", "))
          )
        }
      } else {
        # Check if range is modified
        if (f$value[1] > f$config$min || f$value[2] < f$config$max) {
          tags$span(
            class = "badge bg-info me-1 mb-1",
            paste0(f$label, ": ", f$value[1], " - ", f$value[2])
          )
        }
      }
    })

    # Remove NULL badges
    badges <- Filter(Negate(is.null), badges)

    if (length(badges) == 0) {
      return(tags$span(class = "text-muted",
                      icon("check-circle", class = "text-success"),
                      " Filters added but showing all data (no restrictions)"))
    }

    div(
      tags$span(class = "text-muted me-2", "Active filters:"),
      badges
    )
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

  # Value boxes
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

  # Statistiques
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

shinyApp(ui, server)
