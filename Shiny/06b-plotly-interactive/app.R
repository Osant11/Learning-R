# Interactive Data Explorer with Plotly
# Advanced app demonstrating: plotly, bslib, DT, dynamic selection

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

ui <- page_navbar(
  title = "Interactive Data Explorer",
  theme = my_theme,
  fillable = TRUE,

  # Page principale
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),

    layout_columns(
      col_widths = c(8, 4),
      row_heights = c(1, 2, 1),

      # Header avec métriques
      layout_columns(
        col_widths = c(3, 3, 3, 3),

        value_box(
          title = "Total Cars",
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

      # Card vide pour équilibrer la grille
      card(
        card_header(""),
        card_body(min_height = "50px", padding = 0)
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
        card_header("Controls"),
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

- **Interactive selection** with Plotly
- **Reactive tables** with DT
- **Modern UI** with bslib
- **Value boxes** for key metrics
- **Customizable visualizations**

#### How to use:

1. Select points on the scatter plot using click-and-drag
2. Use lasso mode for free-form selection
3. View selected data in the table
4. Adjust variables and colors in the controls panel

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

  # Données
  data <- mtcars %>%
    tibble::rownames_to_column("car") %>%
    mutate(
      am = factor(am, labels = c("Automatic", "Manual")),
      cyl = factor(cyl),
      gear = factor(gear)
    )

  # Variable réactive pour reset
  reset_trigger <- reactiveVal(0)

  observeEvent(input$reset, {
    reset_trigger(reset_trigger() + 1)
  })

  # Graphique plotly
  output$scatter <- renderPlotly({
    reset_trigger()

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
    if (input$show_trend) {
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

  # Données sélectionnées
  selected_data <- reactive({
    s <- event_data("plotly_selected", source = "select")

    if (is.null(s)) return(data.frame())

    selected_indices <- s$pointNumber + 1
    data[selected_indices, ]
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
    nrow(data)
  })

  output$selected_count <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) return("0")
    nrow(df)
  })

  output$avg_mpg <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) {
      sprintf("%.1f", mean(data$mpg))
    } else {
      sprintf("%.1f", mean(df$mpg))
    }
  })

  output$avg_hp <- renderText({
    df <- selected_data()
    if (nrow(df) == 0) {
      sprintf("%.0f", mean(data$hp))
    } else {
      sprintf("%.0f", mean(df$hp))
    }
  })

  # Statistiques
  output$stats <- renderPrint({
    df <- selected_data()

    if (nrow(df) == 0) {
      cat("Select points to see statistics\n")
      return()
    }

    cat("Selected Cars Summary\n")
    cat("====================\n\n")
    cat("Number of cars:", nrow(df), "\n\n")

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
