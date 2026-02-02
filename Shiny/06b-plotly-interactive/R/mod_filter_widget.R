# =============================================================================
# Filter Widget Module
# A Shiny module for dynamic data filtering
# =============================================================================

# -----------------------------------------------------------------------------
# Module UI
# -----------------------------------------------------------------------------

#' Filter Widget UI
#'
#' Creates a sidebar card with dynamic filter management
#'
#' @param id The module namespace ID
#' @return A tagList containing the filter widget UI
filterWidgetUI <- function(id) {
 ns <- NS(id)

  card(
    card_header(
      class = "d-flex justify-content-between align-items-center py-2",
      span(icon("filter"), "Active Filters"),
      div(
        actionButton(ns("add_filter"), "",
                    icon = icon("plus"),
                    class = "btn-sm btn-success",
                    title = "Add new filter"),
        actionButton(ns("clear_all"), "",
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
        textOutput(ns("filter_count"), inline = TRUE)
      ),

      # Container for dynamically added filters
      div(id = ns("filter_container"),
        uiOutput(ns("dynamic_filters"))
      ),

      # Placeholder when no filters
      uiOutput(ns("no_filters_message"))
    )
  )
}

#' Filter Widget Sidebar
#'
#' Creates a complete sidebar with the filter widget
#'
#' @param id The module namespace ID
#' @return A sidebar element
filterWidgetSidebar <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "Data Filters",
    width = 320,

    # The filter widget card
    filterWidgetUI(id),

    hr(),

    # Help text
    div(
      class = "text-muted small text-center",
      icon("info-circle"),
      "Click", icon("plus", class = "text-success"), "to add a filter"
    )
  )
}

# -----------------------------------------------------------------------------
# Module Server
# -----------------------------------------------------------------------------

#' Filter Widget Server
#'
#' Server logic for the dynamic filter widget
#'
#' @param id The module namespace ID
#' @param filter_config List of filter configurations for each variable
#' @param filter_choices Named vector of available filter choices
#' @return A reactive expression returning the list of active filters
filterWidgetServer <- function(id, filter_config, filter_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Reactive values to store filter state
    # -------------------------------------------------------------------------

    rv <- reactiveValues(
      filters = list(),
      filter_counter = 0
    )

    # -------------------------------------------------------------------------
    # Add Filter Modal
    # -------------------------------------------------------------------------

    observeEvent(input$add_filter, {
      # Get variables not already being filtered
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
          ns("new_filter_var"),
          "Select Variable:",
          choices = available_choices
        ),

        # Dynamic filter value input
        uiOutput(ns("new_filter_value_ui")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_add"), "Add Filter",
                      class = "btn-primary")
        )
      ))
    })

    # Render appropriate input based on selected variable
    output$new_filter_value_ui <- renderUI({
      req(input$new_filter_var)

      var_name <- input$new_filter_var
      config <- filter_config[[var_name]]

      if (config$type == "categorical") {
        selectInput(
          ns("new_filter_value"),
          paste("Select", config$label, ":"),
          choices = config$choices,
          selected = config$choices,
          multiple = TRUE
        )
      } else {
        sliderInput(
          ns("new_filter_value"),
          paste("Select", config$label, "Range:"),
          min = config$min,
          max = config$max,
          value = c(config$min, config$max),
          step = config$step
        )
      }
    })

    # Confirm adding new filter
    observeEvent(input$confirm_add, {
      req(input$new_filter_var, input$new_filter_value)

      var_name <- input$new_filter_var
      config <- filter_config[[var_name]]

      # Create unique filter ID
      rv$filter_counter <- rv$filter_counter + 1
      filter_id <- paste0("filter_", rv$filter_counter)

      # Add filter to list
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

    # -------------------------------------------------------------------------
    # Remove Filters
    # -------------------------------------------------------------------------

    # Remove individual filter
    observeEvent(input$remove_filter, {
      filter_id <- input$remove_filter
      rv$filters[[filter_id]] <- NULL
    })

    # Clear all filters
    observeEvent(input$clear_all, {
      rv$filters <- list()
    })

    # -------------------------------------------------------------------------
    # Render Dynamic Filters
    # -------------------------------------------------------------------------

    output$dynamic_filters <- renderUI({
      filters <- rv$filters

      if (length(filters) == 0) return(NULL)

      filter_ui_list <- lapply(names(filters), function(filter_id) {
        f <- filters[[filter_id]]

        # Create the filter value input
        if (f$type == "categorical") {
          value_input <- selectInput(
            inputId = ns(paste0(filter_id, "_value")),
            label = NULL,
            choices = f$config$choices,
            selected = f$value,
            multiple = TRUE,
            width = "100%"
          )
        } else {
          value_input <- sliderInput(
            inputId = ns(paste0(filter_id, "_value")),
            label = NULL,
            min = f$config$min,
            max = f$config$max,
            value = f$value,
            step = f$config$step,
            width = "100%"
          )
        }

        # Filter card with header and remove button
        div(
          class = "card mb-2",
          div(
            class = "card-header py-1 px-2 d-flex justify-content-between align-items-center",
            style = "background-color: #f8f9fa;",
            span(
              class = "small fw-bold",
              if (f$type == "categorical")
                icon("tags", class = "text-primary me-1")
              else
                icon("sliders", class = "text-info me-1"),
              f$label
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-link text-danger p-0",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                ns("remove_filter"), filter_id
              ),
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
          icon("filter", class = "fa-2x mb-2 d-block mx-auto",
               style = "opacity: 0.3;"),
          p(class = "small mb-0", "No active filters"),
          p(class = "small text-muted", "Click + to add one")
        )
      }
    })

    # Filter count text
    output$filter_count <- renderText({
      n <- length(rv$filters)
      if (n == 0) "No filters active"
      else paste(n, "filter(s) active")
    })

    # -------------------------------------------------------------------------
    # Update filter values when user changes them
    # -------------------------------------------------------------------------

    observe({
      filters <- rv$filters

      for (filter_id in names(filters)) {
        input_id <- paste0(filter_id, "_value")
        new_value <- input[[input_id]]

        if (!is.null(new_value)) {
          rv$filters[[filter_id]]$value <- new_value
        }
      }
    })

    # -------------------------------------------------------------------------
    # Return reactive filters
    # -------------------------------------------------------------------------

    return(reactive({ rv$filters }))
  })
}

# -----------------------------------------------------------------------------
# Helper function for displaying active filters as badges
# -----------------------------------------------------------------------------

#' Render Active Filters Display
#'
#' Creates a UI element showing active filters as badges
#'
#' @param filters List of active filters
#' @return A tagList of badge elements
renderFilterBadges <- function(filters) {
  if (length(filters) == 0) {
    return(tags$span(
      class = "text-muted",
      icon("info-circle"),
      " Use sidebar to add filters"
    ))
  }

  badges <- lapply(filters, function(f) {
    if (f$type == "categorical") {
      if (length(f$value) < length(f$config$choices) && length(f$value) > 0) {
        tags$span(
          class = "badge bg-primary me-1 mb-1",
          paste(f$label, ":", paste(f$value, collapse = ", "))
        )
      }
    } else {
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
    return(tags$span(
      class = "text-muted",
      icon("check-circle", class = "text-success"),
      " Filters added but showing all data (no restrictions)"
    ))
  }

  div(
    tags$span(class = "text-muted me-2", "Active filters:"),
    badges
  )
}
