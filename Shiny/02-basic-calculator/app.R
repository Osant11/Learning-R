# Basic Calculator Shiny App
# Learn: numericInput, selectInput, actionButton, reactivity, updateSelectInput

library(shiny)

# Define operation choices
basic_ops <- c(
  "Add (+)" = "add",
  "Subtract (-)" = "subtract",
  "Multiply (*)" = "multiply",
  "Divide (/)" = "divide"
)

advanced_ops <- c(
  "Add (+)" = "add",
  "Subtract (-)" = "subtract",
  "Multiply (*)" = "multiply",
  "Divide (/)" = "divide",
  "Power (^)" = "power",
  "Modulo (%%)" = "modulo",
  "Integer Divide (%/%)" = "intdiv"
)

# UI - User Interface
ui <- fluidPage(

  # App title
  titlePanel("Basic Calculator"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      # Advanced mode checkbox
      checkboxInput(
        inputId = "advanced_mode",
        label = "Advanced Mode",
        value = FALSE
      ),

      hr(),

      # First number input
      numericInput(
        inputId = "num1",
        label = "First number:",
        value = 0
      ),

      # Operation selector
      selectInput(
        inputId = "operation",
        label = "Operation:",
        choices = basic_ops
      ),

      # Second number input
      numericInput(
        inputId = "num2",
        label = "Second number:",
        value = 0
      ),

      # Calculate button
      actionButton(
        inputId = "calculate",
        label = "Calculate",
        class = "btn-primary"
      )
    ),

    # Main panel for output
    mainPanel(
      h3("Result:"),
      verbatimTextOutput("result"),

      hr(),

      h4("Calculation History:"),
      verbatimTextOutput("history")
    )
  )
)

# Server - Logic
server <- function(input, output, session) {

  # Store calculation history
  history <- reactiveVal("")

  # Update selectInput when advanced mode changes
  observeEvent(input$advanced_mode, {
    if (input$advanced_mode) {
      # Show advanced operations
      updateSelectInput(
        session = session,
        inputId = "operation",
        label = "Operation (Advanced):",
        choices = advanced_ops
      )
    } else {
      # Show basic operations
      updateSelectInput(
        session = session,
        inputId = "operation",
        label = "Operation:",
        choices = basic_ops
      )
    }
  })

  # Perform calculation when button is clicked
  result <- eventReactive(input$calculate, {

    num1 <- input$num1
    num2 <- input$num2
    op <- input$operation

    # Calculate based on operation
    value <- switch(op,
      "add" = num1 + num2,
      "subtract" = num1 - num2,
      "multiply" = num1 * num2,
      "divide" = if (num2 != 0) num1 / num2 else "Error: Division by zero!",
      "power" = num1 ^ num2,
      "modulo" = if (num2 != 0) num1 %% num2 else "Error: Division by zero!",
      "intdiv" = if (num2 != 0) num1 %/% num2 else "Error: Division by zero!"
    )

    # Get operation symbol
    symbol <- switch(op,
      "add" = "+",
      "subtract" = "-",
      "multiply" = "*",
      "divide" = "/",
      "power" = "^",
      "modulo" = "%%",
      "intdiv" = "%/%"
    )

    # Create result text
    if (is.numeric(value)) {
      result_text <- paste(num1, symbol, num2, "=", value)
    } else {
      result_text <- value
    }

    # Update history
    current_history <- history()
    if (current_history == "") {
      history(result_text)
    } else {
      history(paste(result_text, current_history, sep = "\n"))
    }

    return(value)
  })

  # Display result
  output$result <- renderText({
    result()
  })

  # Display history
  output$history <- renderText({
    history()
  })

}

# Run the app
shinyApp(ui = ui, server = server)
