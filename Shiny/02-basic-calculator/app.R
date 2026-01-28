# Basic Calculator Shiny App
# Learn: numericInput, selectInput, actionButton, reactivity

library(shiny)

# UI - User Interface
ui <- fluidPage(

  # App title
  titlePanel("Basic Calculator"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

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
        choices = c(
          "Add (+)" = "add",
          "Subtract (-)" = "subtract",
          "Multiply (*)" = "multiply",
          "Divide (/)" = "divide"
        )
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
server <- function(input, output) {

  # Store calculation history
  history <- reactiveVal("")

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
      "divide" = if (num2 != 0) num1 / num2 else "Error: Division by zero!"
    )

    # Get operation symbol
    symbol <- switch(op,
      "add" = "+",
      "subtract" = "-",
      "multiply" = "*",
      "divide" = "/"
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
