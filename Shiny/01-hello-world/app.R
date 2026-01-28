# Hello World Shiny App
# This is the simplest Shiny app structure

library(shiny)

# UI - User Interface (what the user sees)
ui <- fluidPage(

  # App title
  titlePanel("Hello World!"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for input
    sidebarPanel(
      textInput(
        inputId = "name",
        label = "Enter your name:",
        value = "World"
      )
    ),

    # Main panel for output
    mainPanel(
      h3("Greeting:"),
      textOutput("greeting")
    )
  )
)

# Server - Logic (what happens behind the scenes)
server <- function(input, output) {

  # Render the greeting text
  output$greeting <- renderText({
    paste("Hello,", input$name, "!")
  })

}

# Run the app
shinyApp(ui = ui, server = server)
