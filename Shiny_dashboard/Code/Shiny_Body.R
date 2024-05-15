# Boxes shiny ----
sidebar <- dashboardSidebar( )

## Status / Titles / texts / solidHearder / collapsible / solid backround...
body <- dashboardBody(

  box(title = "Histogram", status = "primary", 
      background = "maroon",
      solidHeader = TRUE, 
      collapsible = TRUE, 
      plotOutput("plot2", height = 250)),
  
  box(
    title = "Inputs",
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
  
)

ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)


# tabBox ----

body <- dashboardBody(
  fluidRow(
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  ),
  fluidRow(
    tabBox(
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "tabBox status"),
      tabPanel("Tab1",
               "Currently selected tab from first box:",
               verbatimTextOutput("tabset1Selected")
      ),
      tabPanel("Tab2", "Tab content 2")
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "tabBoxes"),
    dashboardSidebar(),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the first box
    output$tabset1Selected <- renderText({
      input$tabset1
    })
  }
)

# Put them together into a dashboardPage





