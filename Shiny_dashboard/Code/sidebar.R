library( shiny )
library( shinydashboard )

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/rstudio/shinydashboard/")
  ), 
  sidebarMenu( 
    menuItem( "Dasboard2", tabName = "dashoboard2" )
    )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

shinyApp( ui, server )
