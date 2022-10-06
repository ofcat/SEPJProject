

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
# Define UI for application that draws a histogram


ui <- dashboardPage(

  dashboardHeader(title = "ODVRE Dashboard"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Search", icon = icon("th"), tabName = "search",
               badgeLabel = "new", badgeColor = "green")
    )

  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
     uiOutput('BDBox')
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)



  output$BDTable = renderDT(
    datatable(buildingData,
              options = list(pagelength = 400,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'backround-color': '#FFF',
                               'color': '#29476B'});",
                               "}"))
                             )
  )

  boxStyle = "margin-left: 1rem; margin-right: 1rem;"

  output$BDBox = renderUI({
    box(title = "RE Data",
        fluidRow(style = boxStyle,
                 DTOutput('BDTable')  ))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
