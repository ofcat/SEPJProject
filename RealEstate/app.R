

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


css <- "
   body {
     background-color: #E9ECF0;
   }


"
# Define UI for application that draws a histogram


ui <- dashboardPage(

  # Doesnt work with dashboardpage for some reason, all good when using fluidPage
  # useShinyjs(),
  # tags$style(HTML("
  #                  #main-area {
  #                     margin-left: 10rem;
  #                     margin-right: 1rem;
  #                     font-family: amalia;
  #                     }
  #
  #                  ")),


  dashboardHeader(title = "ODVRE Dashboard"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Search", icon = icon("th"), tabName = "search",
               badgeLabel = "new", badgeColor = "green")
    )

  ),
  dashboardBody(
    tags$style(css),

    # Boxes need to be put in a row (or column)
    fluidRow(id = '#main-area',
     #DTOutput('BDTable')
     uiOutput('BDBox')
    )
  )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)



  output$BDTable = renderDT(
    datatable(buildingData,
              options = list(pagelength = 300,
                             scrollX = TRUE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'backround-color': '#FFF',
                               'color': 'black'});",
                               "}")),
              filter = list(position = 'top', clear = FALSE)
  ))

  boxStyle = "margin-left: 1rem; margin-right: 1rem;"

  output$BDBox = renderUI({
    box(title = "RE Data", style = boxStyle, width = 12,
                 DTOutput('BDTable')  )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
