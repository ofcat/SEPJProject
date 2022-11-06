

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)

# you can put css here, but i would rather have it in a separte css file
#check link below
css <- "



"
# Define UI for application


ui <- dashboardPage(skin = "black",

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

    # css doesnt work now, but here is the link to figure it out
    # https://rstudio.github.io/shinydashboard/appearance.html#css

    tags$head(tags$style(HTML(css))),
    #tags$style(css),

    tabItems(
      tabItem(tabName = "dashboard",
        # Boxes need to be put in a row
        # fluid row id is used for css, but this should probably be deleted
        fluidRow(id = '#first-row',
            #DTOutput('mainDataset')
            uiOutput('mainDatasetBox')

            ),
        fluidRow(id='#second-row',
                # uiOutput('districtPriceBox'),
                # uiOutput('districtPricePlotBox')
                uiOutput('districtPriceBox'),
                uiOutput('districtPricePlotBox')
                )
        ),
      tabItem(tabName = "search",
              fluidRow(
                # uiOutput('x1Box'),
                # uiOutput('x2Box')
              ))

  ),


  )




)


# Define server logic required to draw a histogram
server <- function(input, output) {

  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)
  names(buildingData)[names(buildingData) == 'Kaufpreis'] <- 'Kaufpreis'
  # some data cleaning definetely needed
  buildingData$Kaufpreis = parse_number(buildingData$Kaufpreis)#as.numeric(buildingData$Kaufpreis)

# Main dataset shown on the title page
  output$mainDataset = renderDT(
    datatable(buildingData,
              options = list(pagelength = 300,
                             scrollX = TRUE),
              #code below can be used to use JS snippents inside R, but thats useless now
                             # initComplete = JS(
                             #   "function(settings, json) {",
                             #   "$(this.api().table().header()).css({'backround-color': 'red'
                             #  });",
                             #   "}")),
              filter = list(position = 'top', clear = FALSE)
  ))

  # styling applied to all boxes
  boxStyle = "margin-left: 1rem; margin-right: 1rem;"

  #box used to display the table with the main dataset
  output$mainDatasetBox = renderUI({
    box(title = "RE Data", style = boxStyle, width = 12,
                 DTOutput('mainDataset')  )
  })

  #Second row on dashboard tab
  ## table with two columns, auto plotting with DT
  districtPrice_tbl = select(buildingData, PLZ, Kaufpreis) %>%
     group_by(PLZ) %>%
      summarise(
        medianPrice = median(Kaufpreis, na.rm=TRUE)
      )


  # output$districtPriceOutput = renderDT(
  #   datatable(districtPrice_tbl)
  # )

  output$districtPriceOutput = renderDataTable(
    districtPrice_tbl,
    server=FALSE
  )

  output$districtPriceBox = renderUI({
    box(title = "Median Prices per District", style = boxStyle, width = 6,
        DTOutput('x1'))
  })



  output$x1 <- DT::renderDataTable(districtPrice_tbl, server = FALSE)

  # highlight selected rows in the scatterplot
  output$x2 <- renderPlotly({
    p <- plot_ly(districtPrice_tbl, x = ~PLZ, y = ~medianPrice, mode = "markers",
                 marker = list(opacity = 1, color = "black")) %>%
      layout(
             xaxis = list(
                range=c(0,1900)
              )
            )
    s <- input$x1_rows_selected
    if (length(s)) {
      p <- p %>%
        add_trace(data = districtPrice_tbl[ , drop = FALSE],
                  x = ~PLZ, y = ~medianPrice, mode = "markers",
                  marker = list(opacity = 0.2, color = "black")) %>%
        layout(showlegend = FALSE) %>%
        add_trace(data = districtPrice_tbl[s, , drop = FALSE],
                  x = ~PLZ, y = ~medianPrice, mode = "markers",
                  marker = list(opacity = 1, color = "red")) %>%
        layout(showlegend = FALSE)
    }
    p
  })






  output$districtPricePlotBox = renderUI({
    box(title = "Scatterplot", style= boxStyle, width = 6,
        plotlyOutput('x2'))
  })


  ## cars example
  ##
  #
  #
  #







}

# Run the application
shinyApp(ui = ui, server = server)
