

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)

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
            uiOutput('mainDatasetBox')

            ),
        fluidRow(id='#second-row',
                uiOutput('districtPriceBox'),
                uiOutput('districtPricePlotBox'),
                uiOutput('districtPriceHistBox')

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


  # Median Prices per District table
  output$x1 <- DT::renderDataTable(districtPrice_tbl, server = FALSE)

  output$districtPriceBox = renderUI({
    box(title = "Median Prices per District", style = boxStyle, width = 4,
        DTOutput('x1'))
  })




  # Median Prices per District Scatter plot
  # highlight selected rows in the scatterplot
  output$x2 <- renderPlotly({
    p <- plot_ly(districtPrice_tbl, x = ~PLZ, y = ~medianPrice ,mode = "markers",
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
    box(title = "Scatterplot", style= boxStyle, width = 4,
        plotlyOutput('x2'))
  })
  # Prices Histogram



  districtPurpose = select(buildingData, PLZ, zuordnung) %>%
    group_by(zuordnung, PLZ) %>%
    summarise(count = n(), .groups = 'drop')

  districtPurpose$PLZ = as.factor(districtPurpose$PLZ)
  districtPurpose$zuordnung = as.factor(districtPurpose$zuordnung)

  plot1010 = filter(districtPurpose, PLZ == 1010 | PLZ == 1020)
  plot1010$PLZ = as.factor(plot1010$PLZ)
  plot1010$zuordnung = as.factor(plot1010$zuordnung)


  output$districtPriceHist = renderPlotly({

    #plot_ly(x=plot1010$zuordnung, y = plot1010$count, type = 'bar', color = plot1010$PLZ)

    p = plot_ly()

    s <- input$x1_rows_selected
    #loop through districtPrice_tbl with s as index to get all PLZ that are selected
    # only show selected PLZ in barplot
    if (length(s)) {
      p <- p %>%
        add_bars(data = filter(districtPurpose, PLZ %in% s), #[s , , drop = FALSE]
                  x = ~zuordnung, y = ~count,  type = 'bar', color = ~PLZ)

    }
    p

  })

  output$districtPriceHistBox = renderUI({
    box(title = "Project Purpose", style= boxStyle, width = 4,
        plotlyOutput('districtPriceHist'))
  })




}

# Run the application
shinyApp(ui = ui, server = server)
