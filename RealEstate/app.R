

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)

# you can put css here, but i would rather have it in a sepate css file
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
        # Boxes need to be put in a row (or column)
        # fluid row id is used for css, but this should probably be deleted
        fluidRow(id = '#first-row',
            #DTOutput('mainDataset')
            uiOutput('mainDatasetBox')

            ),
        fluidRow(id='#second-row',
                uiOutput('districtPriceBox'),
                uiOutput('districtPricePlotBox'))
        ),
      tabItem(tabName = "search")

  ),


  )




)


# Define server logic required to draw a histogram
server <- function(input, output) {

  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)
  # some data cleaning definetely needed
  buildingData$Kaufpreis.. = parse_number(buildingData$Kaufpreis..)#as.numeric(buildingData$Kaufpreis..)

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
  districtPrice_tbl = select(buildingData, PLZ, Kaufpreis..) %>%
     group_by(PLZ) %>%
      summarise(
        medianPrice = median(Kaufpreis.., na.rm=TRUE)
      )


  output$districtPriceOutput = renderDT(
    datatable(districtPrice_tbl)
  )

  output$districtPriceBox = renderUI({
    box(title = "Median Prices per District", style = boxStyle, width = 6,
        DTOutput('districtPriceOutput'))
  })

  output$districtPricePlot = renderPlot({
    s = input$districtPriceOutput_rows_selected
    par(mar = c(4, 4, 1, .1))
    #choosing plots
    plot(districtPrice_tbl, xlim = c(0, 1900))
    #plot_ly(districtPrice_tbl, x= districtPrice_tbl$PLZ, y = districtPrice_tbl$Kaufpreis..)
    if (length(s)) points(districtPrice_tbl[s, , drop = FALSE], pch = 19, cex = 2)
  })

  output$districtPricePlotBox = renderUI({
    box(title = "Scatterplot", style= boxStyle, width = 6,
        plotOutput('districtPricePlot'))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
