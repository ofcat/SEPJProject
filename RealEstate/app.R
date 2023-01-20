

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
#,badgeLabel = "new", badgeColor = "green"
    sidebarMenu(
      menuItem("Open Data",  icon = icon("map"),
               menuSubItem("Vienna Open Data", tabName = "dashboard"),
               menuSubItem("Open Data Dataset", tabName = "open_data_dataset",  icon = icon("database"))
               ),
      menuItem("Portfolio Selection", icon = icon("globe"),
               menuSubItem("Create New Portfolio", tabName = "create_new_portfolio"),
               menuSubItem("Explore Existing Portfolios", tabName = "explore_old_portfolios",icon = icon("database"))
               ),


      menuItem("Calculations", tabName = "calculations", icon = icon("signal"))
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
        fluidRow(
          #here for second row
          valueBoxOutput('totalCompaniesBox'),
          valueBoxOutput('totalCompaniesBox2'),
          valueBoxOutput('totalCompaniesBox3')
        ),
        fluidRow(id='#second-row',
                 uiOutput('districtPriceBox'),
                 uiOutput('districtPriceHistBox')

        ),

        fluidRow(
          uiOutput('districtPricePlotBox')
        ),

        # fluidRow(id = '#first-row',
        #     uiOutput('mainDatasetBox')
        #
        #     )

        ),
      tabItem(tabName = 'open_data_dataset',
              fluidRow(uiOutput('mainDatasetBox'))
              ),

      tabItem(tabName = "create_new_portfolio",
              fluidRow(
                valueBoxOutput('generalInfoBox1'),
                valueBoxOutput('generalInfoBox2'),
                valueBoxOutput('generalInfoBox3')

              ),
              fluidRow(
                uiOutput('testBox')
              ),

              fluidRow(
                valueBoxOutput('selectedPropBox1'),
                valueBoxOutput('selectedPropBox2'),
                valueBoxOutput('selectedPropBox3')

              ),
              fluidRow(
                uiOutput('scrapedPropDF')
              ),

              fluidRow(
                valueBoxOutput('newPropBox1'),
                valueBoxOutput('newPropBox2'),


              ),

              fluidRow(
                uiOutput('newPropDF')
              ),
              fluidRow(
                uiOutput('newPortfolioStat1'),
                uiOutput('newPortfolioStat2')
              ),
              ),

      tabItem(tabName = "calculations",

              fluidRow()
              ),
      tabItem(tabName = 'explore_old_portfolios',
              fluidRow())

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
                             scrollX = TRUE), filter = list(position = 'top', clear = FALSE)
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

  districtPrice_tbl2 = districtPrice_tbl

  districtPrice_tbl2$PLZ = as.factor(districtPrice_tbl$PLZ)

  districtPrice_boxPlot = select(buildingData, PLZ, Kaufpreis)
  #%>% group_by(PLZ,Kaufpreis)

  districtPrice_boxPlot$PLZ = as.factor(districtPrice_boxPlot$PLZ)

  #plot_ly(districtPrice_boxPlot, x=~PLZ, y=~Kaufpreis, type="box")

  # Median Prices per District table
  #output$x1 <- DT::renderDataTable(districtPrice_tbl, server = FALSE)

  output$x1 = renderDT(
    datatable(districtPrice_tbl)
                       %>% formatCurrency(2, '\U20AC', digits = 0))

  output$districtPriceBox = renderUI({
    box(title = "Median Prices per District", style = boxStyle, width = 4,
        DTOutput('x1'))
  })


#HEREEEEEEEEEEEE FOR BOXPLOTS

  # Median Prices per District Scatter plot
  # highlight selected rows in the scatterplot
  output$x2 <- renderPlotly({
    # p <- plot_ly(districtPrice_tbl2, x = ~PLZ, y = ~medianPrice ,mode = "markers",
    #              marker = list(opacity = 1, color = "black"))
    #p = plot_ly()
    p = plot_ly(districtPrice_boxPlot, x=~PLZ, y=~Kaufpreis, type="box") %>%
      layout(
        yaxis = list(
          range=c(0,30000000)
        )
      )

    s <- input$x1_rows_selected

    postcodes = c()
    collectPostcodes = function(code) {
      postcodes <<- c(postcodes, code)

    }
    # sapply(s, collectPostcodes(districtPrice_tbl[s]$PLZ))
    sapply(s, function(x) collectPostcodes(districtPrice_tbl[s,]$PLZ))

    # if (length(s)) {
    #   p <- p %>%
    #     add_trace(data = districtPrice_boxPlot[ , drop = FALSE],
    #               x = ~PLZ, y = ~Kaufpreis,
    #               marker = list(opacity = 0.2, color = "black")) %>%
    #     layout(showlegend = FALSE) %>%
    #     add_trace(data = districtPrice_boxPlot[s, , drop = FALSE],
    #               x = ~PLZ, y = ~Kaufpreis,
    #               marker = list(opacity = 1, color = "red")) %>%
    #     layout(showlegend = FALSE)
    # }
    if (length(s)) {
      p <- p %>%
        add_trace(data = filter(districtPrice_boxPlot, PLZ %in% postcodes), type = 'box',
                  x = ~PLZ, y = ~Kaufpreis, color = ~PLZ
                  ) %>%
        layout(showlegend = FALSE,
               yaxis = list(
                 range=c(0,30000000)
               ))
    }
    p
  })



  output$districtPricePlotBox = renderUI({
    box(title = "Scatterplot", style= boxStyle, width = 12,
        plotlyOutput('x2'))
  })
  # Prices Histogram



  districtPurpose = select(buildingData, PLZ, zuordnung) %>%
    group_by(zuordnung, PLZ) %>%
    summarise(count = n(), .groups = 'drop')

  districtPurpose$PLZ = as.factor(districtPurpose$PLZ)
  districtPurpose$zuordnung = as.factor(districtPurpose$zuordnung)

  # plot1010 = filter(districtPurpose, PLZ == 1010 | PLZ == 1020)
  # plot1010$PLZ = as.factor(plot1010$PLZ)
  # plot1010$zuordnung = as.factor(plot1010$zuordnung)


  output$districtPriceHist = renderPlotly({

    #plot_ly(x=plot1010$zuordnung, y = plot1010$count, type = 'bar', color = plot1010$PLZ)

    p = plot_ly()

    s <- input$x1_rows_selected
    #loop through districtPrice_tbl with s as index to get all PLZ that are selected
    postcodes = c()
    collectPostcodes = function(code) {
      postcodes <<- c(postcodes, code)

    }
  # sapply(s, collectPostcodes(districtPrice_tbl[s]$PLZ))
    sapply(s, function(x) collectPostcodes(districtPrice_tbl[s,]$PLZ))

    #browser(postcodes)
  #print(postcodes)
    # only show selected PLZ in barplot
    if (length(s)) {
      #browser(postcodes)
      p <- p %>%
        add_bars(data = filter(districtPurpose, PLZ %in% postcodes), #[s , , drop = FALSE]
                  x = ~zuordnung, y = ~count,  type = 'bar', color = ~PLZ)

    }
    p

  })

  output$districtPriceHistBox = renderUI({
    box(title = "Project Purpose", style= boxStyle, width = 8,
        plotlyOutput('districtPriceHist'))
  })


## value boxes
  output$totalCompaniesBox <- renderValueBox({
    # totalNum = nrow(startUp_csv) %>%
    #   comma(digits = 0, big.mark = '.')

    valueBox(
      'title',"Total number of properties", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$totalCompaniesBox2 <- renderValueBox({
    # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
    #   currency(digits = 0L, "$ ", big.mark = '.')
    valueBox(
      'title', "Total portfolio volume in $", icon = icon("credit-card")
    )
  }
  )
  output$totalCompaniesBox3 <- renderValueBox({
    # countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
    # countClosed = nrow(startUp_csv)
    # result = formattable((countOpenCompanies/countClosed * 100), digits = 2, format = 'f')
     valueBox( 'title',
     "% of objects in workout", icon = icon("thumbs-up", lib = 'glyphicon'),
      color = "yellow"
    )
  }
  )

  ## value boxes
  output$generalInfoBox1 <- renderValueBox({
    # totalNum = nrow(startUp_csv) %>%
    #   comma(digits = 0, big.mark = '.')

    valueBox(
      'title',"Total number of properties", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$generalInfoBox2 <- renderValueBox({
    # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
    #   currency(digits = 0L, "$ ", big.mark = '.')
    valueBox(
      'title', "Total portfolio volume in $", icon = icon("credit-card")
    )
  }
  )
  output$generalInfoBox3 <- renderValueBox({
    # countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
    # countClosed = nrow(startUp_csv)
    # result = formattable((countOpenCompanies/countClosed * 100), digits = 2, format = 'f')
    valueBox( 'title',
              "% of objects in workout", icon = icon("thumbs-up", lib = 'glyphicon'),
              color = "yellow"
    )
  }
  )

  ## value boxes
  output$selectedPropBox1 <- renderValueBox({
    # totalNum = nrow(startUp_csv) %>%
    #   comma(digits = 0, big.mark = '.')

    valueBox(
      'title',"Total number of properties", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$selectedPropBox2 <- renderValueBox({
    # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
    #   currency(digits = 0L, "$ ", big.mark = '.')
    valueBox(
      'title', "Total portfolio volume in $", icon = icon("credit-card")
    )
  }
  )
  output$selectedPropBox3 <- renderValueBox({
    # countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
    # countClosed = nrow(startUp_csv)
    # result = formattable((countOpenCompanies/countClosed * 100), digits = 2, format = 'f')
    valueBox( 'title',
              "% of objects in workout", icon = icon("thumbs-up", lib = 'glyphicon'),
              color = "yellow"
    )
  }
  )

  properties = buildingData[1:100,]

  #output$propertiesDT <- DT::renderDataTable(properties, server = FALSE)

  output$propertiesDT = renderDT(
    datatable(properties,
              options = list(pagelength = 300,
                             scrollX = TRUE), filter = list(position = 'top', clear = FALSE)
    ))

  selectedProperties = reactive({
    input$propertiesDT_rows_selected
  })
  #newPortfolioRows <- input$propertiesDT_rows_selected
  output$portfolioDT = renderDT({

    newPortfolioRows <- input$propertiesDT_rows_selected
    #newPortfolioRows = c(1,2,3),
    datatable(properties[newPortfolioRows,],
              extensions = 'Buttons',
              options = list(pagelength = 300,
                             scrollX = TRUE,
                             dom = 'Bfrtip',
                             buttons =
                               list('copy', list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel'),
                                 text = 'Download'
                               ))),
              class = "display",
              #filter = list(position = 'top', clear = FALSE)
    )})

  output$testBox = renderUI({
    box(title = "Filters for propert selection", style = boxStyle, width = 12  )
  })

  output$newPropDF = renderUI({
    box(title = "selected by user props for new portfolio", style = boxStyle, width = 12,
        DTOutput('portfolioDT'))
  })

  output$scrapedPropDF = renderUI({
    box(title = "Properties that we have scraped", style = boxStyle, width = 12,
        DTOutput("propertiesDT"))
  })

  ## value boxes
  output$newPropBox1 <- renderValueBox({
    # totalNum = nrow(startUp_csv) %>%
    #   comma(digits = 0, big.mark = '.')

    valueBox(
      'title',"Total number of properties", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$newPropBox2 <- renderValueBox({
    # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
    #   currency(digits = 0L, "$ ", big.mark = '.')
    valueBox(
      'title', "Total portfolio volume in $", icon = icon("credit-card")
    )
  }
  )

  output$newPortfolioStat1 = renderUI({
    box(title = "ggplots for new portfolio", style = boxStyle, width = 6  )
  })
  output$newPortfolioStat2 = renderUI({
    box(title = "ggplots for new portfolio", style = boxStyle, width = 6  )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
