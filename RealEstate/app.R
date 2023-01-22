

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinyWidgets)
library(reshape2)


# Define UI for application


ui <- dashboardPage(skin = "black",




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
               )


     # menuItem("Calculations", tabName = "calculations", icon = icon("signal"))
    )

  ),
  dashboardBody(





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

        fluidRow(
          uiOutput("html_out")
        )



        ),
      tabItem(tabName = 'open_data_dataset',
              fluidRow(


                #uiOutput('filtersBox')

                  #output goes here
                  box(title = "Filters", width = 12,
                      ##first row of filters
                      column(width = 3,
                             selectInput(inputId = 'katastralgemeinde', label = 'Katastralgemeinde',
                                         unique(buildingData$Katastralgemeinde), selected = NULL, multiple = T)
                      )
                      ,

                      column(width = 3,
                             selectInput(inputId = 'plz', label = 'Post Code',
                                         unique(buildingData$PLZ), selected = NULL, multiple = T)   )
                      ,

                      column(width = 3,
                             selectInput(inputId = 'erwArt', label = 'Contract type',
                                         unique(buildingData$ErwArt), selected = NULL, multiple = T))
                      ,
                      column(width = 3,
                             selectInput(inputId = 'erwDate', label = 'Contract date',
                                         unique(buildingData$Erwerbsdatum), selected = NULL, multiple = T))
                      ,

                      ## second row of filters

                      column(width = 3,
                             selectInput(inputId = 'widmung', label = 'Widmung',
                                         unique(buildingData$Widmung), selected = NULL, multiple = T)),
                      column(width = 3,
                             selectInput(inputId = 'bauklasse', label = 'Bauklasse',
                                         unique(buildingData$Bauklasse), selected = NULL, multiple = T)),
                      column(width = 3,
                             selectInput(inputId = 'schutzzone', label = 'Schutzzone',
                                         unique(buildingData$Schutzzone), selected = NULL, multiple = T)),
                      column(width = 3,
                             selectInput(inputId = 'wohnzone', label = 'wohnzone',
                                         unique(buildingData$Wohnzone), selected = NULL, multiple = T)),
                      ## third row of filters

                      column(width = 6,
                             sliderInput(inputId = 'priceSlider', label = 'Price',
                                         min = 0,
                                         max = 168909500, #updateSlider function is not picking up for some reason, so hardcoding values for now
                                         value = c(0,168909500),
                                         step = 100000
                                         )),
                      column(width = 6,
                             sliderInput(inputId = 'areaSlider', label = 'Area',
                                         min = 0,
                                         max = 1187781,
                                         value = c(0,1187781),
                                         step = 100
                                          )),

                  ),





                ),
              fluidRow(uiOutput('mainDatasetBox'))
              ),

      tabItem(tabName = "create_new_portfolio",
              fluidRow(
                valueBoxOutput('generalInfoBox1'),
                valueBoxOutput('generalInfoBox2'),
                valueBoxOutput('generalInfoBox3')

              ),
              # fluidRow(
              #   uiOutput('testBox')
              # ),
              #
              # fluidRow(
              #   valueBoxOutput('selectedPropBox1'),
              #   valueBoxOutput('selectedPropBox2'),
              #   valueBoxOutput('selectedPropBox3')
              #
              # ),
              fluidRow(


                #output goes here
                box(title = "Filters", width = 12,
                    ##first row of filters
                    column(width = 4,
                           selectInput(inputId = 'agency', label = 'Agency',
                                       unique(properties$agency), selected = NULL, multiple = T)
                    )
                    ,

                    column(width = 4,
                           selectInput(inputId = 'location', label = 'Post Code',
                                       unique(properties$location), selected = NULL, multiple = T)   )
                    ,

                    column(width = 4,
                           sliderInput(inputId = 'areaSlider2', label = 'Property Area',
                                       min = 26,
                                       max = 750, #updateSlider function is not picking up for some reason, so hardcoding values for now
                                       value = c(26,750),
                                       step = 10
                           )),
                    column(width = 4,
                           sliderInput(inputId = 'priceSlider2', label = 'Selling Price',
                                       min = 0,
                                       max = 15000000, #updateSlider function is not picking up for some reason, so hardcoding values for now
                                       value = c(0,15000000),
                                       step = 10000
                           )),
                    column(width = 4,
                           sliderInput(inputId = 'rentSlider', label = 'Monthly Rent',
                                       min = 0,
                                       max = 105000, #updateSlider function is not picking up for some reason, so hardcoding values for now
                                       value = c(0,105000),
                                       step = 1000
                           )),

                    column(width = 4,
                           sliderInput(inputId = 'annualRentSlider', label = 'Annual Rent Revenue',
                                       min = 0,
                                       max = 1260000, #updateSlider function is not picking up for some reason, so hardcoding values for now
                                       value = c(0,1260000),
                                       step = 10000
                           ))

                    )


              ),
              fluidRow(
                uiOutput('scrapedPropDF')
              ),

              fluidRow(
                valueBoxOutput('newPropBox1', width = 4),
                valueBoxOutput('newPropBox2', width = 4),
                valueBoxOutput('newPropBox5', width = 4)

              ),
              fluidRow(
                valueBoxOutput('newPropBox4', width = 6),
                valueBoxOutput('newPropBox3', width = 6)
              ),

              fluidRow(
                uiOutput('newPropDF'),
                #actionButton("saveButton", "Save portfolio to the DB")
              ),
              fluidRow(
                uiOutput('newPortfolioStat1'),
                uiOutput('newPortfolioStat2'),
                # textOutput("selected"),
                # DTOutput('saveTest')
              ),
              fluidRow(
                box(title = 'Mortgage Calculator', width = 4,
                    numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
                    hr(),
                    numericInput("interest", "Annual interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
                    hr(),
                    sliderInput("length", "Duration of the loan (in years)",
                                min = 0,
                                max = 30,
                                value = 25,
                                step = 1
                    ),
                    uiOutput('MortgageResults')

                    ),
                uiOutput('mortgageCalcBox')
              )
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

  ##SETTING UP THE DB

  library(RSQLite)
  con <- dbConnect(SQLite(), dbname = "sepjDB.sqlite")
  # dbWriteTable(con, "mtcars", mtcars)
  # dbGetQuery(con, "select * from mtcars")

  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)
  names(buildingData)[names(buildingData) == 'Kaufpreis'] <- 'Kaufpreis'
  # some data cleaning definitely needed
  buildingData$Kaufpreis = parse_number(buildingData$Kaufpreis)#as.numeric(buildingData$Kaufpreis)

  # updateSliderInput(session, "priceSlider",
  #                   min = min(buildingData$Kaufpreis),
  #                   max = max(buildingData$Kaufpreis),
  #                   value = mean(buildingData$Kaufpreis)
  # )
  #
  # updateSliderInput(session, "areaSlider",
  #                   min = min(buildingData$Gst.Fl.),
  #                   max = max(buildingData$Gst.Fl.),
  #                   value = mean(buildingData$Gst.Fl.)
  # )

 ### FILTERS FOR DATASET PAGE

  buildingDataReactive = reactive({

    data = buildingData

    if(!is.null(input$katastralgemeinde)){
      data = data %>% filter(Katastralgemeinde %in% input$katastralgemeinde)
    }
    if(!is.null(input$plz)){
      data = data %>% filter(PLZ %in% input$plz)
    }
    if(!is.null(input$erwArt)){
      data = data %>% filter(ErwArt %in% input$erwArt)
    }
    if(!is.null(input$erwDate)){
      data = data %>% filter(Erwerbsdatum %in% input$erwDate)
    }
    #SECOND ROW FILTERS
    if(!is.null(input$widmung)){
      data = data %>% filter(Widmung %in% input$widmung)
    }
    if(!is.null(input$bauklasse)){
      data = data %>% filter(Bauklasse %in% input$bauklasse)
    }
    if(!is.null(input$schutzzone)){
      data = data %>% filter(Schutzzone %in% input$schutzzone)
    }
    if(!is.null(input$wohnzone)){
      data = data %>% filter(Wohnzone %in% input$wohnzone)
    }
    # THIRD ROW
    if(!is.null(input$priceSlider)){
      data = data %>% filter(Kaufpreis >= input$priceSlider[1] & Kaufpreis <= input$priceSlider[2])
    }
    if(!is.null(input$areaSlider)){
      data = data %>% filter(Gst.Fl. >= input$areaSlider[1] & Gst.Fl. <= input$areaSlider[2])
    }


    return(data)
  })






# Main dataset shown on the title page
  output$mainDataset = renderDT(
    datatable(buildingDataReactive(),
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
      '42.657',"Total number of properties recorded", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$totalCompaniesBox2 <- renderValueBox({
    totalCapital = sum(buildingData$Kaufpreis, na.rm = T) %>%
      currency(digits = 0L, "\U20AC", big.mark = '.')
    valueBox(
      totalCapital, "Total trade volume", icon = icon("credit-card")
    )
  }
  )
  output$totalCompaniesBox3 <- renderValueBox({
    # countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
    # countClosed = nrow(startUp_csv)
     result = 80075611 %>%
       currency(digits = 0L, "", big.mark = '.')
     valueBox( result,
     "Total RE area sold (in m2)", icon = icon("thumbs-up", lib = 'glyphicon'),
      color = "yellow"
    )
  }
  )
  output$html_out <- renderUI({

    tags$body(HTML('<div><script defer="defer" type="text/javascript"
                   src="https://www.wien.gv.at/flaechenwidmung/public/ApiGetViennaGisMap.ctrl?containerWidth=1000&amp;containerHeight=500&amp;centerContainer=1&amp;lang=de&amp;bookmark=qM8fRQjROMQWCNFGEDnPRm3-cOJYRKZKu9yG4L0Gu3CftCQ-b-b&amp;bmadr="></script></div>'))
  })


  ###second pagee

  ## value boxes
  output$generalInfoBox1 <- renderValueBox({
    totalNum = nrow(properties) %>%
      comma(digits = 0, big.mark = '.')

    valueBox(
      totalNum,"Total number of properties", icon = icon("list"),
      color = "maroon"
    )
  }
  )

  output$generalInfoBox2 <- renderValueBox({
    totalCapital = sum(properties$price, na.rm = TRUE) %>%
      currency(digits = 0L, "\U20AC ", big.mark = '.')
    valueBox(
      totalCapital, "Total value", icon = icon("credit-card"),
      color = 'aqua'
    )
  }
  )
  output$generalInfoBox3 <- renderValueBox({
    totalArea = sum(properties$floor_area, na.rm = TRUE)%>%
      currency(digits = 0L, "", big.mark = '.')
    valueBox( totalArea,
              "Total area avalaible (m2)", icon = icon("thumbs-up", lib = 'glyphicon'),
              color = "teal"
    )
  }
  )

  ### PORTFOLIO SELECTION PAGE

  #importing dataset

  properties = read.csv(file = 'data/WillHaben_data_clean.csv')
  #properties = properties[,-1]
  properties$location = as.factor(properties$location)

  properties = properties %>%
    mutate(
      rentPrice = price * 0.007,
      annualRent = rentPrice * 12
    )

  #properties = properties[, c(5,4,3,2,7,8,1,6)]
  properties = properties[, c(6,5,4,3,8,9,2,7,1)]
  #
  #properties$price = currency(properties$price, '\U20AC', digits = 0)
  # properties$rentPrice = currency(properties$rentPrice, '\U20AC', digits = 0)
  # properties$annualRent = currency(properties$annualRent, '\U20AC', digits = 0)

  propertiesReactive = reactive({

    data = properties

    if(!is.null(input$agency)){
      data = data %>% filter(agency %in% input$agency)
    }
    if(!is.null(input$location)){
      data = data %>% filter(location %in% input$location)
    }
    if(!is.null(input$areaSlider2)){
      data = data %>% filter(floor_area >= input$areaSlider2[1] & floor_area <= input$areaSlider2[2])
    }

    #SECOND ROW FILTERS
    if(!is.null(input$priceSlider2)){
      data = data %>% filter(price >= input$priceSlider2[1] & price <= input$priceSlider2[2])
    }
    if(!is.null(input$rentSlider)){
      data = data %>% filter(rentPrice >= input$rentSlider[1] & rentPrice <= input$rentSlider[2])
    }
    if(!is.null(input$annualRentSlider)){
      data = data %>% filter(annualRent >= input$annualRentSlider[1] & annualRent <= input$annualRentSlider[2])
    }



    return(data)
  })


  ## value boxes
  output$selectedPropBox1 <- renderValueBox({
    totalNum = nrow(properties) %>%
      comma(digits = 0, big.mark = '.')

    valueBox(
      totalNum,"Total number of properties", icon = icon("list"),
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
              color = "olive"
    )
  }
  )

  #properties = buildingData[1:100,]

  #output$propertiesDT <- DT::renderDataTable(properties, server = FALSE)
  #currency(digits = 0L, "\U20AC", big.mark = '.')
  output$propertiesDT = renderDT(
    datatable(propertiesReactive(),
              options = list(pagelength = 300,
                             scrollX = TRUE), filter = list(position = 'top', clear = FALSE)
    ) %>% formatCurrency(c(4:6), '\U20AC', digits = 0))

  selectedProperties = reactive({
    input$propertiesDT_rows_selected
  })
  #newPortfolioRows <- input$propertiesDT_rows_selected
  output$portfolioDT = renderDT({

    newPortfolioRows <- input$propertiesDT_rows_selected
    #newPortfolioRows = c(1,2,3),
    testingSave <<- properties[newPortfolioRows,]
    ###BUG HEREEEEEEEEEEEEEEEEEE
    datatable(propertiesReactive()[newPortfolioRows,],
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
    )%>% formatCurrency(c(4:6), '\U20AC', digits = 0)

    })

  # selectedRow <- eventReactive(input$propertiesDT_rows_selected,{
  #   row.names(properties)[c(input$propertiesDT_rows_selected)]
  # })

  # observeEvent(input$saveButton, {
  #   dtSave = properties[selectedProperties(),]
  #  # dbWriteTable(con, "portfolios", properties[selectedProperties(),])
  # })
  # output$selected <- renderPrint({
  #  # selectedRow()
  #   #properties[ selectedRow(),]
  #   selectedProperties()
  #
  # })
  # output$saveTest = renderDT({
  #   datatable(dtSave)
  # })

  #browser(selectedRow())

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
    # totalNum = nrow(propertiesReactive()) %>%
    #   comma(digits = 0, big.mark = '.')

    valueBox(
      length(input$propertiesDT_rows_selected),"№ of properties in portfolio", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$newPropBox2 <- renderValueBox({
    # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
    #   currency(digits = 0L, "$ ", big.mark = '.')
    totalArea = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$floor_area) %>%
      currency(digits = 0L, "", big.mark = '.')
    valueBox(
      totalArea, "Total area in portfolio (in m2)", icon = icon("credit-card"), color = 'green'
    )
  }
  )
  output$newPropBox3 <- renderValueBox({
    totalCapital = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$price) %>%
      currency(digits = 0L, "\U20AC ", big.mark = '.')
    valueBox(
      totalCapital, "Total portfolio volume in EUR", icon = icon("credit-card"), color = 'aqua'
    )
  }
  )

  output$newPropBox4 <- renderValueBox({
    totalAnnualRent = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$annualRent) %>%
      currency(digits = 0L, "\U20AC ", big.mark = '.')
    valueBox(
      totalAnnualRent, "Annual rent revenue in EUR", icon = icon("credit-card"), color = 'olive'
    )
  }
  )

  output$newPropBox5 <- renderValueBox({
   totalPrice = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$price)
    totalArea = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$floor_area)
    averagePricePerMeter = (totalPrice / totalArea) %>%
      currency(digits = 0L, "\U20AC ", big.mark = '.')
    valueBox(
      averagePricePerMeter, "Average price per sqm in EUR", icon = icon("credit-card"), color = 'olive'
    )

  }
  )
  ## SAVING NEW PORTFOLIO TO DB
 # newPortfolioRows <- input$propertiesDT_rows_selected
  #dbWriteTable(con, "portfolios", properties[selectedProperties(),])

  output$newPortfolioChart1 = renderPlotly({


    plot_ly(propertiesReactive()[input$propertiesDT_rows_selected,],
            type="pie",
            labels=propertiesReactive()[input$propertiesDT_rows_selected,]$location,
            values=propertiesReactive()[input$propertiesDT_rows_selected,]$price,
            textinfo='label+percent')


  })

  output$newPortfolioChart2 = renderPlotly({

    plot_ly(propertiesReactive()[input$propertiesDT_rows_selected,],
            type="pie",
            labels=propertiesReactive()[input$propertiesDT_rows_selected,]$location,
            values=propertiesReactive()[input$propertiesDT_rows_selected,]$floor_area,
            textinfo='label+percent')

  })

  output$newPortfolioStat1 = renderUI({
    box(title = "Capital distribution per District", style = boxStyle, width = 6 ,
        plotlyOutput('newPortfolioChart1'))
  })
  output$newPortfolioStat2 = renderUI({
    box(title = "Area Distribution per District", style = boxStyle, width = 6,
        plotlyOutput('newPortfolioChart2'))
  })


  output$mortgageCalc <- renderUI({

   #  tags$body(HTML(
   #  '<div style="width960px; padding-left:150px; padding-right:150px; position:relative;"><iframe
   #  src ="https://www.mortgagecalculator.net/embeddable/v2/?size=1&textColor=003140&backgroundColor=e7f0f3"
   #  width="100%" frameborder=0 scrolling=no height=330>
   #  </iframe>
   # </div>'))


  })

  output$mortgageCalcBox = renderUI({
    box(title = "ggplots for new portfolio", style = boxStyle, width = 8,
        plotlyOutput('distPlot'))
  })

  #### MORTGAGE CALCULATOR
  ## inspired by Antoine Soetewey
  ## taken from here, link: https://statsandr.com/blog/mortgage-calculator-r-shiny/
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
   # if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")

   plot2 <- ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") +
       scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")

   #plot_ly(aDFyear2, type = 'bar', x =~Year, y=~value)
   ggplotly(plot2)
    #}
  }

  output$distPlot <- renderPlotly({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = TRUE)
  })

  output$MortgageResults <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Summary", "</h3>",
      "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })



}





# Run the application
shinyApp(ui = ui, server = server)
