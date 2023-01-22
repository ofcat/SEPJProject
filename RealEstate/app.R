

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


# UI for application


ui <- dashboardPage(skin = "black",




  dashboardHeader(title = "ODVRE Dashboard"),
  dashboardSidebar(

    ## creating menu for website navigation with two items and two subitems
    sidebarMenu(
      menuItem("Open Data",  icon = icon("map"),
               menuSubItem("Vienna Open Data", tabName = "dashboard"),
               menuSubItem("Open Data Dataset", tabName = "open_data_dataset",  icon = icon("database"))
               ),
      menuItem("Portfolio Selection", icon = icon("globe"),
               menuSubItem("Create New Portfolio", tabName = "create_new_portfolio"),
               menuSubItem("Explore Existing Portfolios", tabName = "explore_old_portfolios",icon = icon("database"))
               )

    )

  ),
  dashboardBody(





    tabItems(
      # HOME PAGE
      tabItem(tabName = "dashboard",

        fluidRow(

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

      # DATASET PAGE
      tabItem(tabName = 'open_data_dataset',
              fluidRow(

                  # filters box for the Open Data Dataset
                  box(title = "Search filters", width = 12,
                      ## first row of filters
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

      # CREATE NEW PORTFOLIO PAGE
      tabItem(tabName = "create_new_portfolio",
              # general infoboxes from the top of the page
              fluidRow(
                valueBoxOutput('generalInfoBox1'),
                valueBoxOutput('generalInfoBox2'),
                valueBoxOutput('generalInfoBox3')

              ),
              fluidRow(
                ## Filters for Willhaben dataset
                box(title = "Search filters", width = 12,

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
                    # second row
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
                # willhaben dataframe
                uiOutput('scrapedPropDF')
              ),

              # user selected portfolio infoboxes
              fluidRow(
                valueBoxOutput('newPropBox1', width = 4),
                valueBoxOutput('newPropBox2', width = 4),
                valueBoxOutput('newPropBox5', width = 4)

              ),
              fluidRow(
                valueBoxOutput('newPropBox4', width = 4),
                valueBoxOutput('newPropBox3', width = 4),
                valueBoxOutput('newPropBox6', width = 4)
              ),

              # user selected portfolio dataframe
              fluidRow(
                uiOutput('newPropDF'),

              ),
              # pie charts for user selected portfolio
              fluidRow(
                uiOutput('newPortfolioStat1'),
                uiOutput('newPortfolioStat2'),

              ),
              # mortgage calculator section
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


      tabItem(tabName = 'explore_old_portfolios',
              fluidRow())

  ),


  )

)


# Define server logic required to draw a histogram
server <- function(input, output) {


  # data wrangling with Open Data Dataset
  buildingData = read.table("data/dataRealEstate.txt", sep = ";", header = TRUE)
  names(buildingData)[names(buildingData) == 'Kaufpreis'] <- 'Kaufpreis'
  buildingData$Kaufpreis = parse_number(buildingData$Kaufpreis)

  ## these functions did not work for some reason, i would look into this in my free time
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
  #creating reactive value for ODD (open data dataset) that will respond to user input in filters

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






# ODD shown on the second page
  output$mainDataset = renderDT(
    datatable(buildingDataReactive(),
              options = list(pagelength = 300,
                             scrollX = TRUE), filter = list(position = 'top', clear = FALSE)
  ))

  # styling applied to all boxes shown on the dashboard
  boxStyle = "margin-left: 1rem; margin-right: 1rem;"

  #box used to display the table with the main dataset
  output$mainDatasetBox = renderUI({
    box(title = "ODD (Open Data Dataset)", style = boxStyle, width = 12,
                 DTOutput('mainDataset')  )
  })

  #Second row on home tab
  ## table with two columns, auto plotting with DT
  # table used to shown median price per vienna district

  districtPrice_tbl = select(buildingData, PLZ, Kaufpreis) %>%
     group_by(PLZ) %>%
      summarise(
        medianPrice = median(Kaufpreis, na.rm=TRUE)
      )


  districtPrice_tbl2 = districtPrice_tbl
  districtPrice_tbl2$PLZ = as.factor(districtPrice_tbl$PLZ)

  # similar table, but used to create boxplots for all Vienna districts to analyse selling price
  districtPrice_boxPlot = select(buildingData, PLZ, Kaufpreis)
  districtPrice_boxPlot$PLZ = as.factor(districtPrice_boxPlot$PLZ)



  # Median Prices per District table

 #function and box to output median price per PLZ on the home tab
  output$x1 = renderDT(
    datatable(districtPrice_tbl)
                       %>% formatCurrency(2, '\U20AC', digits = 0))

  output$districtPriceBox = renderUI({
    box(title = "Median Prices per District", style = boxStyle, width = 4,
        DTOutput('x1'))
  })




  # Boxplot Prices per District
  # highlight selected rows in the boxplot
  output$x2 <- renderPlotly({

    p = plot_ly(districtPrice_boxPlot, x=~PLZ, y=~Kaufpreis, type="box") %>%
      layout(
        yaxis = list(
          range=c(0,30000000)
        )
      )

    # accesing selected rows
    s <- input$x1_rows_selected

    # collecting all postcodes from the datable to highlight them in the boxplot
    postcodes = c()
    collectPostcodes = function(code) {
      postcodes <<- c(postcodes, code)

    }

    sapply(s, function(x) collectPostcodes(districtPrice_tbl[s,]$PLZ))

    # highlighting here
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
    box(title = "Analysing Price Distribution in Vienna Districts", style= boxStyle, width = 12,
        plotlyOutput('x2'))
  })

  # Analysing project purpose per district
  ##preparing the data

  districtPurpose = select(buildingData, PLZ, zuordnung) %>%
    group_by(zuordnung, PLZ) %>%
    summarise(count = n(), .groups = 'drop')

  districtPurpose$PLZ = as.factor(districtPurpose$PLZ)
  districtPurpose$zuordnung = as.factor(districtPurpose$zuordnung)


  output$districtPriceHist = renderPlotly({

    p = plot_ly()

    s <- input$x1_rows_selected

    #loop through districtPrice_tbl with s as index to get all PLZ that are selected
    #reusing the function
    postcodes = c()
    collectPostcodes = function(code) {
      postcodes <<- c(postcodes, code)

    }

    sapply(s, function(x) collectPostcodes(districtPrice_tbl[s,]$PLZ))


    # only show selected PLZ in barplot
    if (length(s)) {
      p <- p %>%
        add_bars(data = filter(districtPurpose, PLZ %in% postcodes),
                  x = ~zuordnung, y = ~count,  type = 'bar', color = ~PLZ)

    }
    p

  })

  output$districtPriceHistBox = renderUI({
    box(title = "Project Purpose Ditribution per District", style= boxStyle, width = 8,
        plotlyOutput('districtPriceHist'))
  })


## value boxes showed on top of the home tab
  output$totalCompaniesBox <- renderValueBox({

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

     result = 80075611 %>%
       currency(digits = 0L, "", big.mark = '.')
     valueBox( result,
     "Total RE area sold (in m2)", icon = icon("thumbs-up", lib = 'glyphicon'),
      color = "yellow"
    )
  }
  )

  # map showed on the bottom the page
  #taken from wien.gv.at
  output$html_out <- renderUI({

    tags$body(HTML('<div><script defer="defer" type="text/javascript"
                   src="https://www.wien.gv.at/flaechenwidmung/public/ApiGetViennaGisMap.ctrl?containerWidth=1000&amp;containerHeight=500&amp;centerContainer=1&amp;lang=de&amp;bookmark=qM8fRQjROMQWCNFGEDnPRm3-cOJYRKZKu9yG4L0Gu3CftCQ-b-b&amp;bmadr="></script></div>'))
  })


  # SELECTING PORTFOLIO PAGE

  ## value boxes from the top pf the page
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



  #importing dataset from willhaben

  #some data manipulations

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

  # currency conversion done later in datatables
  # properties$price = currency(properties$price, '\U20AC', digits = 0)
  # properties$rentPrice = currency(properties$rentPrice, '\U20AC', digits = 0)
  # properties$annualRent = currency(properties$annualRent, '\U20AC', digits = 0)

  # creating reactive variable for filters

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


  # ## value boxes
  # output$selectedPropBox1 <- renderValueBox({
  #   totalNum = nrow(properties) %>%
  #     comma(digits = 0, big.mark = '.')
  #
  #   valueBox(
  #     totalNum,"Total number of properties", icon = icon("list"),
  #     color = "purple"
  #   )
  # }
  # )
  #
  # output$selectedPropBox2 <- renderValueBox({
  #   # totalCapital = sum(startUp_csv$funding_total_usd, na.rm = TRUE) %>%
  #   #   currency(digits = 0L, "$ ", big.mark = '.')
  #   valueBox(
  #     'title', "Total portfolio volume in $", icon = icon("credit-card")
  #   )
  # }
  # )
  # output$selectedPropBox3 <- renderValueBox({
  #   # countOpenCompanies = nrow(filter(startUp_csv, status == 'operating' |status == 'acquired'))
  #   # countClosed = nrow(startUp_csv)
  #   # result = formattable((countOpenCompanies/countClosed * 100), digits = 2, format = 'f')
  #   valueBox( 'title',
  #             "% of objects in workout", icon = icon("thumbs-up", lib = 'glyphicon'),
  #             color = "olive"
  #   )
  # }
  # )



  # printing the main willhaben data to the page
  output$propertiesDT = renderDT(
    datatable(propertiesReactive(),
              options = list(pagelength = 300,
                             scrollX = TRUE), filter = list(position = 'top', clear = FALSE)
    ) %>% formatCurrency(c(4:6), '\U20AC', digits = 0))

  # reactive function that holds selected rows from the main dataset --> not used
  selectedProperties = reactive({
    input$propertiesDT_rows_selected
  })


  # rendering user filtered dataset
  output$portfolioDT = renderDT({

    #accesing selected rows directly
    newPortfolioRows <- input$propertiesDT_rows_selected

    #testingSave <<- properties[newPortfolioRows,]

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



  # output$testBox = renderUI({
  #   box(title = "Filters for propert selection", style = boxStyle, width = 12  )
  # })

  output$newPropDF = renderUI({
    box(title = "Selected Portfolio", style = boxStyle, width = 12,
        DTOutput('portfolioDT'))
  })

  output$scrapedPropDF = renderUI({
    box(title = "Available Property Dataset", style = boxStyle, width = 12,
        DTOutput("propertiesDT"))
  })

  ## value boxes for user made portfolio
  output$newPropBox1 <- renderValueBox({

    valueBox(
      length(input$propertiesDT_rows_selected),"№ of properties in portfolio", icon = icon("list"),
      color = "purple"
    )
  }
  )

  output$newPropBox2 <- renderValueBox({

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

  output$newPropBox6 <- renderValueBox({
    totalMontlyRent = sum(propertiesReactive()[input$propertiesDT_rows_selected,]$rentPrice) %>%
      currency(digits = 0L, "\U20AC ", big.mark = '.')
    valueBox(
      totalMontlyRent, "Montly rent revenue in EUR", icon = icon("credit-card"), color = 'light-blue'
    )
  }
  )

  # pie charts for user made portfolio showing price and area distribution per district
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

  # boxes for pie charts

  output$newPortfolioStat1 = renderUI({
    box(title = "Capital distribution per District", style = boxStyle, width = 6 ,
        plotlyOutput('newPortfolioChart1'))
  })
  output$newPortfolioStat2 = renderUI({
    box(title = "Area Distribution per District", style = boxStyle, width = 6,
        plotlyOutput('newPortfolioChart2'))
  })



  output$mortgageCalcBox = renderUI({
    box(title = "Mortgage Amortisation Chart", style = boxStyle, width = 8,
        plotlyOutput('distPlot'))
  })

  #### MORTGAGE CALCULATOR
  ## inspired by Antoine Soetewey
  ## taken from link: https://statsandr.com/blog/mortgage-calculator-r-shiny/
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
