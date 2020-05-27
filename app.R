## app.R ##
## Shiny Dashboard App for Housing and Transportation Cost
## last edited jbc 2-20-05-27

library(shinydashboard)
library(flexdashboard)

library(ggplot2)
library(DT)
library(leaflet)
library(rgdal)
library(sp)
library(magrittr)
library(RColorBrewer)


######   GLOBALS   ######

# scenario 1770 test data
list_scenarios <- c("Baseline" = "1770")
aaa <- read.csv("aaa.csv")

#rzone10   ezone10  county10     isMPA     isPDX     is4co    isMult   isClack    isWash isClarkco 
geog_LUT <- read.csv("rzone_geog_LUT.csv")
geog_names <- c(
  "MPA"
  # ,
  # "Multnomah Co",
  # "Clackamas Co.",
  # "Washington Co."
  )


geog_vals <- c(
  "isMPA"
  # ,
  # "isMult",
  # "isClack",
  # "isWash"
)

list_geog <- c(
  "MPA" = "isMPA"
  # ,
  # "Multnomah Co." = "isMult",
  # "Clackamas Co." = "isClack",
  # "Washington Co." = "isWash"
)

list_years <- c(
  "Year 2015" = "1",
  "Year 2040" = "6")

list_valtypes <- c(
  "% of Income" = "poi", 
  "% of Households" = "pcb")

list_tenure <- c(
  "All Households" = "AllHH",
  "Renters Only" = "Renter",
  "Owners Only" = "Owner")

list_ctypes <- c(
  "Housing" = "H",
  "Transportation" = "T",
  "Combined H+T" = "HT")

list_income <- c(
  "Extremely Low" = "inc1",
  "Very Low" = "inc2",
  "Low" = "inc3",
  "Median" = "inc4",
  "Above Median" = "inc5")
  
lyrRzones <- readOGR("shapes/rzones_simple_wgs84.shp",layer = "rzones_simple_wgs84",GDAL1_integer64_policy = TRUE)
lyrUGB <- readOGR("shapes/ugb_line_simple_wgs84.shp",layer = "ugb_line_simple_wgs84",GDAL1_integer64_policy = TRUE)
lyrRivers <- readOGR("shapes/rivers_simple_wgs84.shp",layer = "rivers_simple_wgs84",GDAL1_integer64_policy = TRUE)


######  SIDEBAR  ######   

sidebar <- dashboardSidebar(
  
    # sidebarMenu(id = "mySidebar",
    #             menuItem("About", tabName = "about", icon = icon("crow")),
    #             menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
    #             menuItem("Map", tabName = "map", icon = icon("map"))
    # ),
    
    selectInput("selScenario","Scenario:",list_scenarios,selectize = F, size=1),
    
    selectInput("selYear", "Model Year:",list_years,selectize = F,size = 2),
    selectInput("selGeog","Geography:",list_geog,selectize = F, size=1),
    hr(),
    selectInput("selValTypes", "Show Burden As:",list_valtypes,selectize = F,size = 2),
    selectInput("selTenure", "Show by Tenure:",list_tenure,selectize = F,size = 3),
    hr()
    # ,
     
  
)

  
######  BODY  ######



body <- dashboardBody(

      
    ### CSS STUFF ###
    tags$head(tags$style(HTML('
  
      
   [class*="col-lg-"],[class*="col-md-"],
   [class*="col-sm-"],[class*="col-xs-"]{
    padding-right: 0px !important;
    padding-left: 10px !important;
    }

    .html-widget.gauge svg {
    margin-top: -50px;
    }
    
    /* logo */
    .skin-blue .main-header .logo {
    background-color: #123469;
    }
    
    /* navbar (rest of the header) */
    .skin-blue .main-header .navbar {
     background-color: #123469;
    }

    /* main sidebar */
     .skin-blue .main-sidebar {
    background-color: #194385;
      }

    .nav-tabs{
    font-size: 18px;
    }

     '))),    # tags$head

tabBox(    
       side = "left", width=12,
      ###### about tab  ######  
    
      tabPanel("About",
            h3(strong("About the Housing and Transportation Cost Calculator")),
            hr(),
            h4("The traditional measure of housing affordability recommends that housing cost no more than 30% of household income. However, that benchmark fails to take into account transportation costs, which are typically a household’s second-largest expenditure."), 
            
            HTML("<h4>This calculator offers an expanded view of affordability, one that combines housing and transportation costs, much like the <a href='https://htaindex.cnt.org/'>H+T Index</a> from the Center of Neighborhood Technology (CNT), and the <a href='https://www.hudexchange.info/programs/location-affordability-index/'>Location Affordability Index (LAI) </a> from Housing and Urban Development (HUD).</h4>"),
            
            h4("The H+T and LAI calculators create a current snapshot of affordability using the most recent available data. However, we would also like to be able to forecast affordability, as it is an important measure of equity for the Metro region."),
            
            h4("This cost calulator incorporates forecasts from both the MetroScope land use model and the travel demand models.  Note that we are still refining our calculations to be more consistent with those from the other tools, but our existing data will be sufficient to demonstrate the visualization."),
        fluidRow(
          column(width=4,
            h5(strong("Households are considered 'cost-burdened' when:")),
            h5("-- Housing cost is more than 30% of income"),
            h5("-- Transportation cost is more than 30% of income"),
            h5("-- Combined H+T cost is more than 45% of income")),
          
          column(width=4,
            h5(strong("Income Groups (based on HUD definitions):")),
            h5("Extremely Low = Less than 30% of area median"),
            h5("Very Low = 30% to 50% of area median"),
            h5("Low = 50% to 80% of area median"),
            h5("Median = 80% to 100% of area median"),
            h5("Above Median = Above area median"))
        )   
    
    ),  # about tab
    
    ######  dashboard tab  ######  
 tabPanel("Dashboard",
        
      
    # fluidRow(      
    #   column(h3((textOutput("title_valtype"))), width=3,align="center"),
    #   column(h3((textOutput("title_tenure"))), width=3,align="center"),
    #   column(h3(("Data by Income Group")), width=3,align="center")
    # ),
    # hr(),
    h3(strong("Housing Only")),
            

    fluidRow(
        box(gaugeOutput("gauge1_H"), width = 2, height = 120,  title="Extremely Low"),
        box(gaugeOutput("gauge2_H"), width = 2, height = 120,  title="Very Low"),
        box(gaugeOutput("gauge3_H"), width = 2, height = 120,  title="Low"),
        box(gaugeOutput("gauge4_H"), width = 2, height = 120,  title="Median"),
        box(gaugeOutput("gauge5_H"), width = 2, height = 120,  title="Above Median")
    ),
    h3(strong("Transportation Only")),
  
    fluidRow(
        box(gaugeOutput("gauge1_T"), width = 2, height = 120,  title="Extremely Low"),
        box(gaugeOutput("gauge2_T"), width = 2, height = 120,  title="Very Low"),
        box(gaugeOutput("gauge3_T"), width = 2, height = 120,  title="Low"),
        box(gaugeOutput("gauge4_T"), width = 2, height = 120,  title="Median"),
        box(gaugeOutput("gauge5_T"), width = 2, height = 120,  title="Above Median")
    ),

   h3(strong("Combined H+T")),
  
       fluidRow(
        box(gaugeOutput("gauge1_HT"), width = 2, height = 120,  title="Extremely Low"),
        box(gaugeOutput("gauge2_HT"), width = 2, height = 120,  title="Very Low"),
        box(gaugeOutput("gauge3_HT"), width = 2, height = 120,  title="Low"),
        box(gaugeOutput("gauge4_HT"), width = 2, height = 120,  title="Median"),
        box(gaugeOutput("gauge5_HT"), width = 2, height = 120,  title="Above Median")
    )
   
   
    ), # tabItem dash
   
   ###### map tab  ######  
   
 tabPanel("Map",
           # fluidRow(
           # column(h3((textOutput("title_ctypeM"))), width=3,align="center"),
           # column(h3((textOutput("title_valtypeM"))), width=3,align="center"),
           # column(h3((textOutput("title_incomeM"))), width=3,align="center"),
           # column(h3((textOutput("title_tenureM"))), width=3,align="center")
           # ),
        fluidRow(
          column(width=3,
            selectInput("selCostTypes", "Map Costs For:",list_ctypes,selectize = F,size = 3),
            selectInput("selIncome", "Map by Income:",list_income,selectize = F,size = 5)
            # 
            # HTML("<h1>&nbsp;</h1>"),
            # HTML("<h1>&nbsp;</h1>"),
            # HTML("<h1>&nbsp;</h1>"),
            # HTML("<h1>&nbsp;</h1>"),
            # HTML("<h1>&nbsp;</h1>")
            
          ),
          column(leafletOutput("mymap", height=600),width=9)
        )
        
          
      # ,
      # DTOutput("table")
 ) #map panel
) #tabs
  
) # body

myheader <- dashboardHeader(title = "Housing and Transportation Cost Calculator", titleWidth = 450)

#####  UI  ######
ui <- dashboardPage(
    myheader,
    sidebar,
    body
)

######  SERVER  ######
server <- function(input, output) {
       
    ### return weighted averages by income group ###
    
    getAverages <- function(myType){
    data_in <- aaa
    
    idx <- (data_in$year == input$selYear) &
      (data_in$tabletype == paste0(myType,input$selValTypes)) &
      (data_in$tenure == input$selTenure) &
      (geog_LUT[,input$selGeog] == 1)
     
    idx_wgt <- (data_in$year == input$selYear) &
      (data_in$tabletype == "demandHH") &
      (data_in$tenure == input$selTenure) &
      (geog_LUT[,input$selGeog] == 1)
    
    
    data_out <- data_in[idx,2:6]
    data_wgt_out <- data_in[idx_wgt,2:6]
    
    vals_by_income <- colSums(data_out * data_wgt_out)/colSums(data_wgt_out)
    vals_by_income[vals_by_income > 100] <- 100
    return(vals_by_income)
    
    }

    
  
  ######  TEXT LABELS  ######
    output$title_valdesc <- renderText({
      
      if (input$selValTypes == "poi"){
        myTitle1 <- "How much is the burden?"
      }
      if (input$selValTypes == "pcb"){
        myTitle1 <- "How many are burdened?"
      }
      return(myTitle1)
    })
    
  
    output$title_valtype <- renderText({
        
        if (input$selValTypes == "poi"){
          myTitle1 <- "Avg. Household Cost, as % of Income"
        }
        if (input$selValTypes == "pcb"){
          myTitle1 <- "Avg. % of Housesholds Burdened"
        }
        return(myTitle1)
    })
  
    output$title_valtypeM <- renderText({
      
      if (input$selValTypes == "poi"){
        myTitle1 <- "Avg. Household Cost, as % of Income"
      }
      if (input$selValTypes == "pcb"){
        myTitle1 <- "Avg. % of Housesholds Burdened"
      }
      return(myTitle1)
    })
    
    output$title_tenure <- renderText({
    
        if (input$selTenure == "Owner"){
           myTitle2 <- "Owners Only"
        }
        if (input$selTenure == "Renter"){
           myTitle2 <- "Renters Only"
        }
        if (input$selTenure == "AllHH"){
           myTitle2 <- "All Households"
        }
            
        return(myTitle2)
    })
    output$title_tenureM <- renderText({
      
      if (input$selTenure == "Owner"){
        myTitle2 <- "Owners Only"
      }
      if (input$selTenure == "Renter"){
        myTitle2 <- "Renters Only"
      }
      if (input$selTenure == "AllHH"){
        myTitle2 <- "All Households"
      }
      
      return(myTitle2)
    })
    
    
    output$title_ctypeM <- renderText({
      if (input$selCostTypes == "H"){
        myTitle2 <- "Housing"
      }
      if (input$selCostTypes == "T"){
        myTitle2 <- "Transportation"
      }
      if (input$selCostTypes == "HT"){
        myTitle2 <- "Combined H+T"
      }
      
      return(myTitle2)
    })
    
    output$title_incomeM <- renderText({
      if (input$selIncome == "inc1"){
        myTitle2 <- "Extremely Low Income"
      }
      if (input$selIncome == "inc2"){
        myTitle2 <- "Very Low Income"
      }      
      if (input$selIncome == "inc3"){
        myTitle2 <- "Low Income"
      }      
      if (input$selIncome == "inc4"){
        myTitle2 <- "Median Income"
      }      
      if (input$selIncome == "inc5"){
        myTitle2 <- "Above Median Income"
      }      
      return(myTitle2)
    })
    
    # 
    # output$table <- renderDT(DT::datatable(
    #   options = list(lengthMenu = c(5, 500),dom = 'itlp'),
    #   data =  getMapData()
    #) # datatable()
    #) #renderDataTable()
    
    ######  TEXT GAUGES  ######

    ######  H Gauges  ######
  
    output$gauge1_H <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("H"))[1],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge2_H <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("H"))[2],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge3_H <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("H"))[3],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge4_H <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("H"))[4],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge5_H <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("H"))[5],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
  
    ######  T Gauges  ######
    
    output$gauge1_T <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("T"))[1],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge2_T <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("T"))[2],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge3_T <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("T"))[3],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge4_T <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("T"))[4],0), 
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
    
    output$gauge5_T <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("T"))[5],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(29, 0), warning = c(74,30), danger = c(100, 75)
          ))
    })
  
    ######  HT Gauges  ######

    output$gauge1_HT <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("HT"))[1],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(44, 0), warning = c(74,45), danger = c(100, 75)
          ))
    })
    
    output$gauge2_HT <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("HT"))[2],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(44, 0), warning = c(74,45), danger = c(100, 75)
          ))
    })
    
    output$gauge3_HT <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("HT"))[3],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(44, 0), warning = c(74,45), danger = c(100, 75)
          ))
    })
    
    output$gauge4_HT <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("HT"))[4],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(44, 0), warning = c(74,45), danger = c(100, 75)
          ))
    })
    
    output$gauge5_HT <- flexdashboard::renderGauge({
    gauge(round( as.vector(getAverages("HT"))[5],0),  
          min = 0, max = 100, symbol = '%',
          gaugeSectors(success = c(44, 0), warning = c(74,45), danger = c(100, 75)
          ))
    })
    
    ##########################
    ######  MAP SPECFIC ######
    ##########################
    ### return complete rzone vectors by income group ###
    

    
    
    getMapData <- function(){
      data_in <- aaa
      
      idx <- (data_in$year == input$selYear) &
        (data_in$tabletype == paste0(input$selCostTypes,input$selValTypes)) &
        (data_in$tenure == input$selTenure) 
      
      data_out <- data_in[idx,2:6]
      data_out[data_out > 100] <- 100
      data_out$rzone <- data_in[idx,1]
      
      idx2 <- (data_in$year == input$selYear) &
        (data_in$tabletype == "demandHH") &
        (data_in$tenure == input$selTenure) 
      
      data_out$hh <- rowSums(data_in[idx2,2:6])
   
      
      return(data_out)
    }
    
    getLabels <- function(){
      myData <- getMapData()
      

        myLabel <- sprintf(
          "<strong>Rzone %.0f</strong>
          <br>Total HH: %.0f
          <br/>XL: &nbsp;&nbsp;&nbsp;%.0f%%
          <br/>VL: &nbsp;&nbsp;&nbsp;%.0f%%
          <br/>L: &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%.0f%%
          <br/>M: &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%.0f%%
          <br/>AM: &nbsp;&nbsp;&nbsp;%.0f%%",
          myData$rzone,
          myData$hh,
          myData$inc1,
          myData$inc2,
          myData$inc3,
          myData$inc4,
          myData$inc5
        ) %>% lapply(htmltools::HTML)


      
      return(myLabel)
    }
    
    output$mymap <- renderLeaflet({

      Xmybins <- c(0,10,20,30,40,50,60,70,80,90,100)
      Xtitle <- "%"
      Xcolors <- "YlOrRd"
      XmyLabels <- getLabels()
      XisReversed <- F
      Xboxes <- 10

      pallet <- brewer.pal(name=Xcolors,n=Xboxes)

      Xpallet <- colorBin( palette=pallet, domain=getMapData()[,input$selIncome],
                           na.color="White", bins=Xmybins, reverse = XisReversed)


      data_input <- merge(lyrRzones, getMapData(), by.x = "rzone10", by.y = "rzone")
      data_input@data$map <- data_input@data[,input$selIncome]

      leaflet(data_input, options = leafletOptions(minZoom = 8, maxZoom = 13, )) %>%
        setView(-122.7,45.48, zoom = 10) %>%

        addPolygons(color = "#444444",
                    weight = 0.2,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    fillColor = ~Xpallet(map),
                    label = XmyLabels,
                    highlightOptions = highlightOptions(color = "cyan", weight = 2, bringToFront = TRUE)) %>%

        addPolygons(data= lyrRivers, color = "lightblue",
                    weight = 0.5,
                    opacity = 0.5,
                    fillOpacity = 0.4,
                    fillColor = "lightblue") %>%

        addPolylines(data= lyrUGB, color = "black", weight = 1.5) %>%

        addLegend("topright",
                  pal = Xpallet,
                  values = ~map,
                  title = Xtitle,
                  labFormat = labelFormat(
                    prefix = "", suffix = "", between = " to ", big.mark = ","
                  ),
                  opacity = 1)

    }) # myMap

    
    
    
    
}  # server



shinyApp(ui, server)