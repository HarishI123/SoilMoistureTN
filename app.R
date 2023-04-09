#Loading required Packages
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(ggplot2)
library(fontawesome)
library(shinycssloaders)
library(maps)
library(leaflet)
library(leaflet.extras)
library(rvest)
library(plotly)
#===================================================================================================================================
#Data importing
df<-read.csv("data/location.csv")
c1 = df %>% select("District")
#============================================================================================================================
#ui for shiny App
ui<-dashboardPage(
  #defines header
  skin = "green",
  dashboardHeader(
    title="The Soil Moisture Analysis - TamilNadu" ,
    titleWidth = 500,
    dropdownMenu()
  ),
  #defines sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Moisture per year", tabName = "dashboard", icon = icon("dashboard")),
      sidebarMenuOutput("menu"),
      menuItem("Map",tabName="unions",icon=icon("signal")),
      menuItem("Trend Analysis",tabName="world",icon=icon("globe"))
    )
  ),
  #defines body
  dashboardBody(
    tags$head(tags$style(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(href="https://fonts.googleapis.com/css2?family=Roboto+Condensed&display=swap")
    )),
    
    tabItems(
      #First TAB Menu-Dashboard
      tabItem(tabName = "dashboard",

              fluidRow(
                tags$h3(style = "font-family: 'Roboto Condensed', sans-serif;font-weight:bold","Average Soil Moisture over Year",align = "center"),
                plotOutput("container"),
                withSpinner(plotlyOutput("bar")

              ),
      ),),
      #second tab menu- ABOUT
      tabItem(tabName="about",
              div(fluidRow(
                h2(style = "font-family: 'Roboto Condensed', sans-serif;font-weight:bold;text-align:center","Soil Moisture in TamilNadu"),
                br(),
                p(style="font-family:'Roboto Condensed',sans-serif;font-size:20px;text-align:center",strong("Soil Moisture"),"refers to the amount of water that is present in the soil. It is a critical factor in determining the health and growth of plants, as well as the stability of soil structure. Soil moisture is influenced by several factors, including rainfall, temperature, humidity, soil type, and vegetation cover."),
                p(style="font-family:'Roboto Condensed',sans-serif;font-size:20px;text-align:center", "Soil moisture is a crucial factor in determining the health and growth of plants, as well as the stability of soil structure. Adequate soil moisture helps to bind soil particles together, preventing erosion and other forms of soil degradation, and plays a crucial role in maintaining soil structure and stability. In addition, soil moisture affects the availability of nutrients for plants, which is essential for better growth and yields. Soil moisture also plays a role in regulating the climate by absorbing and storing more heat in moist soil, which can help to moderate temperatures in hot, dry environments. It is a critical component of the water cycle, helping to regulate water flow and prevent flooding. Therefore, monitoring soil moisture levels is essential for maintaining healthy plants and soil ecosystems, preventing erosion and soil degradation, regulating the climate, and ensuring the availability of nutrients for plant growth."),
                div(style="display:flex;justify-content:center;align-items:center;",tags$img(style="border:5px solid green;",src="https://images.pexels.com/photos/1108572/pexels-photo-1108572.jpeg", width =400 , height = 300)
                ),br(),
                p(style="font-family:'Roboto Condensed',sans-serif;font-size:20px;text-align:center",strong("Average Measurements:"),"The average percentage of soil moisture can vary widely depending on soil type, climate, and other environmental factors. However, in general, soil moisture is typically expressed as a percentage of the weight of water in the soil compared to the total weight of the soil. The range of soil moisture content can vary from as low as 1-2% in extremely dry soils to as high as 50-60% in saturated soils. However, for most soils, the optimal range of soil moisture for plant growth is typically between 25-50% of the soil's total weight. It's important to note that the ideal soil moisture level can vary depending on the type of plant being grown, so it's important to monitor soil moisture levels closely and adjust watering practices accordingly.")
              ),
              div(style="display:flex;justify-content:center;align-items:center;",tags$img(style="border:5px solid green;",src="https://www.victoriatexasweather.com/soilchart.gif", width =500 , height = 300)
              ),
              div(tags$a(href="https://indiawris.gov.in/wris/#/about","For more Information about data"))
                
                
                ), 
        ),
      tabItem(tabName = "unions",
              h2(style="font-family:'Roboto Condensed',sans-serif;font-weigth:bold;text-align:center","Technology used for Soil Monitoring"),
              p(style="font-family:'Roboto Condensed',sans-serif;font-size:20px;text-align:center","A technology called Soilsens -- a low cost smart soil monitoring system has come as a potential help to farmers facing farming decision predicaments.  Soilsens product line is developed by Proximal Soilsens Technologies Pvt. Ltd, a startup incubated at Indian Institute of Technology Bombay (IITB), Mumbai with support from the Ministry of Department of Science and Technology (DST) and Ministry of Electronics and Information Technology (Meity). Proximal Soilsens started with a mission to build affordable technologies for precision farming. The idea is to “create wealth through sustainability”. The system is embedded with soil moisture sensor, soil temperature sensor, ambient humidity sensor, and ambient temperature sensor. Based on these parameters, farmers are advised about optimum irrigation through a mobile app. This data is also available on cloud. There is also a portable soil moisture system."),
              h3(style = "font-family: 'Roboto Condensed', sans-serif;font-weight:bold","Geographical Location with Soil Moisture",align="center") ,
              leafletOutput("Map") %>% withSpinner(color = "green")
      ),
      tabItem(tabName = "world",
              h3(style = "font-family: 'Roboto Condensed', sans-serif;font-weight:bold","Trends and analysis of soil in each District",align="center"),
              div(style ="display:flex;justify-content:center;align-items:center",fluidRow(
                infoBoxOutput('TamilNadu',width=12))
                
              ),
              fluidRow(
                h3(style = "font-family: 'Roboto Condensed', sans-serif;font-weight:bold","Volumeteric soil moisture content (%) till 15cm depth from 2018 to 2023 using NRSC VIC data model",align = "center"),
                column(12,
                       
                       box(selectInput("District",label="Select Region",choices=df$District,selected  = ''),width = 12) 
                       
                ),
                column(12,box(highchartOutput("line"),width = "12"))
              )
      )
    )#end tabitems
  )#end body
)#end dashboard
#===================================================================================================================================================
#Server for Shiny App
server <- function(input, output) {
  
  output$TamilNadu<-renderInfoBox({
    infoBox(title = p(strong("Tamil Nadu"),style="font-size: 20px"),value=18.70,subtitle ="Average Volumeteric Soil moisture (%)", icon = icon("tint"),color = 'teal')
  })
  
  output$container <- renderPlot({
    # Plot the selected variables
    ggplot(df,aes(x = Date,y = SoilMoisture)) +
      geom_line() +
      labs(x = "Year",y = "Soil Moisture",title = "Average Soil Moisture per Year")
  })
  output$bar <- renderPlotly({
   df %>% 
      plot_ly() %>% 
      add_bars(x=~District, y=~SoilMoisture) %>% 
      layout(title = paste("Districtwise Volumetric Soil Moisture"),colorway = "green",
             xaxis = list(title = "District Name"),
             yaxis = list(title = "Soil Moisture Percentage"))
  })
  output$line <- renderHighchart({
    data<-df %>% filter(District==input$District)
    hchart(data, "line",color="green",hcaes(x=Date,y=SoilMoisture))  %>%
      
      hc_exporting(enabled = TRUE) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 2) %>%
      hc_title(text="Time series plot Moisture Rate",align="center") %>%
      hc_subtitle(text="Data Source: WRIS",align="center") %>%
      hc_add_theme(hc_theme_elementary())
  })
 
  
  
  output$Map <- renderLeaflet({
    leaflet(data = df) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(
        ~Longitude,
        ~Latitude,
        icon=icon("location-dot"),
        label=paste("Soil Moisture:",df$SoilMoisture,"District:",df$Date),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery","Stamen Watercolor"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}
shinyApp(ui=ui,server=server)