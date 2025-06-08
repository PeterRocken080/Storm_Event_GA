library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(DT)
library(sf)
library(webshot2)

# Load data
data <- read_csv("storm_data_search_results update_Final.csv") %>%
  mutate(
    BEGIN_LAT = as.numeric(BEGIN_LAT),
    BEGIN_LON = as.numeric(BEGIN_LON),
    BEGIN_DATE = mdy(BEGIN_DATE),
    YEAR = year(BEGIN_DATE)
  ) %>%
  filter(!is.na(BEGIN_LAT) & !is.na(BEGIN_LON))

# Load shapefile
georgia_boundary <- st_read("GA_shapefile.shp") %>%
  st_transform(crs = 4326)

# Tornado count per county
county_summary <- data %>%
  group_by(NAME_2) %>%
  summarise(Total_Tornadoes = n())

georgia_boundary <- georgia_boundary %>%
  left_join(county_summary, by = "NAME_2")

georgia_boundary$Total_Tornadoes[is.na(georgia_boundary$Total_Tornadoes)] <- 0

# ==== UI ====
ui <- fluidPage(
  titlePanel("🌪️ Tornado History in Georgia"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Tornado Events"),
      selectInput("scale", "EF/F Scale", choices = c("All", unique(data$TOR_F_SCALE))),
      selectInput("county", "County", choices = c("All", sort(unique(data$NAME_2)))),
      dateRangeInput("daterange", "Date Range",
                     start = min(data$BEGIN_DATE, na.rm = TRUE),
                     end = max(data$BEGIN_DATE, na.rm = TRUE)),
      checkboxInput("showHeatmap", "Show Heatmap Layer", FALSE),
      checkboxInput("showBoundary", "Show Georgia Boundary", TRUE),
      checkboxInput("showChoropleth", "Show County Tornado Frequency", FALSE),
      actionButton("updateMap", "Update Map", icon = icon("sync")),
      downloadButton("downloadData", "Download Filtered Data"),
      downloadButton("saveMap", "Export Map Image")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Map", leafletOutput("tornadoMap", height = "650px")),
        tabPanel("EF Scale Chart", plotOutput("efChart")),
        tabPanel("Trend Over Time", plotOutput("trendPlot")),
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Project Overview",
                 tags$div(
                   style = "margin-top:20px; font-size:14px; color:#555;",
                   icon("info-circle"),
                   strong(" About This Project: "),
                   "This app visualizes tornado activity across Georgia. Use filters, map layers, and charts to explore storms by scale, year, and location."
                 )
        )
      )
    )
  ),
  
  tags$div("© Peter Acquah", style = "text-align:center; padding:10px; font-size:13px; color:gray; margin-top:20px;")
)

# ==== SERVER ====
server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- data
    if (input$scale != "All") df <- df %>% filter(TOR_F_SCALE == input$scale)
    if (input$county != "All") df <- df %>% filter(NAME_2 == input$county)
    df %>% filter(BEGIN_DATE >= input$daterange[1], BEGIN_DATE <= input$daterange[2])
  })
  
  output$tornadoMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -83.5, lat = 32.5, zoom = 6)
  })
  
  # 🧠 MAP DRAW FUNCTION
  draw_map <- function(df) {
    tornado_counts <- georgia_boundary$Total_Tornadoes
    tornado_counts <- tornado_counts[!is.na(tornado_counts) & tornado_counts > 0]
    pal <- colorNumeric("YlOrRd", domain = tornado_counts)
    
    map <- leafletProxy("tornadoMap", data = df) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearHeatmap() %>%
      clearControls()
    
    if (input$showChoropleth && length(tornado_counts) > 0) {
      map <- map %>%
        addPolygons(data = georgia_boundary,
                    fillColor = ~pal(Total_Tornadoes),
                    weight = 1,
                    color = "#333",
                    fillOpacity = 0.6,
                    label = ~paste0(NAME_2, ": ", Total_Tornadoes, " tornadoes"),
                    highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)) %>%
        addLegend("bottomright", pal = pal, values = tornado_counts,
                  title = "Tornado Frequency<br/>by County", opacity = 0.8)
    }
    
    if (!input$showChoropleth && input$showBoundary) {
      map <- map %>%
        addPolygons(data = georgia_boundary, color = "black", weight = 1.5, fill = FALSE)
    }
    
    if (nrow(df) > 0) {
      map <- map %>%
        addCircleMarkers(
          lng = ~BEGIN_LON, lat = ~BEGIN_LAT,
          radius = 5,
          color = "red",
          stroke = FALSE, fillOpacity = 0.8,
          label = ~paste0("EF Scale: ", TOR_F_SCALE, " | ", BEGIN_LOCATION),
          popup = ~paste0(
            "<b>Event ID:</b> ", EVENT_ID, "<br/>",
            "<b>Date:</b> ", BEGIN_DATE, "<br/>",
            "<b>Location:</b> ", BEGIN_LOCATION, " (", NAME_2, ")<br/>",
            "<b>EF Scale:</b> ", TOR_F_SCALE, "<br/>",
            "<b>Magnitude:</b> ", MAGNITUDE, "<br/>",
            "<b>Injuries:</b> ", INJURIES_DIRECT, "<br/>",
            "<b>Damage ($):</b> ", DAMAGE_PROPERTY_NUM
          ),
          clusterOptions = markerClusterOptions()
        )
    }
    
    if (input$showChoropleth && length(tornado_counts) > 0) {
      map <- map %>%
        addLegend("bottomright", pal = pal, values = tornado_counts,
                  title = "Tornado Frequency<br/>by County", opacity = 0.8)
    } else {
      map <- map %>%
        addLegend("bottomright", colors = "red", labels = "Tornado Event", title = "Map Legend")
    }
    
    if (!input$showChoropleth) {
      map <- map %>%
        addLegend("bottomright", colors = "red", labels = "Tornado Event", title = "Map Legend")
    }
    
  }
  
  # 🔁 Auto-draw on launch
  observe({
    df <- filtered_data()
    draw_map(df)
  })
  
  # 🔁 Redraw on button click
  observeEvent(input$updateMap, {
    df <- filtered_data()
    draw_map(df)
  })
  
  output$trendPlot <- renderPlot({
    filtered_data() %>%
      group_by(YEAR) %>%
      summarise(Total = n()) %>%
      ggplot(aes(x = YEAR, y = Total)) +
      geom_line(color = "steelblue") +
      geom_point() +
      labs(title = "Tornadoes per Year", x = "Year", y = "Number of Events")
  })
  
  output$efChart <- renderPlot({
    filtered_data() %>%
      group_by(TOR_F_SCALE) %>%
      summarise(Count = n()) %>%
      ggplot(aes(x = TOR_F_SCALE, y = Count)) +
      geom_bar(stat = "identity", fill = "tomato") +
      labs(title = "Tornado Count by EF Scale", x = "EF Scale", y = "Count")
  })
  
  output$summaryTable <- renderDT({
    filtered_data() %>%
      select(EVENT_ID, BEGIN_DATE, NAME_2, BEGIN_LOCATION, TOR_F_SCALE, INJURIES_DIRECT, DAMAGE_PROPERTY_NUM) %>%
      arrange(desc(BEGIN_DATE)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("tornado_data_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$saveMap <- downloadHandler(
    filename = function() {
      paste0("tornado_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      mapshot(leafletProxy("tornadoMap"), file = file)
    }
  )
}

shinyApp(ui, server)
