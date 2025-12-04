library(shiny)
library(bslib) 
library(dplyr)
library(ggplot2) 
library(DT) 
library(leaflet) 
library(sf) 
library(rnaturalearth) 
library(rnaturalearthdata) 
library(rlang) 
library(purrr)
library(tidyr)
library(tibble) 


kpi_box <- function(title, value_html, subtitle, color = "primary", icon_name = "bar-chart-fill", custom_style = "") {
  bg_class <- switch(color,
                     "primary" = "bg-primary text-white",
                     "info" = "bg-info text-white",
                     "warning" = "bg-warning text-dark",
                     "success" = "bg-success text-white",
                     "danger" = "bg-danger text-white",
                     "secondary" = "bg-secondary text-white",
                     "light" = "bg-light text-dark",
                     "bg-primary text-white")
  
  icon_html <- tags$i(
    class = paste0("bi bi-", icon_name), 
    style = paste0("font-size: 3rem; margin-right: 1rem;", 
                   if (grepl("text-white", bg_class)) "opacity: 0.6;" else "opacity: 0.4; color: #000;")
  )
  
  tags$div(
    class = paste("card shadow", bg_class),
    style = paste("height: 100%; overflow: hidden; display: flex; align-items: center; padding: 1rem; margin-bottom: 0;", 
                  custom_style), 

    tags$div(
      class = "d-flex flex-column align-items-start",
      style = "flex-grow: 1;",
  
      value_html,
      
      p(class = "text-uppercase", 
        style = "font-weight: 600; font-size: 0.75rem; margin: 0; line-height: 1;", 
        title
      ), 
      p(style = "font-size: 0.7rem; margin: 0; opacity: 0.8;", subtitle)
    ))}

h2_value <- function(val) {
  h2(style = "font-weight: 700; margin: 0; font-size: 2rem; line-height: 1.1;", val)
}

import_wdi <- "world_bank_development_indicators.csv" 
if (file.exists(import_wdi)) {
  wdi_data <- read.csv(import_wdi) %>%
    mutate( 
      Date_Parsed = as.Date(as.character(date), format = "%m/%d/%Y"), 
      Year = suppressWarnings(as.integer(format(Date_Parsed, "%Y"))) 
    ) %>% 
    rename(Country = country) %>% 
    select(-date, -Date_Parsed) %>% 
    filter(!is.na(Year)) 
} else {
  message("WDI file not found. Using dummy data.")
  wdi_data <- data.frame( 
    Country = c("A", "B", "A", "B"), 
    Year = c(2010, 2010, 2011, 2011), 
    population = c(1e6, 2e6, 1.1e6, 2.1e6), 
    GDP_current_US = c(1e10, 2e10, 1.1e10, 2.1e10), 
    life_expectancy_at_birth = c(70, 80, 70.5, 80.5), 
    population_density = c(10, 5, 11, 6), 
    agricultural_land = c(50, 20, 51, 21), 
    forest_land = c(10, 70, 10.5, 70.5), 
    land_area = c(100000, 400000, 100000, 400000), 
    birth_rate = c(15, 10, 15.2, 10.1), 
    death_rate = c(7, 8, 7.1, 8.1), 
    rural_population = c(200000, 400000, 210000, 410000) 
  )} 

wdi_vars <- names(wdi_data) %>% setdiff(c("Country", "Year")) 

wdi_years <- sort(unique(wdi_data$Year)) 
wdi_min_year <- if(length(wdi_years) > 0) min(wdi_years) 
wdi_max_year <- if(length(wdi_years) > 0) max(wdi_years)

world <- ne_countries(scale = "medium", returnclass = "sf") 

import_airbnb_2023 <- "AB_US_2023.csv" 
import_airbnb_2020 <- "AB_US_2020.csv" 

airbnb_cols <- c( 
  "city", "neighbourhood", "room_type", "price", 
  "latitude", "longitude", "number_of_reviews", "calculated_host_listings_count", 
  "reviews_per_month" 
) 

load_airbnb <- function(file_path, year) { 
  if (file.exists(file_path)) { 
    data <- read.csv(file_path) %>% 
      select(all_of(airbnb_cols)) %>% 
      filter(price > 0 & price < 1000) %>% 
      mutate(Year = as.factor(year)) %>% 
      rename(City = city) 
  } else { 
    message(paste("File not found:", file_path, "Using dummy data for year", year)) 
    data <- data.frame( 
      City = c("New York", "Boston"), neighbourhood = c("Hells Kitchen", "Back Bay"), 
      room_type = c("Entire home/apt", "Private room"), price = c(150, 75), 
      latitude = c(40.75, 42.35), longitude = c(-73.99, -71.08), 
      number_of_reviews = c(50, 20), calculated_host_listings_count = c(5, 1), 
      reviews_per_month = c(2.5, 1.0), 
      Year = as.factor(year) 
    ) 
  } 
  return(data)
} 

airbnb_2023 <- load_airbnb(import_airbnb_2023, 2023) 
airbnb_2020 <- load_airbnb(import_airbnb_2020, 2020) 

airbnb_data <- bind_rows(airbnb_2020, airbnb_2023) 

airbnb_years <- sort(unique(airbnb_data$Year)) 
airbnb_cities <- sort(unique(airbnb_data$City)) 
airbnb_roomtypes <- sort(unique(airbnb_data$room_type)) 


ui <- navbarPage( 
  title = "Project 3: Global Development and Airbnb Analysis", 
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel( 
    "Overview", 
    fluidPage( 
      h2("Data Visualization Project with R Shiny"), 
      p("This dashboard explores two real-world datasets: World Development Indicators (WDI) and US Airbnb Listings (2020 vs 2023)."), 
      
      wellPanel( 
        h3("Dashboard Structure and Data Sources"),
        p("The application is divided into two primary analysis sections:"),
        tags$ul( 
          tags$li( 
            strong("World Development Indicators (WDI):"), 
            " Use interactive controls to filter, visualize trends, and map WDI indicators (like GDP, Population, Life Expectancy) across different countries and years. Features a time-series plot, a geographic map, and a data table."
          ),
          tags$li( 
            strong("Airbnb Listings (2020 vs 2023):"),
            " Compare key metrics, price distributions, and host activity for Airbnb listings in major US cities, highlighting changes between 2020 and 2023. Features year-over-year KPIs, a box plot for price analysis, and an interactive map of listings."
          )
        ),
        p(strong("Note:"), " This app is pre-configured for WDI ('world_bank_development_indicators.csv'), 'AB_US_2020.csv', and 'AB_US_2023.csv'. Please ensure these files are available in the app directory for the full functionality.")
      ) 
    ) 
  ), 
  
  tabPanel( 
    "Global Development (WDI)",
    sidebarLayout(
      sidebarPanel(
        h3("WDI Explorer Controls"), 
        width = 3, 
        selectizeInput(inputId = "wdi_countries", label = "Select Countries:",
                       choices = sort(unique(wdi_data$Country)),
                       selected = c("United States", "India", "China"),
                       multiple = TRUE),
        sliderInput(inputId = "wdi_years", label = "Select Year Range:",
                    min = wdi_min_year, max = wdi_max_year,
                    value = c(wdi_min_year, wdi_max_year), sep = "", step = 1),
        selectInput("wdi_indicator", "Select Indicator:", choices = wdi_vars,
                    selected = "GDP_current_US"),
        checkboxInput("wdi_logy", "Log scale (y-axis)", value = FALSE),
        selectizeInput(inputId = "wdi_table_cols", label = "Select Columns for Table/Download:",
                       choices = wdi_vars,
                       selected = c("GDP_current_US", "life_expectancy_at_birth", "population", "Year"),
                       multiple = TRUE)
      ), 
      mainPanel(
        tabsetPanel(
          tabPanel("Summary KPIs", h4("Key Global Development Indicators"), uiOutput("wdi_kpis")),
          tabPanel("Time-Series Explorer", h4("Indicator Trend Over Time"), plotlyOutput("wdi_time_series")),
          tabPanel("Map View", h4("Geographic View by Year"),
                   numericInput("wdi_map_yr", "Select Year for Map:",
                                value = wdi_max_year, min = wdi_min_year, max = wdi_max_year),
                   leafletOutput("wdi_map")),
          tabPanel("Data Table", h4("Filtered WDI Data"),
                   dataTableOutput("wdi_table"),
                   downloadButton("download_wdi_data", "Download Filtered Data"))
        ), 
        width = 9 
      ))), 
  
  tabPanel(
    "Airbnb Listings (2020 vs 2023)",
    sidebarLayout(
      sidebarPanel(
        h3("Airbnb Analysis Controls"),
        width = 3, 
        selectizeInput(inputId = "airbnb_years_comp", label = "Select Years for Comparison:",
                       choices = airbnb_years,
                       selected = airbnb_years,
                       multiple = TRUE),
        
        selectizeInput(inputId = "airbnb_cities_input", label = "Select Cities:",
                       choices = airbnb_cities,
                       selected = head(airbnb_cities, 3),
                       multiple = TRUE),
        
        selectInput("airbnb_roomtype", "Select Room Type:",
                    choices = c("All", airbnb_roomtypes), selected = "All"),
        
        sliderInput("airbnb_price_range", "Price Range ($):",
                    min = min(airbnb_data$price), max = max(airbnb_data$price),
                    value = c(50, 300))
      ), 
      mainPanel(
        tabsetPanel(
          tabPanel("Summary KPIs", h4("Key Performance Indicators"), uiOutput("airbnb_kpis")),
          tabPanel("City Analysis", h4("Price Distribution: Year-over-Year Comparison by City"), plotOutput("airbnb_box_plot")),
          tabPanel("Interactive Map", h4("Listing Locations"), leafletOutput("airbnb_map")),
          tabPanel("Host Insights", h4("Host Activity vs Reviews"),
                   plotOutput("airbnb_host_plot"),) 
        ), 
        width = 9 
      ))))




server <- function(input, output, session) {
  wdi_reactive <- reactive({ wdi_data })
  airbnb_reactive <- reactive({ airbnb_data })
  
  wdi_filtered <- reactive({
    df <- wdi_reactive()
    req(input$wdi_countries, input$wdi_years)
    df %>% 
      filter(Country %in% input$wdi_countries,
             Year >= input$wdi_years[1], Year <= input$wdi_years[2])
  }) 
  
  output$wdi_kpis <- renderUI({
    df <- wdi_filtered()
    req(nrow(df) > 0)
    
    color_first_year <- "success"
    color_overall_avg <- "primary"
    color_last_year <- "info"
    
    first_year <- input$wdi_years[1]
    last_year <- input$wdi_years[2]
    
    df_first <- df %>% filter(Year == first_year)
    le_first <- round(mean(df_first$life_expectancy_at_birth, na.rm = TRUE), 1)
    gdp_first <- mean(df_first$GDP_current_US, na.rm = TRUE)
    gdp_first_formatted <- format(round(gdp_first / 1e9, 2), big.mark = ",")
    pop_first <- mean(df_first$population, na.rm = TRUE)
    pop_first_formatted <- format(round(pop_first / 1e6, 2), big.mark = ",")
    
    avg_le <- round(mean(df$life_expectancy_at_birth, na.rm = TRUE), 1)
    avg_gdp <- mean(df$GDP_current_US, na.rm = TRUE)
    avg_gdp_formatted <- format(round(avg_gdp / 1e9, 2), big.mark = ",")
    avg_pop <- mean(df$population, na.rm = TRUE)
    avg_pop_formatted <- format(round(avg_pop / 1e6, 2), big.mark = ",")
    
    df_last <- df %>% filter(Year == last_year)
    le_last <- round(mean(df_last$life_expectancy_at_birth, na.rm = TRUE), 1)
    gdp_last <- mean(df_last$GDP_current_US, na.rm = TRUE)
    gdp_last_formatted <- format(round(gdp_last / 1e9, 2), big.mark = ",")
    pop_last <- mean(df_last$population, na.rm = TRUE)
    pop_last_formatted <- format(round(pop_last / 1e6, 2), big.mark = ",")
    
    
    tagList(
      h3(paste0("Metrics for Initial Year (", first_year, ")")),
      fluidRow(
        column(4, kpi_box(
          "Life Expectancy",
          h2_value(le_first %||% "N/A"),
          paste("Average in", first_year, "for selected countries"),
          color = color_first_year,
        )), 
        column(4, kpi_box(
          "GDP",
          h2_value(paste0("$", gdp_first_formatted %||% "N/A", "B")),
          paste("Average in", first_year, "for selected countries"),
          color = color_first_year,
        )), 
        column(4, kpi_box(
          "Population",
          h2_value(paste0(pop_first_formatted %||% "N/A", "M")),
          paste("Average in", first_year, "for selected countries"),
          color = color_first_year,
        ))
      ), 
      
      h3("Overall Averages Across Selected Range"),
      fluidRow(
        column(4, kpi_box(
          "Life Expectancy",
          h2_value(avg_le %||% "N/A"),
          "Years (Average across all selected countries/years)",
          color = color_overall_avg,
        )), 
        column(4, kpi_box(
          "GDP",
          h2_value(paste0("$", avg_gdp_formatted %||% "N/A", "B")),
          "Billions USD (Average across all selected countries/years)",
          color = color_overall_avg,
        )), 
        column(4, kpi_box(
          "Population",
          h2_value(paste0(avg_pop_formatted %||% "N/A", "M")),
          "Millions (Average across all selected countries/years)",
          color = color_overall_avg,
        ))
      ), 
      
      h3(paste0("Metrics for Final Year (", last_year, ")")),
      fluidRow(
        column(4, kpi_box(
          "Life Expectancy",
          h2_value(le_last %||% "N/A"),
          paste("Average in", last_year, "for selected countries"),
          color = color_last_year,
        )), 
        column(4, kpi_box(
          "GDP",
          h2_value(paste0("$", gdp_last_formatted %||% "N/A", "B")),
          paste("Average in", last_year, "for selected countries"),
          color = color_last_year,
        )), 
        column(4, kpi_box(
          "Population",
          h2_value(paste0(pop_last_formatted %||% "N/A", "M")),
          paste("Average in", last_year, "for selected countries"),
          color = color_last_year,
        ))))})
  
  
  
  output$wdi_time_series <- renderPlotly({
    df <- wdi_filtered()
    req(nrow(df) > 0)
    indicator_sym <- rlang::sym(input$wdi_indicator)
    p <- ggplot(df, aes(x = Year, y = !!indicator_sym, color = Country)) +
      geom_line(na.rm = TRUE) +
      labs(x = "Year", y = input$wdi_indicator, title = paste("Time Trend of", input$wdi_indicator)) +
      theme_light(base_size = 13)
    if (input$wdi_logy) { p <- p + scale_y_log10() }
    ggplotly(p)
  })
  
  output$wdi_map <- renderLeaflet({
    df <- wdi_reactive() %>% filter(Year == input$wdi_map_yr) %>% select(Country, indicator = input$wdi_indicator)
    if (nrow(df) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>%
               addPopups(0, 0, "No data available for selected year."))
    }
    
    world_join <- left_join(world, df, by = c("name_long" = "Country"))
    if (all(is.na(world_join$indicator))) {
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>%
               addPopups(0, 0, paste("No indicator data available for", input$wdi_map_yr))
      )} 
    pal <- colorNumeric("viridis", domain = world_join$indicator, na.color = "transparent")
    l <- leaflet(world_join) %>% addTiles() %>%
      addPolygons(fillColor = ~pal(indicator), weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
                  label = ~paste(name_long, ":", round(indicator, 2))) %>%
      addLegend(pal = pal, values = ~indicator, title = input$wdi_indicator, position = "bottomright")
    
    year_label <- paste0(
      '<div style="',
      '  position: fixed; ',
      '  top: 50%; ',
      '  left: 50%; ',
      '  transform: translate(-50%, -50%); ',
      '  margin-left: 12.5%; ',
      '  opacity: 0.3; ',
      '  font-size: 100px; ',
      '  font-weight: bold; ',
      '  color: #333; ',
      '  pointer-events: none; ',
      '  z-index: 999;',
      '">', 
      input$wdi_map_yr,
      '</div>' 
    )
    
    l <- l %>%
      addControl(
        html = year_label,
        position = "topleft" 
      )
    l 
  })
  
  wdi_table_data <- reactive({
    df <- wdi_filtered()
    cols_to_select <- c("Country", "Year", input$wdi_table_cols)
    valid_cols <- intersect(cols_to_select, names(df))
    df %>% select(all_of(valid_cols))
  })
  
  output$wdi_table <- renderDataTable({
    wdi_table_data() %>% arrange(Country, Year) %>% datatable(options = list(pageLength = 10))
  })
  
  output$download_wdi_data <- downloadHandler(
    filename = function() { paste0("wdi_filtered_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(wdi_table_data(), file, row.names = FALSE) }
  )
  
  
  
  airbnb_filtered <- reactive({
    df <- airbnb_reactive()
    req(input$airbnb_cities_input, input$airbnb_price_range, input$airbnb_years_comp)
    df <- df %>% filter(Year %in% input$airbnb_years_comp)
    if (input$airbnb_roomtype != "All") { df <- df %>% filter(room_type == input$airbnb_roomtype) }
    df %>%
      filter(City %in% input$airbnb_cities_input,
             price >= input$airbnb_price_range[1], price <= input$airbnb_price_range[2])
  })
  
  
  comparison_bar_right <- function(diff_text, start_year, end_year) {
    bg_class <- "bg-success text-white"
    
    tags$div(
      class = paste("text-center p-3 shadow", bg_class),
      style = "height: 100%; display: flex; flex-direction: column; justify-content: center; align-items: center; border-radius: 0.75rem;",
      tags$span(style = "font-size: 1.25rem; font-weight: 700;", diff_text),
      tags$p(style = "font-size: 0.75rem; font-weight: 400; margin: 0; opacity: 0.8;", paste0("Difference between ", start_year, " & ", end_year))
    )}
  
  kpi_block_with_comparison <- function(title_text, metric_name, df, years_to_compare) { 
    year_1 <- years_to_compare[1]
    year_2 <- years_to_compare[2]
    kpis_long <- df %>%
      filter(Year %in% c(year_1, year_2)) %>%
      group_by(Year) %>%
      summarise(
        Avg_Price = round(mean(price, na.rm = TRUE), 2),
        Total_Listings = n(),
        Avg_Reviews = round(mean(number_of_reviews, na.rm = TRUE), 1)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value", -Year) %>%
      tidyr::unite("Metric_Year", Metric, Year, sep = "_") %>%
      tidyr::spread(Metric_Year, Value)
    
    get_kpi <- function(kpi_name, year) {
      col_name <- paste(kpi_name, year, sep = "_")
      val <- kpis_long[[col_name]][1]
      return(val)
    }
    
    
    if (metric_name == "Avg_Price") {
      value_y1 <- get_kpi(metric_name, year_1)
      value_y2 <- get_kpi(metric_name, year_2)
      value_y1_fmt <- paste0("$", value_y1 %||% "N/A")
      value_y2_fmt <- paste0("$", value_y2 %||% "N/A")
      diff <- round((value_y2 %||% 0) - (value_y1 %||% 0), 2)
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+$", "-$"), abs(diff))
      subtitle_y1 <- paste("Average in", year_1, "for selected criteria")
      subtitle_y2 <- paste("Average in", year_2, "for selected criteria")
      
    } else if (metric_name == "Total_Listings") {
      value_y1 <- get_kpi(metric_name, year_1)
      value_y2 <- get_kpi(metric_name, year_2)
      value_y1_fmt <- format(value_y1, big.mark=",") %||% "N/A"
      value_y2_fmt <- format(value_y2, big.mark=",") %||% "N/A"
      diff <- (value_y2 %||% 0) - (value_y1 %||% 0)
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+", ""), format(diff, big.mark=","))
      subtitle_y1 <- paste("Count in", year_1, "for selected criteria")
      subtitle_y2 <- paste("Count in", year_2, "for selected criteria")
      
    } else if (metric_name == "Avg_Reviews") {
      value_y1 <- get_kpi(metric_name, year_1)
      value_y2 <- get_kpi(metric_name, year_2)
      value_y1_fmt <- value_y1 %||% "N/A"
      value_y2_fmt <- value_y2 %||% "N/A"
      diff <- round((value_y2 %||% 0) - (value_y1 %||% 0), 1)
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+", ""), abs(diff))
      subtitle_y1 <- paste("Average in", year_1, "for selected criteria")
      subtitle_y2 <- paste("Average in", year_2, "for selected criteria")
    }
    
    
    
    color_y1 <- "secondary"
    color_y2 <- "info"
    
    tagList(
      h3(title_text),
      fluidRow(
        column(12,
               tags$div(
                 style = "display: flex; gap: 1rem; align-items: stretch; margin-bottom: 2rem;",
                 tags$div(
                   style = "flex: 2 2 66%; display: flex; flex-direction: column; gap: 1rem;",
                   
                   tags$div(style = "flex: 1 1 50%;",
                            kpi_box(
                              paste(title_text, "(", year_1, ")"),
                              h2_value(value_y1_fmt),
                              subtitle_y1,
                              color = color_y1,
                              custom_style = "margin-bottom: 0;"
                            )),
                   
                   tags$div(style = "flex: 1 1 50%;",
                            kpi_box(
                              paste(title_text, "(", year_2, ")"),
                              h2_value(value_y2_fmt),
                              subtitle_y2,
                              color = color_y2,
                              custom_style = "margin-top: 0;"
                            ))),
                 
                 tags$div(
                   style = "flex: 1 1 33%;",
                   comparison_bar_right(diff_text, year_1, year_2)
                 )))))}
  
  output$airbnb_kpis <- renderUI({
    df <- airbnb_filtered()
    years_to_compare <- as.character(sort(input$airbnb_years_comp))
    
    if (length(years_to_compare) < 2 || nrow(df) == 0) {
      
      return(tagList(
        p("Please select at least 2 years and 1+ cities to enable year-over-year comparison."),
        fluidRow(
          column(4, kpi_box("Avg Price", h2_value("N/A"), "Select criteria", color="light")),
          column(4, kpi_box("Listings", h2_value("N/A"), "Select criteria", color="light")),
          column(4, kpi_box("Reviews", h2_value("N/A"), "Select criteria", color="light"))
        )))}
    
    tagList(
      kpi_block_with_comparison(
        title_text = "Average Price ($)",
        metric_name = "Avg_Price",
        df = df,
        years_to_compare = years_to_compare
      ),
      
      kpi_block_with_comparison(
        title_text = "Total Listings Count",
        metric_name = "Total_Listings",
        df = df,
        years_to_compare = years_to_compare
      ),
      
      kpi_block_with_comparison(
        title_text = "Average Reviews Per Listing",
        metric_name = "Avg_Reviews",
        df = df,
        years_to_compare = years_to_compare
      ))})
  
  
  
  output$airbnb_box_plot <- renderPlot({
    df <- airbnb_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = City, y = price, fill = Year)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8, position = position_dodge(0.8)) +
      coord_flip() +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Price Distribution by City and Year",
           y = "Price (USD)",
           x = "City",
           fill = "Year") +
      theme_minimal()
  })
  
  
  output$airbnb_map <- renderLeaflet({
    df <- airbnb_filtered() 
    req(nrow(df) > 0) 
    
    if (nrow(df) > 1000) { df <- df %>% sample_n(1000) }
    
    pal <- colorFactor(c("blue", "red"), domain = df$Year)
    
    leaflet(df) %>% addTiles() %>% 
      addCircleMarkers( 
        lng = ~longitude, lat = ~latitude, radius = 4, color = ~pal(Year), 
        stroke = FALSE, fillOpacity = 0.8, 
        label = ~paste("Price: $", price, " | City:", City, " | Area:", neighbourhood, " | Type:", room_type, " | Year:", Year) 
      ) %>% 
      addLegend(pal = pal, values = ~Year, title = "Year", position = "bottomleft") 
  })
  
  output$airbnb_host_plot <- renderPlot({
    df <- airbnb_filtered()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = calculated_host_listings_count, y = reviews_per_month, color = Year)) +
      geom_point(alpha = 0.6) +
      scale_x_log10() +
      facet_grid(City ~ room_type) +
      labs(x = "Host Listing Count (Log Scale)", y = "Reviews Per Month", title = "Host Activity vs. Popularity by City and Room Type") +
      theme_light()
  })} 


shinyApp(ui, server)