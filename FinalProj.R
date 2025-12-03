##remove comments before submission!

#install packages
library(shiny)
library(bslib) #theme for shiny app
library(dplyr)
library(ggplot2) #plot visualization
library(DT) #interactive data tables
library(leaflet) #for creating the interactive map
library(sf) #simple features for reading/manipulating geographic data
library(rnaturalearth) #for accessing the world map boundary data
library(rnaturalearthdata) #data source for rnaturalearth
library(rlang) #for non-standard evaluation (dynamic column selection)
library(purrr) #for functional programming tools
library(tidyr) #for tidying data
library(tibble) #for modern data frame


#creates a kpi box with a solid colored background
kpi_box <- function(title, value_html, subtitle, color = "primary", icon_name = "bar-chart-fill", custom_style = "") {
  #solid text and background colors 
  bg_class <- switch(color,
                     "primary" = "bg-primary text-white",
                     "info" = "bg-info text-white",
                     "warning" = "bg-warning text-dark",
                     "success" = "bg-success text-white",
                     "danger" = "bg-danger text-white",
                     "secondary" = "bg-secondary text-white",
                     "light" = "bg-light text-dark",
                     "bg-primary text-white")
  
  #icon html for large icon and slightly transparent
  icon_html <- tags$i(
    class = paste0("bi bi-", icon_name), 
    style = paste0("font-size: 3rem; margin-right: 1rem;", 
                   if (grepl("text-white", bg_class)) "opacity: 0.6;" else "opacity: 0.4; color: #000;")
  )
  
  tags$div(
    class = paste("card shadow", bg_class), #add shadow and color classes
    #have rounded corners
    style = paste("height: 100%; overflow: hidden; display: flex; align-items: center; padding: 1rem; margin-bottom: 0;", 
                  custom_style), #to control top/bottom margins if needed
    
    #right side container 
    tags$div(
      class = "d-flex flex-column align-items-start",
      style = "flex-grow: 1;",
      
      #placeholder for main metric val
      value_html,
      
      #title and subtitle section
      p(class = "text-uppercase", 
        style = "font-weight: 600; font-size: 0.75rem; margin: 0; line-height: 1;", 
        title
      ), #subtitle below and smaller
      p(style = "font-size: 0.7rem; margin: 0; opacity: 0.8;", subtitle)
    ))}

#helper function for clean kpi boxes
h2_value <- function(val) {
  h2(style = "font-weight: 700; margin: 0; font-size: 2rem; line-height: 1.1;", val)
}

####wdi data prep
import_wdi <- "world_bank_development_indicators.csv" #import file
if (file.exists(import_wdi)) { #checks if exists
  wdi_data <- read.csv(import_wdi) %>%
    mutate( #start data manipulation
      Date_Parsed = as.Date(as.character(date), format = "%m/%d/%Y"), #convert data col into data object
      Year = suppressWarnings(as.integer(format(Date_Parsed, "%Y"))) #extracts year as integer from the date
    ) %>% 
    rename(Country = country) %>% #rename country to Country
    select(-date, -Date_Parsed) %>% #remove original date
    filter(!is.na(Year)) #filters out rows where year was not extracted properly
} else { #execute if file not found
  message("WDI file not found. Using dummy data.") #creates dummy data to use
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

wdi_vars <- names(wdi_data) %>% setdiff(c("Country", "Year")) #extracts names of all columns and removes country+year for indicator variables

wdi_years <- sort(unique(wdi_data$Year)) #extracts and sorts year
wdi_min_year <- if(length(wdi_years) > 0) min(wdi_years) #determines min year
wdi_max_year <- if(length(wdi_years) > 0) max(wdi_years)#determines max year

world <- ne_countries(scale = "medium", returnclass = "sf") #loads geographical boundary data using rnaturalearth

####airbnb data prep
import_airbnb_2023 <- "AB_US_2023.csv" #import 2023 data
import_airbnb_2020 <- "AB_US_2020.csv" #import 2020 data

airbnb_cols <- c( #select columns for analysis
  "city", "neighbourhood", "room_type", "price", 
  "latitude", "longitude", "number_of_reviews", "calculated_host_listings_count", 
  "reviews_per_month" 
) 

#define custom function to handle loading and cleaning
load_airbnb <- function(file_path, year) { 
  if (file.exists(file_path)) { #check exists
    data <- read.csv(file_path) %>% #read csv into dataframe
      select(all_of(airbnb_cols)) %>% #select cols defined above
      filter(price > 0 & price < 1000) %>% #filters out negative prices or high outlier prices
      mutate(Year = as.factor(year)) %>% #create Year variable
      rename(City = city) #rename city to City
  } else { #executes if file not found
    message(paste("File not found:", file_path, "Using dummy data for year", year)) #uses dummy data if not found
    data <- data.frame( #populates dummy data
      City = c("New York", "Boston"), neighbourhood = c("Hells Kitchen", "Back Bay"), 
      room_type = c("Entire home/apt", "Private room"), price = c(150, 75), 
      latitude = c(40.75, 42.35), longitude = c(-73.99, -71.08), 
      number_of_reviews = c(50, 20), calculated_host_listings_count = c(5, 1), 
      reviews_per_month = c(2.5, 1.0), 
      Year = as.factor(year) 
    ) 
  } 
  return(data) #returns either real or dummy data
} 

airbnb_2023 <- load_airbnb(import_airbnb_2023, 2023) #calls function to process/store 2023 data
airbnb_2020 <- load_airbnb(import_airbnb_2020, 2020) #calls function to process/store 2020 data

airbnb_data <- bind_rows(airbnb_2020, airbnb_2023) #combine vertically into single dataframe

airbnb_years <- sort(unique(airbnb_data$Year)) #extract and sort year values 
airbnb_cities <- sort(unique(airbnb_data$City)) #extract and sort city values
airbnb_roomtypes <- sort(unique(airbnb_data$room_type)) #extract and sort room type values


#####UI########

ui <- navbarPage( #starts the main navigation panel for the app
  title = "Project 3: Global Development and Airbnb Analysis", #sets the title displayed in the navigation bar
  theme = bs_theme(bootswatch = "flatly"), #use theme flatly
  
  ####overview tab
  tabPanel( #starts first tab panel
    "Overview", 
    fluidPage( #starts overview page
      h2("Data Visualization Project with R Shiny"), #level 2 heading
      #paragraph introducing datasets
      p("This dashboard explores two real-world datasets: World Development Indicators (WDI) and US Airbnb Listings (2020 vs 2023)."), 
      
      wellPanel( #starts a gray container for text
        h3("Dashboard Structure and Data Sources"), #level 3 heading in the wellpanel
        p("The application is divided into two primary analysis sections:"),
        tags$ul( #starts unordered list
          tags$li( #first list line
            strong("World Development Indicators (WDI):"), #descriptive text for wdi tab
            " Use interactive controls to filter, visualize trends, and map WDI indicators (like GDP, Population, Life Expectancy) across different countries and years. Features a time-series plot, a geographic map, and a data table."
          ),
          tags$li( #second list line
            strong("Airbnb Listings (2020 vs 2023):"),#descriptive text for airbnb tab
            " Compare key metrics, price distributions, and host activity for Airbnb listings in major US cities, highlighting changes between 2020 and 2023. Features year-over-year KPIs, a box plot for price analysis, and an interactive map of listings."
          )
        ),
        p(strong("Note:"), " This app is pre-configured for WDI (`world_bank_development_indicators.csv`), `AB_US_2020.csv`, and `AB_US_2023.csv`. Please ensure these files are available in the app directory for the full functionality.")
      ) 
    ) 
  ), 
  
  ####wdi tab panel ui
  tabPanel( #starts defining wdi tab
    "Global Development (WDI)", #set title
    sidebarLayout( #starts the sidebar and main panel layout
      sidebarPanel( #starts the left side panel for user controls
        h3("WDI Explorer Controls"), 
        width = 3, 
        selectizeInput(inputId = "wdi_countries", label = "Select Countries:", #creates dropdown to select country inputs
                       choices = sort(unique(wdi_data$Country)), #lists all countries in dropdown
                       selected = c("United States", "India", "China"), #default selected countries
                       multiple = TRUE), #can select multiple countries
        sliderInput(inputId = "wdi_years", label = "Select Year Range:",  #year slider
                    min = wdi_min_year, max = wdi_max_year, #sets min as earliest year and max as latest year
                    value = c(wdi_min_year, wdi_max_year), sep = "", step = 1), #default to all years
        selectInput("wdi_indicator", "Select Indicator:", choices = wdi_vars, #select indicator to analyze
                    selected = "GDP_current_US"), #default to gdp
        checkboxInput("wdi_logy", "Log scale (y-axis)", value = FALSE), #check for log scale
        selectizeInput(inputId = "wdi_table_cols", label = "Select Columns for Table/Download:", #select variables for the downloadable table
                       choices = wdi_vars, #give choices of all variables in dataset
                       selected = c("GDP_current_US", "life_expectancy_at_birth", "population", "Year"), #default selections
                       multiple = TRUE) #can select multiple
      ), 
      mainPanel( #starts the right-side panel for outputs
        tabsetPanel( #starts a set of subtabs for different visualizations
          tabPanel("Summary KPIs", h4("Key Global Development Indicators"), uiOutput("wdi_kpis")), #tab 1 for kpis
          tabPanel("Time-Series Explorer", h4("Indicator Trend Over Time"), plotlyOutput("wdi_time_series")), #tab 2 for indicator trend over time
          tabPanel("Map View", h4("Geographic View by Year"), #tab 3 for map view
                   numericInput("wdi_map_yr", "Select Year for Map:", #create field to select year
                                value = wdi_max_year, min = wdi_min_year, max = wdi_max_year), #defaults to latest year available and sets min and max years
                   leafletOutput("wdi_map")), #placeholder for the leaflet map output
          tabPanel("Data Table", h4("Filtered WDI Data"), #tab4 for table
                   dataTableOutput("wdi_table"), #placeholder for table output
                   downloadButton("download_wdi_data", "Download Filtered Data")) #download button for filtered data
        ), 
        width = 9 
      ))), 
  
  ####airbnb tab panel ui
  tabPanel( #starts defining airbnb tab
    "Airbnb Listings (2020 vs 2023)", #set title
    sidebarLayout( #starts the sidebar and main panel layout
      sidebarPanel( #starts the left side panel for user controls
        h3("Airbnb Analysis Controls"), #display heading for control section
        width = 3, 
        selectizeInput(inputId = "airbnb_years_comp", label = "Select Years for Comparison:", #dropdown for selecting 1+ years
                       choices = airbnb_years, #provides unique years as choices
                       selected = airbnb_years, #selection defaults to include all years
                       multiple = TRUE), #can select multiple years
        
        selectizeInput(inputId = "airbnb_cities_input", label = "Select Cities:", #dropdown for selecting cities
                       choices = airbnb_cities, #provides all cities as choices
                       selected = head(airbnb_cities, 3), #selection defaults to first 3 cities
                       multiple = TRUE), #can select multiple cities
        
        selectInput("airbnb_roomtype", "Select Room Type:", #dropdown to select single room type
                    choices = c("All", airbnb_roomtypes), selected = "All"), #give all and unique room types as choices, defaults to all
        
        sliderInput("airbnb_price_range", "Price Range ($):", #create slider for filtering by price
                    min = min(airbnb_data$price), max = max(airbnb_data$price), #set min as minimum price in dataset and max as maximum price in dataset
                    value = c(50, 300)) #defaults to $50-$300
      ), 
      mainPanel( #starts the right side panel for outputs
        tabsetPanel( #starts a set of subtabs for different visualizations
          tabPanel("Summary KPIs", h4("Key Performance Indicators"), uiOutput("airbnb_kpis")), #subtab for KPIs
          tabPanel("City Analysis", h4("Price Distribution: Year-over-Year Comparison by City"), plotOutput("airbnb_box_plot")), #subtab for price distribution plot
          tabPanel("Interactive Map", h4("Listing Locations"), leafletOutput("airbnb_map")), #subtab for interactive map
          tabPanel("Host Insights", h4("Host Activity vs Reviews"), #subtab for host activity scatterplot
                   plotOutput("airbnb_host_plot"),) 
        ), 
        width = 9 
      ))))




####### server function begins here ######

server <- function(input, output, session) { #defines the server function
  wdi_reactive <- reactive({ wdi_data }) #creates a reactive expression for the wdi data
  airbnb_reactive <- reactive({ airbnb_data }) #creates a reactive expression for the airbnb data
  
  ####wdi server logic starts here ####
  
  wdi_filtered <- reactive({ #starts the reactive expression for filtering wdi data
    df <- wdi_reactive() #retrieve data 
    req(input$wdi_countries, input$wdi_years) #ensures required inputs are selected
    df %>% 
      filter(Country %in% input$wdi_countries, #keep only selected countries
             Year >= input$wdi_years[1], Year <= input$wdi_years[2]) #keeps years between selected years
  }) 
  
  output$wdi_kpis <- renderUI({ #renders kpi boxes
    df <- wdi_filtered() #gets the filtered wdi data
    req(nrow(df) > 0) #requires at least one row of data
    
    #define colors
    color_first_year <- "success" #green color
    color_overall_avg <- "primary" #dark color
    color_last_year <- "info" #blue color
    
    first_year <- input$wdi_years[1] #retrieve first year from slider
    last_year <- input$wdi_years[2] #retrieve last year from slider
    
    #calculate metrics for first year
    df_first <- df %>% filter(Year == first_year) #filters reactive df to include only first year
    le_first <- round(mean(df_first$life_expectancy_at_birth, na.rm = TRUE), 1) #calculate avg life expectancy for year 1
    gdp_first <- mean(df_first$GDP_current_US, na.rm = TRUE) #calculate avg gdp for year 1
    gdp_first_formatted <- format(round(gdp_first / 1e9, 2), big.mark = ",") #format gdp (billions, round 2 decimals, comma separator)
    pop_first <- mean(df_first$population, na.rm = TRUE) #calculate avg pop for year 1
    pop_first_formatted <- format(round(pop_first / 1e6, 2), big.mark = ",") #format pop (millions, 2 decimals, comma separator)
    
    #calculate metrics between years
    avg_le <- round(mean(df$life_expectancy_at_birth, na.rm = TRUE), 1) #calculate avg life expectancy for all years
    avg_gdp <- mean(df$GDP_current_US, na.rm = TRUE) #calculate avg gdp for all years
    avg_gdp_formatted <- format(round(avg_gdp / 1e9, 2), big.mark = ",") #format gdp (billions, round 2 decimals, comma separator)
    avg_pop <- mean(df$population, na.rm = TRUE) #calculate avg pop across all years
    avg_pop_formatted <- format(round(avg_pop / 1e6, 2), big.mark = ",") #format pop (millions, 2 decimals, comma separator)
    
    #calculate metrics for last year
    df_last <- df %>% filter(Year == last_year) #filters reactive df to include only last year
    le_last <- round(mean(df_last$life_expectancy_at_birth, na.rm = TRUE), 1) #calculate avg life expectancy for last year
    gdp_last <- mean(df_last$GDP_current_US, na.rm = TRUE) #calculate avg gdp for last year
    gdp_last_formatted <- format(round(gdp_last / 1e9, 2), big.mark = ",") #format gdp (billions, round 2 decimals, comma separator)
    pop_last <- mean(df_last$population, na.rm = TRUE) #calculate avg pop for last year
    pop_last_formatted <- format(round(pop_last / 1e6, 2), big.mark = ",") #format pop (millions, 2 decimals, comma separator)
    
    
    ####format kpi boxes for wdi#####
    
    tagList( #group and return elements sequentially
      #ui structure for first years
      h3(paste0("Metrics for Initial Year (", first_year, ")")),#heading for first year averages
      fluidRow( #first set of kpis
        column(4, kpi_box( #render life expectancy kpi box
          "Life Expectancy", #set kpi title
          h2_value(le_first %||% "N/A"), #set main value
          paste("Average in", first_year, "for selected countries"), #set subtitle
          color = color_first_year, #set color
        )), 
        column(4, kpi_box( #render gdp kpi box for year 1
          "GDP", #set kpi title
          h2_value(paste0("$", gdp_first_formatted %||% "N/A", "B")), #set main value with $ suffix of B
          paste("Average in", first_year, "for selected countries"), #set subtitle
          color = color_first_year, #set color
        )), 
        column(4, kpi_box( #render pop kpi box for year 1
          "Population", #set kpi title
          h2_value(paste0(pop_first_formatted %||% "N/A", "M")), #set main value suffix M
          paste("Average in", first_year, "for selected countries"), #set subtitle
          color = color_first_year, #set color
        ))
      ), 
      
      #ui structure for overall years
      h3("Overall Averages Across Selected Range"), #heading for overall averages
      fluidRow( #next set of kpis
        column(4, kpi_box( #render life expectancy box for overall
          "Life Expectancy", #set kpi title
          h2_value(avg_le %||% "N/A"), #set main value
          "Years (Average across all selected countries/years)", #set subtitle
          color = color_overall_avg, #set color
        )), 
        column(4, kpi_box( #render kpi gdp box for overall avg
          "GDP", #set kpi title
          h2_value(paste0("$", avg_gdp_formatted %||% "N/A", "B")), #set main value with $ and B suffix
          "Billions USD (Average across all selected countries/years)", #set subtitle
          color = color_overall_avg, #set color
        )), 
        column(4, kpi_box( #pop kpi box for overall average
          "Population", #set kpi title
          h2_value(paste0(avg_pop_formatted %||% "N/A", "M")), #set main value with M suffix
          "Millions (Average across all selected countries/years)", #set subtitle
          color = color_overall_avg, #set color
        ))
      ), 
      
      #ui structure for last year
      h3(paste0("Metrics for Final Year (", last_year, ")")), #set heading for last year
      fluidRow( #last set of kpis
        column(4, kpi_box( #render life expectancy kpi box in last year
          "Life Expectancy", #set kpi title
          h2_value(le_last %||% "N/A"), #set main value
          paste("Average in", last_year, "for selected countries"), #set subtitle
          color = color_last_year, #set color
        )), 
        column(4, kpi_box( #render gdp kpi box for last year
          "GDP", #set kpi title
          h2_value(paste0("$", gdp_last_formatted %||% "N/A", "B")), #set main value with $ and B suffix
          paste("Average in", last_year, "for selected countries"), #set subtitle
          color = color_last_year, #set color
        )), 
        column(4, kpi_box( #render pop kpi box for last year
          "Population", #set kpi title
          h2_value(paste0(pop_last_formatted %||% "N/A", "M")), #set main value with M suffix
          paste("Average in", last_year, "for selected countries"), #set subtitle
          color = color_last_year, #set color
        ))))})
  
  
  
  ####time series plot 
  output$wdi_time_series <- renderPlotly({ #renders interactive time-series plot
    df <- wdi_filtered() #gets the filtered wdi data
    req(nrow(df) > 0) #requires at least one row of data
    indicator_sym <- rlang::sym(input$wdi_indicator) #converts the selected indicator string into a symbol
    p <- ggplot(df, aes(x = Year, y = !!indicator_sym, color = Country)) + #initializes ggplot to plot selected indicator
      geom_line(na.rm = TRUE) + #adds lines (ignore n/a values)
      labs(x = "Year", y = input$wdi_indicator, title = paste("Time Trend of", input$wdi_indicator)) + #sets dynamic labels and title
      theme_light(base_size = 13) #apply light theme
    if (input$wdi_logy) { p <- p + scale_y_log10() } #conditionally adds a log scale if the checkbox is checked
    ggplotly(p) #converts the ggplot object to an interactive plotly object
  })
  
  ####geographic map view using leaflet
  output$wdi_map <- renderLeaflet({ #renders the interactive wdi map
    df <- wdi_reactive() %>% filter(Year == input$wdi_map_yr) %>% select(Country, indicator = input$wdi_indicator) #filters data for the map year and selects the indicator
    if (nrow(df) == 0) { #checks for data in selected year
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>% #returns a basic world map
               addPopups(0, 0, "No data available for selected year.")) #adds popup if no data
    }
    
    world_join <- left_join(world, df, by = c("name_long" = "Country")) #joins the indicator data to the sf world map data
    if (all(is.na(world_join$indicator))) { #checks if no indicator data could be joined
      return(leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2) %>% #returns a basic world map
               addPopups(0, 0, paste("No indicator data available for", input$wdi_map_yr)) #adds an informative popup
      )} 
    pal <- colorNumeric("viridis", domain = world_join$indicator, na.color = "transparent") #creates a color palette based on indicator values
    leaflet(world_join) %>% addTiles() %>% #initializes leaflet with the joined map data and tiles
      addPolygons(fillColor = ~pal(indicator), weight = 1, opacity = 1, color = "white", fillOpacity = 0.7, #adds country boundaries colored by the indicator
                  label = ~paste(name_long, ":", round(indicator, 2))) %>% #adds country name and indicator value to the hover label
      addLegend(pal = pal, values = ~indicator, title = input$wdi_indicator, position = "bottomright") #adds a legend for the color scale
  })
  
  ####data table and download logic
  wdi_table_data <- reactive({ #reactive expression for the data table and download
    df <- wdi_filtered() #gets the filtered wdi data
    cols_to_select <- c("Country", "Year", input$wdi_table_cols) #select country, year, and indicators chosen by user
    valid_cols <- intersect(cols_to_select, names(df)) #ensure columns exist in data (handles for empty data)
    df %>% select(all_of(valid_cols)) #selects the valid subset of columns
  })
  
  output$wdi_table <- renderDataTable({ #renders the data table
    wdi_table_data() %>% arrange(Country, Year) %>% datatable(options = list(pageLength = 10)) #displays the selected columns in the table
  })
  
  output$download_wdi_data <- downloadHandler( #sets up the download handler for the data table
    filename = function() { paste0("wdi_filtered_data_", Sys.Date(), ".csv") }, #defines the name of the downloaded file
    content = function(file) { write.csv(wdi_table_data(), file, row.names = FALSE) } #writes the selected columns to the output file
  )
  
  
  
  #######airbnb server logic start here######
  
  airbnb_filtered <- reactive({ #starts the reactive expression for filtering airbnb data
    df <- airbnb_reactive() #get airbnb data
    req(input$airbnb_cities_input, input$airbnb_price_range, input$airbnb_years_comp) #requires city, price range, and year inputs
    df <- df %>% filter(Year %in% input$airbnb_years_comp) #filters data by the selected years
    if (input$airbnb_roomtype != "All") { df <- df %>% filter(room_type == input$airbnb_roomtype) } #conditionally filters by room type
    df %>% #starts the final filtering pipeline
      filter(City %in% input$airbnb_cities_input, #filters for selected cities
             price >= input$airbnb_price_range[1], price <= input$airbnb_price_range[2]) #filters for the selected price range
  })
  
  
  ####airbnb comparison for right side bar
  comparison_bar_right <- function(diff_text, start_year, end_year) { #helper to create comparison bar
    bg_class <- "bg-success text-white" #make background success (green)
    
    tags$div( #format for comparison bar on right
      class = paste("text-center p-3 shadow", bg_class),
      style = "height: 100%; display: flex; flex-direction: column; justify-content: center; align-items: center; border-radius: 0.75rem;",
      tags$span(style = "font-size: 1.25rem; font-weight: 700;", diff_text),
      tags$p(style = "font-size: 0.75rem; font-weight: 400; margin: 0; opacity: 0.8;", paste0("Difference between ", start_year, " & ", end_year))
    )}
  
  #customer helper function to generate complete kpi block (title, 2 boxes stacked vertically on left, 1 comparison bar on the right)
  kpi_block_with_comparison <- function(title_text, metric_name, df, years_to_compare) { 
    year_1 <- years_to_compare[1] #extract year 1 (earlier) from input vector
    year_2 <- years_to_compare[2] #extract year 2 (most recent) from input vector 
    kpis_long <- df %>% #calculate kpi's grouped by year
      filter(Year %in% c(year_1, year_2)) %>% #filter data to only include listings from the two compared years
      group_by(Year) %>% #groups data by year factor
      summarise( #calculates the three summary metrics (avg price, total listings, avg reviews)
        Avg_Price = round(mean(price, na.rm = TRUE), 2), #calculate average price (round 2 decimals, ignore n/a)
        Total_Listings = n(), #calculate total listings
        Avg_Reviews = round(mean(number_of_reviews, na.rm = TRUE), 1) #calculate average number of reviews (round to 1 decimal, ignore n/a)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value", -Year) %>% #pivot data from wide to long and put all metric names into single metric column
      tidyr::unite("Metric_Year", Metric, Year, sep = "_") %>% #combine metric column and year into new metric_year col (ex. Avg_Price_2023)
      tidyr::spread(Metric_Year, Value) #pivot data from long back to wide using metric_year as new column header
    
    get_kpi <- function(kpi_name, year) { #define nested helper function to retrieve specific kpi value by name and year
      col_name <- paste(kpi_name, year, sep = "_") #construct required column name (ex. Avg_Price_2023)
      val <- kpis_long[[col_name]][1] #retrieve value from the the kpis_long dataframe using col_name
      return(val) #return kpi value
    }
    
    
    ####logic block for calculating display values 
    
    #conditional logic block executed if the current metric is avg_price
    if (metric_name == "Avg_Price") {
      value_y1 <- get_kpi(metric_name, year_1) #retrieve avg price for year 1
      value_y2 <- get_kpi(metric_name, year_2) #retrieve avg price for year 2
      value_y1_fmt <- paste0("$", value_y1 %||% "N/A") #format 1st year value as currency string, defaults to n/a if null
      value_y2_fmt <- paste0("$", value_y2 %||% "N/A") #format 2nd year value as currency string, defaults to n/a if null
      diff <- round((value_y2 %||% 0) - (value_y1 %||% 0), 2) #calculate difference of year2-year1 
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+$", "-$"), abs(diff)) #format difference value (either +/- with $ and use absolute value)
      subtitle_y1 <- paste("Average in", year_1, "for selected criteria") #construct subtitle of year 1 kpi box
      subtitle_y2 <- paste("Average in", year_2, "for selected criteria") #construct subtitle of year 2 kpi box
      
      #conditional logic block executed if the current metric is total_listings
    } else if (metric_name == "Total_Listings") {
      value_y1 <- get_kpi(metric_name, year_1) #retrieve total listings for year 1
      value_y2 <- get_kpi(metric_name, year_2) #retrieve total listings for year 2
      value_y1_fmt <- format(value_y1, big.mark=",") %||% "N/A" #format's year 1 value with commas, default to n/a
      value_y2_fmt <- format(value_y2, big.mark=",") %||% "N/A" #format's year 2 value with commas, default to n/a
      diff <- (value_y2 %||% 0) - (value_y1 %||% 0) #calculate difference of year2-year1
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+", ""), format(diff, big.mark=",")) #format difference value (either +/- with comma)
      subtitle_y1 <- paste("Count in", year_1, "for selected criteria") #construct subtitle of year 1 kpi box
      subtitle_y2 <- paste("Count in", year_2, "for selected criteria") #construct subtitle of year 2 kpi box
      
      #conditional logic block executed if the current metric is avg_reviews
    } else if (metric_name == "Avg_Reviews") {
      value_y1 <- get_kpi(metric_name, year_1) #retrieve avg reviews for year 1
      value_y2 <- get_kpi(metric_name, year_2) #retrieve avg reviews for year 2
      value_y1_fmt <- value_y1 %||% "N/A" #assign the 1st year value 
      value_y2_fmt <- value_y2 %||% "N/A" #assign the 2nd year value
      diff <- round((value_y2 %||% 0) - (value_y1 %||% 0), 1) #calculate difference, round to 1 decimal
      diff_text <- if (is.na(diff)) "N/A" else paste0(ifelse(diff > 0, "+", ""), abs(diff)) #format difference value (either +/- and use absolute value)
      subtitle_y1 <- paste("Average in", year_1, "for selected criteria") #construct subtitle of year 1 kpi box
      subtitle_y2 <- paste("Average in", year_2, "for selected criteria") #construct subtitle of year 2 kpi box
    }
    
    
    
    ####kpi box format and inputs
    
    #make consistent colors for the years
    color_y1 <- "secondary" #bootstrap for gray
    color_y2 <- "info" #bootstrap for blue
    
    tagList(
      h3(title_text),#title
      fluidRow( #create the block of two stacked boxes and 1 vertical bar
        column(12,
               tags$div( #hold left kpi col and right comparison bar
                 style = "display: flex; gap: 1rem; align-items: stretch; margin-bottom: 2rem;",
                 tags$div( #hold the two year kpi boxes and stack vertically
                   style = "flex: 2 2 66%; display: flex; flex-direction: column; gap: 1rem;", #tke 2/3 width and stack vertically
                   
                   #box 1
                   tags$div(style = "flex: 1 1 50%;",
                            kpi_box(
                              paste(title_text, "(", year_1, ")"), #title of box
                              h2_value(value_y1_fmt), #format main value
                              subtitle_y1, #sets subtitle
                              color = color_y1, #assign color to secondary
                              custom_style = "margin-bottom: 0;" #remove gap in boxes
                            )),
                   
                   #box 2
                   tags$div(style = "flex: 1 1 50%;",
                            kpi_box(
                              paste(title_text, "(", year_2, ")"), #title of box
                              h2_value(value_y2_fmt), #format main value
                              subtitle_y2, #sets subtitle
                              color = color_y2, #assign color to info
                              custom_style = "margin-top: 0;" # Remove gap between boxes
                            ))), #end vertical stack
                 
                 #comparison bar 
                 tags$div(
                   style = "flex: 1 1 33%;", #takes 1/3 width
                   comparison_bar_right(diff_text, year_1, year_2)
                 )))))}
  
  #assigns renderUI result to airbnb_kpis variable
  output$airbnb_kpis <- renderUI({ #create reactive for the kpi's
    df <- airbnb_filtered() #call reactive to get filtered airbnb data
    years_to_compare <- as.character(sort(input$airbnb_years_comp)) #convert selected years to character strings and sort by year
    
    #start conditional to check viability of year comparison
    #must have at least tw
    if (length(years_to_compare) < 2 || nrow(df) == 0) { #must have at least 2 selected years with one row
      
      #if condition not met, execute the following
      return(tagList(
        #displays message to the user explaining why the comparison isn't visible.
        p("Please select at least 2 years and 1+ cities to enable year-over-year comparison."),
        #start fluid layout row for the placeholder kpi boxes
        fluidRow(
          #col 1 for average price placeholder
          column(4, kpi_box("Avg Price", h2_value("N/A"), "Select criteria", color="light")),
          #col 2 for listings count placeholder
          column(4, kpi_box("Listings", h2_value("N/A"), "Select criteria", color="light")),
          #col 3 for review placeholder
          column(4, kpi_box("Reviews", h2_value("N/A"), "Select criteria", color="light"))
        )))}
    
    #use taglist to combine multiple ui elements into a single output
    tagList(
      #adds header to 1st block - average price
      kpi_block_with_comparison( #call the custom helper function to generate the ui
        title_text = "Average Price ($)", #set title of block
        metric_name = "Avg_Price", #specify internal metric/column to calculate (avg_price logic in helper)
        df = df, #pass filtered dataframe to helper
        years_to_compare = years_to_compare #pass sorted list of years to helper
      ),
      
      #adds header to 2nd block - total listings
      kpi_block_with_comparison( #call the custom helper function to generate the ui
        title_text = "Total Listings Count", #set title of block
        metric_name = "Total_Listings", #specify internal metric/column to calculate (total_listings logic in helper)
        df = df, #pass filtered dataframe to helper
        years_to_compare = years_to_compare #pass sorted list of years to helper
      ),
      
      #adds header to 3rd block - average reviews
      kpi_block_with_comparison( #call the custom helper function to generate the ui
        title_text = "Average Reviews Per Listing", #set title of block
        metric_name = "Avg_Reviews", #specify internal metric/column to calculate (avg_reviews logic in helper)
        df = df, #pass filtered dataframe to helper
        years_to_compare = years_to_compare #pass sorted list of years to helper
      ))})
  
  
  
  ####### airbnb chart+map logic
  
  
  ####city analysis (price distribution by city)
  output$airbnb_box_plot <- renderPlot({ #renders the price distribution box plot
    df <- airbnb_filtered() #gets the filtered airbnb data
    req(nrow(df) > 0) #requires at least one row of data
    
    ggplot(df, aes(x = City, y = price, fill = Year)) + #initializes ggplot
      geom_boxplot(outlier.shape = NA, alpha = 0.8, position = position_dodge(0.8)) + #adds box plots, hides outliers, and separates them by year
      coord_flip() + #flips the axes for horizontal boxes
      scale_fill_brewer(palette = "Set1") + #applies a color scheme
      labs(title = "Price Distribution by City and Year", #sets the main title
           y = "Price (USD)", #sets the y-axis label
           x = "City", #sets the x-axis label
           fill = "Year") + #sets the legend title
      theme_minimal() #applies a minimal theme
  })
  
  
  ####interactive map using leaflet
  output$airbnb_map <- renderLeaflet({ #renders the interactive listing map
    df <- airbnb_filtered() #gets the filtered airbnb data
    req(nrow(df) > 0) #requires at least one row of data
    
    if (nrow(df) > 1000) { df <- df %>% sample_n(1000) } #downsamples data to 1000 points if needed for performance
    
    pal <- colorFactor(c("blue", "red"), domain = df$Year) #creates a color palette to distinguish years
    
    leaflet(df) %>% addTiles() %>% #initializes leaflet and adds default map tiles
      addCircleMarkers( #adds circular markers for each listing
        lng = ~longitude, lat = ~latitude, radius = 4, color = ~pal(Year), #sets location, size, and colors by year
        stroke = FALSE, fillOpacity = 0.8, #removes marker border and sets fill opacity
        label = ~paste("Price: $", price, " | City:", City, " | Area:", neighbourhood, " | Type:", room_type, " | Year:", Year) #sets the hover label content
      ) %>% 
      addLegend(pal = pal, values = ~Year, title = "Year", position = "bottomleft") #adds a legend for the marker colors
  })
  
  
  ####host insights scatter plot
  output$airbnb_host_plot <- renderPlot({ #renders the scatter plot
    df <- airbnb_filtered() #gets the filtered airbnb data
    req(nrow(df) > 0) #requires at least one row of data
    
    ggplot(df, aes(x = calculated_host_listings_count, y = reviews_per_month, color = Year)) + #begin ggplot with host listings on x, reviews on y, and color by year
      geom_point(alpha = 0.6) + #adds scatter points with transparency
      scale_x_log10() + #applies a log scale to the host listing count axis
      facet_grid(City ~ room_type) + #creates a grid of plots faceted by city and room type
      labs(x = "Host Listing Count (Log Scale)", y = "Reviews Per Month", title = "Host Activity vs. Popularity by City and Room Type") + #sets labels and title
      theme_light() #applies a minimal theme
  })} 


shinyApp(ui, server) #run shiny app