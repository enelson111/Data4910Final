# Data4910Final

**Global Development and Airbnb Analysis Dashboard**
An interactive R Shiny dashboard for exploring World Bank Development Indicators (WDI) and US Airbnb listing data across multiple years.

---

## Dataset Description

### World Bank Development Indicators (WDI)

* **Source:** `world_bank_development_indicators.csv`
* **Original Data:** World Bank World Development Indicators ([https://www.kaggle.com/datasets/nicolasgonzalezmunoz/world-bank-world-development-indicators?resource=download](https://www.kaggle.com/datasets/nicolasgonzalezmunoz/world-bank-world-development-indicators?resource=download)) by Nicolás Ariel González Muñoz
* **Coverage:** 1960-2023

## Key Variables:
* `country`: The country or geographic region.
* `date`: Date of the measurement. This column along with country can be used as index.
* `agricultural_land%`: Agricultural land as a % of land area of the country/region.
* `forest_land%`: Forest area as the % of land area of the country/region.
* `land_area`: Land area, measured in km^2.
* `avg_precipitation`: Average precipitation in depth, measured in mm per year.
* `trade_in_services%`: Trade in services as a % of GDP.
* `control_of_corruption_estimate`: Index that makes an estimate of the control of corruption.
* `control_of_corruption_std`: Standard error of the estimate of control of corruption.
* `access_to_electricity%`: Percentage of the population that has access to electricity.
* `renewvable_energy_consumption%`: Renewable energy consumption as a % of total final energy consumption.
* `electric_power_consumption`: Electric power consumption, measured in kWh per capita.
* `CO2_emisions`: CO2 emisions measured in kt.
* `other_greenhouse_emisions`: Total greenhouse gas emissions, measured in kt of CO2 equivalent.
* `population_density`: Population density, measured in people per km^2 of land area.
* `inflation_annual%`: Inflation, consumer prices, as annual %.
* `real_interest_rate`: Real interest rate (%).
* `risk_premium_on_lending`: Risk premium on lending (lending rate minus treasury bill rate, %).
* `research_and_development_expenditure%`: Research and development expenditure, as a percentage of GDP.
* `central_goverment_debt%`: Central government debt, total , as a % of GDP.
* `tax_revenue%`: Tax revenue as a % of GDP.
* `expense%`: Expense as a % of GDP.
* `goverment_effectiveness_estimate`: Index that makes an estimate of the Government Effectiveness.
* `goverment_effectiveness_std`: Standard error of the estimate of Government Effectiveness.
* `human_capital_index`: Human Capital Index (HCI) (scale 0-1).
* `doing_business`: Ease of doing business score (0 = lowest performance to 100 = best performance).
* `time_to_get_operation_license`: Days required to obtain an operating license.
* `statistical_performance_indicators`: Statistical performance indicators (SPI): Overall score (scale 0-100).
* `individuals_using_internet%`: Percentage of population using the internet.
* `logistic_performance_index`: Logistics performance index: Overall (1=low to 5=high).
* `military_expenditure%`: Military expenditure as a % of GDP.
* `GDP_current_US`: GDP (current US$).
* `political_stability_estimate`: Index that makes an estimate of the Political Stability and Absence of Violence/Terrorism.
* `political_stability_std`: Standard error of the estimate of Political Stability and Absence of Violence/Terrorism.
* `rule_of_law_estimate`: Index that makes an estimate of the Rule of Law.
* `rule_of_law_std`: Standard error of the estimate of Rule of Law.
* `regulatory_quality_estimate`: Index that makes an estimate of Regulatory Quality.
* `regulatory_quality_std`: Standard error of the estimate of Regulatory Quality.
* `government_expenditure_on_education%`: Government expenditure on education, total, as a % of GDP.
* `government_health_expenditure%`: Domestic general government health expenditure as a % of GDP.
* `multidimensional_poverty_headcount_ratio%`: Multidimensional poverty headcount ratio (% of total population).
* `gini_index`: Gini index.
* `birth_rate`: Birth rate, crude (per 1,000 people).
* `death_rate`: Death rate, crude (per 1,000 people).
* `life_expectancy_at_birth`: Life expectancy at birth, total (years).
* `population`: Total population.
* `rural_population`: Rural population.
* `voice_and_accountability_estimate`: Index that makes an estimate of Voice and Accountability.
* `voice_and_accountability_std`: Standard error of the estimate of Voice and Accountability.
* `intentional_homicides`: Intentional homicides (per 100,000 people).

### Airbnb US Listings Data

* **Sources:**
    * `AB_US_2020.csv` - Listings from 2020
    * `AB_US_2023.csv` - Listings from 2023
* **Original Data:** U.S. Airbnb Open Data ([https://www.kaggle.com/datasets/kritikseth/us-airbnb-open-data?select=AB_US_2023.csv](https://www.kaggle.com/datasets/kritikseth/us-airbnb-open-data?select=AB_US_2023.csv)) by Kritik Seth
* **Coverage:** Major US cities including Los Angeles, New York City, Denver, New Orleans, and more.

## Key Variables:
* `City`: City name
* `neighbourhood`: Neighborhood/area within city
* `room_type`: Type of accommodation (Entire home/apt, Private room, Shared room)
* `price`: Nightly price in USD, filtered to include prices between $0-$1,000
* `latitude`/`longitude`: Geographical coordinates
* `number_of_reviews`: Total reviews received
* `calculated_host_listings_count`: Number of listings per host
* `reviews_per_month`: Average monthly review rate

---

## Design Choices & Key Findings

### Dashboard Structure
The application uses a **multi-tab navigation interface** with three main sections:
* **Overview Tab:** Provides context about the datasets and dashboard functionality
* **Global Development (WDI) Tab:** Interactive exploration of world development indicators
* **Airbnb Listings Tab:** Year-over-year comparison of Airbnb market trends (2020 vs 2023)

### WDI Section Design Choices

#### Interactive Controls:
* Multi-country selection to enable comparative analysis
* Year range slider for flexible time period selection
* Indicator dropdown covering economic, demographic, and environmental metrics
* Optional log-scale transformation for handling wide-ranging values
* Customizable table columns for downloading table data
* **Custom data upload with variable mapping** - Users can upload their own WDI CSV files and map column names to required variables (Country, Year, GDP, Life Expectancy, Population)

#### Visualizations:
* **Summary KPIs:** Three-year comparison (initial year, average across range, final year) for Life Expectancy, GDP, and Population
* **Time-Series Plot:** Interactive **Plotly** line chart showing indicator trends over time by country
* **Geographic Map:** **Leaflet** chloropleth map displaying indicator values by country for a selected year
* **Data Table:** Sortable, filterable table with CSV download functionality

#### Key Finding:
* Life expectancy, GDP, and population show **consistent upward trends** across all countries in the dataset, with values increasing substantially from earlier years to more recent years.

### Airbnb Section Design Choices

#### Interactive Controls:
* Year comparison selector (2020 vs 2023)
* Multi-city selector for regional comparisons
* Room type filter to segment by accommodation category
* Price range slider to focus on specific market segments

#### Visualizations:
* **Summary KPIs:** Year-over-year comparison blocks showing average price, total listings, and average reviews with difference calculations
* **Box Plot:** Price distribution by city and year to identify market changes
* **Interactive Map:** **Leaflet** map with color-coded markers by year (blue for 2020, red for 2023)
* **Host Insights Scatter Plot:** Relationship between host activity and listing popularity, faceted by city and room type

#### Key Findings:
* There is a trend of **prices increasing** in all cities between the year 2020 and 2023. With all countries, room types, and prices selected, the average price increases by **$26.33**.
* Between 2020 and 2023, the availability of **Entire home/apt increased by approximately 15,000**, while Hotel rooms decreased by 1,225 units, Private rooms decreased by 6,699 units, and Shared rooms decreased by 1,747 units.
* There is a trend of the **average reviews per listing increasing** between years 2020 and 2023. With all countries, room types, and prices selected, the average reviews per listing increased by **6.5**.

### Technical Design Decisions

* **Data Handling:**
    * **Default data loading** - The app automatically loads the default WDI and Airbnb datasets if they exist in the app directory
    * **Optional custom data upload** - Users can optionally upload their own WDI CSV file with any column structure
    * **Flexible variable mapping** - When uploading custom data, users map their column names to required variables through dropdown menus
    * Fallback to **dummy data** if CSV files are missing (enables testing without datasets)
    * **Reactive filtering system** ensures real-time updates based on user inputs
    * **Downsampling of map markers** (1,000 max) to maintain performance with large datasets
* **UI/UX:**
    * **Bootstrap "Flatly" theme** for modern, professional appearance
    * Custom **KPI box component** with color-coding for visual hierarchy
    * **Responsive layout** with 3-column sidebar and 9-column main panel
    * **Tab-based organization** to reduce cognitive load
    * **Collapsible upload panel** - Custom file upload UI only appears when needed, keeping the interface clean
* **Performance Optimizations:**
    * **Reactive expressions** to minimize redundant calculations
    * **Conditional rendering** (e.g., `req()` function) to prevent errors from incomplete inputs
    * Strategic use of `na.rm = TRUE` to handle missing data

---

## Instructions for Running the App

### Prerequisites
* **R Installation** (version 4.0 or higher recommended)

#### Required R Packages:
Install the following packages by running this command in your R console:
```R
install.packages(c(
  "shiny", "bslib", "dplyr", "ggplot2", "DT", 
  "leaflet", "sf", "rnaturalearth", "rnaturalearthdata",
  "rlang", "purrr", "tidyr", "tibble", "plotly"
))
```

## Required Data Files (Optional)

Place the following CSV files in the **same directory** as the R script for default functionality:

* `world_bank_development_indicators.csv`
* `AB_US_2020.csv`
* `AB_US_2023.csv`

> **Note:** The app will run with dummy data if files are missing. You can also upload your own WDI data through the app interface.

---

## Running the Application

1. Open **`FinalProj.R`** in RStudio
2. Click the **"Run App"** button in the top-right corner of the editor
3. The dashboard will launch in a new window or your default browser

---

## Navigating the Dashboard

* Start on the **Overview** tab to understand the datasets.
* **Explore WDI data:**
    * The app loads with default data automatically - start exploring immediately
    * Select countries of interest (try comparing regions like US, China, India)
    * Adjust year range to focus on specific periods
    * Switch between indicators to see different development metrics
    * Use the map view to identify geographic patterns
    * **Upload custom data (optional):**
        * Click "Upload Custom WDI CSV" in the sidebar
        * Select your CSV file (any column structure works)
        * Map your columns to the required variables:
            * **Country Column** - Which column contains country names
            * **Year Column** - Which column contains year values
            * **GDP Column** - (Optional) Which column contains GDP data
            * **Life Expectancy Column** - (Optional) Which column contains life expectancy data
            * **Population Column** - (Optional) Which column contains population data
        * Click "Apply Configuration" to update all visualizations with your custom data
        * The app will automatically update all charts, maps, and tables to use your data
* **Analyze Airbnb trends:**
    * Compare 2020 and 2023 to observe pandemic recovery patterns
    * Filter by city and room type to segment the market
    * Examine the host insights plot to understand listing behavior

---

## Using Custom WDI Data

The dashboard supports uploading your own World Bank or similar development indicator data:

### Step-by-Step Guide:

1. **Prepare Your Data:**
   * Ensure your CSV has columns for country/region names and year values
   * Optional: Include columns for GDP, life expectancy, population, or other indicators
   * Your column names can be anything - you'll map them in the next step

2. **Upload Your File:**
   * Navigate to the "Global Development (WDI)" tab
   * Look for the "Custom File" section in the left sidebar
   * Click "Upload Custom WDI CSV (Optional)" and select your file

3. **Map Your Variables:**
   * After upload, dropdown menus will appear
   * Select which column in your data represents each variable:
     * **Country Column:** Required - identifies countries/regions
     * **Year Column:** Required - identifies time periods
     * **GDP Column:** Optional - select "None" if not applicable. To ensure accurate display in the KPI summary, the values in this column **must be the full numerical amount (e.g., 1,000,000,000)**, not pre-scaled values like '1' if the unit is "billions." The application automatically scales and formats the number into billions (e.g., $1.00B).
     * **Life Expectancy Column:** Optional - select "None" if not applicable
     * **Population Column:** Optional - select "None" if not applicable

4. **Apply Configuration:**
   * Click the "Apply Configuration" button
   * The app will process your data and update all visualizations
   * You'll see a notification confirming successful configuration

5. **Explore Your Data:**
   * All controls (country selector, year range, etc.) will update to reflect your data
   * All visualizations, maps, and tables will use your custom dataset
   * KPI boxes will show metrics from your data (or "N/A" if columns weren't mapped)

### Tips for Custom Data:
* Your data doesn't need to match the default structure exactly
* Map only the variables you have - unmapped variables will show "N/A"
* Ensure year values are numeric (e.g., 2020, 2021) for proper sorting
* The app handles missing values gracefully with `na.rm = TRUE`

---

## Troubleshooting

* **Error: "Package X is not installed"**
    * Run `install.packages("X")` and restart the app
* **Error: "File not found"**
    * Verify CSV files are in the same directory as the R script
    * The app will run with dummy data but with limited functionality
    * Alternatively, upload your own data through the app interface
* **Custom data not displaying properly:**
    * Verify your year column contains numeric values
    * Check that country names match the format in the Natural Earth dataset for map visualization
    * Ensure you clicked "Apply Configuration" after mapping variables
* **Map not displaying properly:**
    * Ensure `rnaturalearth` and `rnaturalearthdata` packages are installed
    * Try running `rnaturalearth::ne_download(returnclass = "sf")` to download map data
* **Performance issues with large datasets:**
    * The app automatically downsamples map points to **1,000** for performance
    * Consider filtering to fewer countries/cities if the interface feels sluggish

---

## Author and Date
* **Author:** Emily Nelson
* **Date:** 12-04-2025