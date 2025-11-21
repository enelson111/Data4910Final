library(shiny)
library(bslib)

ui <- navbarPage(
  title = "Dashboard",
  theme = bs_theme(bootswatch = "flatly"), 
  
  #overview page
  tabPanel(
    "Overview",
    fluidPage(
      h2("planning to have overview of project here"), 
      br()
    )
  ),
  
  #dataset 1
  tabPanel(
    "Dataset 1",
    sidebarLayout(
      sidebarPanel(
        h3("Sidebar"),
        p("planning to have dataset description and user inputs here"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Overview", h4("Tab 1 content here")),
          tabPanel("Table View", h4("Tab 2 content here")),
          tabPanel("Map View", h4("Tab 3 content here")),
          tabPanel("Tab 4", h4("Tab 4 content here"))
        ),
        width = 9
      )
    )
  ),
  
  #dataset 2
  tabPanel(
    "Dataset 2",
    sidebarLayout(
      sidebarPanel(
        h3("Sidebar"),
        p("planning to have dataset description and user inputs here"), 
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Overview", h4("Tab 1 content here")),
          tabPanel("Table View", h4("Tab 2 content here")),
          tabPanel("Map View", h4("Tab 3 content here")),
          tabPanel("Tab 4", h4("Tab 4 content here"))
        ),
        width = 9
      )
    )
  ),
  
  #dataset 3
  tabPanel(
    "Dataset 3",
    sidebarLayout(
      sidebarPanel(
        h3("Sidebar"),
        p("planning to have dataset description and user inputs here"), 
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Overview", h4("Tab 1 content here")),
          tabPanel("Table View", h4("Tab 2 content here")),
          tabPanel("Map View", h4("Tab 3 content here")),
          tabPanel("Tab 4", h4("Tab 4 content here"))
        ),
        width = 9
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
