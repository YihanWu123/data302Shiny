#census app 
library(shiny)
library(maps)
library(tidyverse)
library(tools)
source("func_statemap.R")

counties <- sf::read_sf(dsn = "data/county_data.shp")

counties_map <- map_data("county") %>% 
  select(long, lat, group, county = subregion, state = region) %>% 
  left_join(., counties, by = c("county" = "county",  "state" = "state"))

choices <- c(state.name)

# User interface ----
ui <- fluidPage(
  titlePanel("County Demographic Map by State"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      selectInput("area", label = "Select a state to display", 
                  choices = state.name, selected = choices[1]),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White")
    ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  dataInput <- reactive({
    if (input$area %in% state.name) { 
      counties_map %>%
        filter(state == tolower(input$area))
    }
  }) 
  
  genArgs <- reactive({
    
    args <- switch(input$var,
                   "Percent White" = list(dataInput()$white, "darkgreen", "% White"),
                   "Percent Black" = list(dataInput()$black, "black", "% Black"),
                   "Percent Hispanic" = list(dataInput()$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(dataInput()$asian, "darkviolet", "% Asian"))
    args$area_name <- input$area
    args$map_data <- dataInput() 
    args
  })
  
  output$map <- renderPlot({
    if ( input$area %in% state.name ) { do.call(state_map, genArgs()) } 
  })
}

# Run app ----
shinyApp(ui, server)

