library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(maps)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("US Arrests")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Data Analysis with information from the 1974 US Arrests data."),
      selectInput("var",
                  label = strong("Crime list"),
                  choices = c("Murder" = 1, "Assault" = 2,"Rape" = 3),
                  selected = 1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(h3("Data Analysis"),
              tabsetPanel(type="tabs",
                          tabPanel("map", plotOutput("map")),
                          tabPanel("plot", plotOutput("plot"))
                          
              ))
  )
)