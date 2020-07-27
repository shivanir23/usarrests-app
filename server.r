# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map<- renderPlot({
    
    
    if(as.numeric(input$var) == 2)
    {arrests <- USArrests 
    arrests$region <- tolower(rownames(USArrests))
    head(arrests)
    # Retrieve the states map data and merge with crime data
    states_map <- map_data("state")
    arrests_map <- left_join(states_map, arrests, by = "region")
    
    # Create the map
    ggplot(arrests_map, aes(long, lat, group = group))+
      geom_polygon(aes(fill = Assault), color = "white")+
      scale_fill_viridis_c(option = "B")+labs(main="Assault Rate in the United States in 1973",x="Longitude",y="Latitude")
    }
    else if(as.numeric(input$var) == 3)
    {arrests <- USArrests 
    arrests$region <- tolower(rownames(USArrests))
    head(arrests)
    # Retrieve the states map data and merge with crime data
    states_map <- map_data("state")
    arrests_map <- left_join(states_map, arrests, by = "region")
    
    # Create the map
    ggplot(arrests_map, aes(long, lat, group = group))+
      geom_polygon(aes(fill = Rape), color = "white")+
      scale_fill_viridis_c(option = "C")+labs(main="Rape Rate in the United States in 1973",x="Longitude",y="Latitude")}
    
    else if(as.numeric(input$var) == 1)
    {arrests <- USArrests 
    arrests$region <- tolower(rownames(USArrests))
    head(arrests)
    # Retrieve the states map data and merge with crime data
    states_map <- map_data("state")
    arrests_map <- left_join(states_map, arrests, by = "region")
    
    # Create the map
    ggplot(arrests_map, aes(long, lat, group = group))+
      geom_polygon(aes(fill = Murder), color = "white")+
      scale_fill_viridis_c(option = "A")+labs(main="Murder Rate in the United States in 1973",x="Longitude",y="Latitude")
    }
  })
  
  
  output$plot<- renderPlot({
    
    if(as.numeric(input$var) == 1)
    {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
    hist(USArrests$Murder, las = 2, ylab = "Murder Rate per 100,000", 
         main = "Murder Rate in the United States in 1973",col = viridis(5))}
    if(as.numeric(input$var) == 2)
    {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
    hist(USArrests$Assault, las = 2, ylab = "Assault Rate per 100,000", 
         main = "Assault Rate in the United States in 1973",col = viridis(8))}
    if(as.numeric(input$var) == 3)
    {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
    hist(USArrests$Rape, las = 2, ylab = "Rape Rate per 100,000", 
         main = "Rape Rate in the United States in 1973",col = viridis(10))}
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
