#
# Server of app
#

server <- function(input, output) {
  
  # Number of trips text in UI
  output$num_trips <- renderText({
    "df.geyser  %>%  summarise(media = mean(eruptions))" 
  })
  
}



