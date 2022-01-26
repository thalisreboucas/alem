#
# Server of app
#

server <- function(input, output) {
  
  # Number of kilometers in UI
  output$num_int_eruption <- renderText({
   print(mix_erupt$interactions)
  })
  
}

shinyApp(ui,server)

