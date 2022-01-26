#
# Autor :
# Thalis Rebouças
#

# Define UI for application that draws a Algorithm EM 


ui =  shiny::htmlTemplate(
  # Index Page
  "index.html" ,
  # Number of trips
  number_of_trips = textOutput(
    "num_trips",
    inline = T
  )
  
  )
 