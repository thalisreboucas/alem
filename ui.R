#
# Autor :
# Thalis Rebouças
#

# Define UI for application that draws a Algorithm EM 


ui =  shiny::htmlTemplate(
  # Index Page
  "www/index.html" ,
  
 int_eruption =  shiny::textOutput(
    "num_int_eruption",
    inline = T)
  
  )
 