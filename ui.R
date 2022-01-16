#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html" ,
  
  intro = htmlTemplate("www/partial/intro.html" ),
  
  first = htmlTemplate("www/partial/first.html" ), 
  
  second = htmlTemplate("www/partial/second.html") , 
  
  third = htmlTemplate("www/partial/third.html"),
  
  final = htmlTemplate("www/partial/final.html")
  
  )
