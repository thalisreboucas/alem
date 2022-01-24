#
# Autor :
# Thalis Rebouças
#

# Define UI for application that draws a Algorithm EM 

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html" ,
  
  intro = htmlTemplate("www/partial/intro.html" ),
  
  first = htmlTemplate("www/partial/first.html" ), 
  
  second = htmlTemplate("www/partial/second.html") , 
  
  third = htmlTemplate("www/partial/third.html"),
  
  fourth = htmlTemplate("www/partial/fourth.html"),
  
  final = htmlTemplate("www/partial/final.html") 
  
  
  )
