# Material card -----------------------------------------------------------

material_card <- function(..., header = NULL, bgcolor = "white") {
  div(
    class = "card",
    header, 
    div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
  )
}