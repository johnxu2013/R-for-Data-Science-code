library(leaflet)
leaflet() |>
  setView(174.764, -36.877, zoom = 16) |>
  addTiles() |>
  addMarkers(174.764, -36.877, popup = "Maungawhau")

library(shiny)
textInput("name", "What is your name?")
numericInput("age", "How old are you?", NA, min = 0, max = 150)
