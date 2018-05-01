library(shiny)
library(leaflet)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

source("model.R")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Shiny optimized routing"),

  p("This is just a tutorial to show how to build a quick route optimization shiny app"),
  p("The app finds the optimal route visiting all markers - usually called the traveling salesman problem."),
  p("Click on the map to place markers. Click on a marker to remove that marker"),
  hr(),
  leafletOutput("leafletmap")
)

server <- function(input, output, session) {

  # this will hold all our markers
  markers <- reactiveVal(data.frame(
    lat = numeric(),
    lng = numeric(),
    id = integer()
  ))

  # init the map
  output$leafletmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(13.376915, 52.516224, zoom = 5)
  })

  # whenever someone clicks on a marker, remove it from the markers DF
  observeEvent(input$leafletmap_marker_click, {
    e <- input$leafletmap_marker_click
    old_markers <- markers()
    old_markers <- filter(old_markers,
                          abs(lat - e$lat) > 0.000001,
                          abs(lng - e$lng) > 0.000001)
    markers(old_markers)
  })

  # whenever someone clicks on the map, add a marker
  observeEvent(input$leafletmap_click, {
    e <- input$leafletmap_click
    old_markers <- markers()
    old_markers <- rbind(old_markers, data.frame(
      lat = e$lat,
      lng = e$lng,
      id = 42L
    ))
    old_markers$id <- 1L:nrow(old_markers)
    markers(old_markers)
  })

  # when markers change, redraw the map
  observe({
    markers_data <- markers()
    leafletProxy("leafletmap", session) %>%
      clearMarkers() %>%
      addMarkers(lng = markers_data$lng, lat = markers_data$lat)
  })

  # when markers change, recalculate the distance matrix
  # replace this with a proper distance matrix based on streetlevel distance
  # usually this requires a server
  distance_matrix <- reactive({
    markers_data <- markers()
    req(nrow(markers_data) > 2L)
    round(geosphere::distm(matrix(c(markers_data$lng, markers_data$lat), ncol = 2L)))
  })

  # build the MIP model
  # based on the Miller–Tucker–Zemlin (MTZ) formulation
  model <- reactive({
    markers_data <- markers()
    dmatrix <- distance_matrix()
    build_model(n = nrow(markers_data), dist_fun = function(i, j) {
      vapply(seq_along(i), function(k) dmatrix[i[k], j[k]], numeric(1L))
    })
  })

  # solve the model with GLPK
  solution <- reactive({
    solve_model(model(), with_ROI("glpk", verbose = TRUE, presolve = TRUE))
  })

  # extract the solution
  # one line per tuple of coordinates
  legs <- reactive({
    markers_data <- markers()
    get_solution(solution(), x[i, j]) %>%
      filter(value > 0.9) %>%
      left_join(markers_data, by = c("i" = "id")) %>%
      left_join(markers_data, by = c("j" = "id"))
  })

  # draw some lines
  observe({
    lines <- legs()
    req(nrow(lines) > 2L) # at least three lines

    proxy <- leafletProxy("leafletmap", session)
    clearShapes(proxy)
    for(i in seq_len(nrow(lines))) {
      line <- lines[i, ]
      data <- data.frame(
        lng = c(line$lng.x, line$lng.y),
        lat = c(line$lat.x, line$lat.y))
      addPolylines(proxy, data = data,
                   lng = ~lng, lat = ~lat)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
