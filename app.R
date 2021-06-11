library(shiny)
library(shiny.semantic)
library(leaflet)
library(geosphere)
library(dplyr)

# load the data
shipsData <- readRDS("ships.rds")

# set up ships types and minimum and maximum latitude and longitude
shipsTypes <- unique(shipsData["ship_type"])
MIN_LAT <- min(shipsData["LAT"])
MAX_LAT <- max(shipsData["LAT"])
MIN_LON <- min(shipsData["LON"])
MAX_LON <- max(shipsData["LON"])

# Define UI for application that draws a histogram
ui <- semanticPage(
    div(class = "ui raised segment",
        div(
            a(class = "ui green ribbon label", "Search Ship Data by Type and Name"),
            h1(id = "Search Ship Data by Type and Name", "Ship Data Package"),
        )
    ),
    div(
        style = "margin-left: 210px",
        sidebar_layout(
            sidebar_panel(
                h1(selectInput(
                    "shipTypeSelect",
                    label = "Ship Type:",
                    choices = shipsTypes$ship_type,
                    selected = shipsTypes$ship_type[1]
                )
                ),
                h1(uiOutput("selectShipName")),
                h2(textOutput("shipDistanceText")),
                tags$br(),
            ),
            main_panel(
                leafletOutput("map")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # General Data preprocessing function
    general_data_preprocess <- function(df1) {

        ranks <- order(df1$DATETIME)

        nextLat <- dplyr::lead(df1[ranks, "LAT"], n = 1L, default = NA)
        nextLon <- dplyr::lead(df1[ranks, "LON"], n = 1L, default = NA)


        data_list <- list(ranks = ranks, nextLat = nextLat, nextLon = nextLon)
        names(data_list) <- c("ranks", "next_Lat", "next_Lon")
        return(data_list)

    }

    # Function to calculate the shortest distance between two points on an ellipsoid
    calculate_distance <- function(x_lon, x_lat, y_lon, y_lat) {
        geosphere::distGeo(c(x_lon, x_lat), c(y_lon, y_lat))
    }



    values <- reactiveValues()

    shipsFilteredByType <- reactive({
        filteredShips <- shipsData[shipsData[, "ship_type"] == input$shipTypeSelect,]
        values$shipName <- filteredShips[1, "SHIPNAME"]
        filteredShips
    })

    output$selectShipName <- renderUI({
        shipsNames <- unique(shipsFilteredByType()["SHIPNAME"])
        selectInput(
            "shipName",
            label = "Ship Name:",
            choices = shipsNames$SHIPNAME,
            selected = shipsNames$SHIPNAME[1]
        )
    })

    observeEvent(input$shipName, {
        req(input$shipName)
        values$shipName <- input$shipName
    })


    shipDistance <- reactive({
        req(values$shipName, shipsFilteredByType())

        filteredByType <- shipsFilteredByType()
        selectedShipData <- filteredByType[filteredByType[,"SHIPNAME"] == values$shipName,]

        # create parameter list
        parameter_list <- general_data_preprocess(selectedShipData)

        frameWithNextValues <- cbind(selectedShipData[parameter_list$ranks,], next_Lat = parameter_list$next_Lat, next_Lon = parameter_list$next_Lon)
        frameWithoutNA <- frameWithNextValues[0:(nrow(frameWithNextValues)-1),]

        distanceCalculation <- mapply(calculate_distance, frameWithoutNA$LON, frameWithoutNA$LAT, frameWithoutNA$next_Lon, frameWithoutNA$next_Lat)

        observationsIndexes <- which(distanceCalculation == max(distanceCalculation))
        lastIndex <- tail(observationsIndexes, n=1)
        cbind(frameWithoutNA[lastIndex,], distance=distanceCalculation[lastIndex])
    })

    output$map <- renderLeaflet({
        #https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
        addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
            colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
            labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

            return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
        }

        leaflet() %>%
            addTiles() %>%
            fitBounds(MIN_LON, MIN_LAT, MAX_LON, MAX_LAT) %>%
            addLegendCustom(colors = c("orange", "blue"), labels = c("Start", "End"), sizes = c(10, 10))
    })

    output$shipDistanceText <- renderText({
        req(shipDistance())
        paste("Ship distance in meters:", format(round(shipDistance()$distance, 0), nsmall = 0), sep=" ")
    })

    observe({
        shipDistanceVector <- shipDistance()

        leafletProxy('map') %>% # use the proxy to save computation
            clearShapes() %>%
            addCircles(lng=c(shipDistanceVector$LON),
                       lat=c(shipDistanceVector$LAT),
                       group='circles',
                       weight=1, radius=100, color='orange',
                       fillColor='orange',
                       fillOpacity=0.5,
                       opacity=1) %>%
            addCircles(lng=c(shipDistanceVector$next_Lon),
                       lat=c(shipDistanceVector$next_Lat),
                       group='circles',
                       weight=1, radius=100, color='blue',
                       fillColor='blue',
                       fillOpacity=0.5,
                       opacity=1)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
