library(shiny)
library(shiny.semantic)
library(leaflet)
library(geosphere)
library(dplyr)

# load the data
ships_data <- readRDS("ships.rds")

# set up ships types and minimum and maximum latitude and longitude
ships_types <- unique(ships_data["ship_type"])
min_lat <- min(ships_data["LAT"])
max_lat <- max(ships_data["LAT"])
min_lon <- min(ships_data["LON"])
max_lon <- max(ships_data["LON"])

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
                    "ship_type_select",
                    label = "Ship Type:",
                    choices = ships_types$ship_type,
                    selected = ships_types$ship_type[1]
                )
                ),
                h1(uiOutput("select_ship_name")),
                h2(textOutput("ship_distance_text")),
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

        next_lat <- dplyr::lead(df1[ranks, "LAT"], n = 1L, default = NA)
        next_lon <- dplyr::lead(df1[ranks, "LON"], n = 1L, default = NA)


        data_list <- list(ranks = ranks, next_lat = next_lat, next_lon = next_lon)
        return(data_list)

    }

    # Function to calculate the shortest distance between two points on an ellipsoid
    calculate_distance <- function(x_lon, x_lat, y_lon, y_lat) {
        geosphere::distGeo(c(x_lon, x_lat), c(y_lon, y_lat))
    }



    values <- reactiveValues()

    ships_filtered_by_type <- reactive({
        filtered_ships <- ships_data[ships_data[, "ship_type"] == input$ship_type_select,]
        values$ship_name <- filtered_ships[1, "SHIPNAME"]
        filtered_ships
    })

    output$select_ship_name <- renderUI({
        ships_names <- unique(ships_filtered_by_type()["SHIPNAME"])
        selectInput(
            "ship_name",
            label = "Ship Name:",
            choices = ships_names$SHIPNAME,
            selected = ships_names$SHIPNAME[1]
        )
    })

    observeEvent(input$ship_name, {
        req(input$ship_name)
        values$ship_name <- input$ship_name
    })


    ship_distance <- reactive({
        req(values$ship_name, ships_filtered_by_type())

        filtered_by_type <- ships_filtered_by_type()
        selected_ship_data <- filtered_by_type[filtered_by_type[,"SHIPNAME"] == values$ship_name,]

        # create parameter list
        parameter_list <- general_data_preprocess(selected_ship_data)

        frame_with_next_values <- cbind(selected_ship_data[parameter_list$ranks,], next_lat = parameter_list$next_lat, next_lon = parameter_list$next_lon)
        frame_without_na <- frame_with_next_values[0:(nrow(frame_with_next_values)-1),]

        distance_calculation <- mapply(calculate_distance, frame_without_na$LON, frame_without_na$LAT, frame_without_na$next_lon, frame_without_na$next_lat)

        observations_indexes <- which(distance_calculation == max(distance_calculation))
        last_index <- tail(observations_indexes, n=1)
        cbind(frame_without_na[last_index,], distance=distance_calculation[last_index])
    })

    output$map <- renderLeaflet({
        #https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
        addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
            colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
            labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

            return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
        }

        leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::fitBounds(min_lon, min_lat, max_lon, max_lat) %>%
            addLegendCustom(colors = c("orange", "blue"), labels = c("Start", "End"), sizes = c(10, 10))
    })

    output$ship_distance_text <- renderText({
        req(ship_distance())
        paste("Ship distance in meters:", format(round(ship_distance()$distance, 0), nsmall = 0), sep=" ")
    })

    observe({
        ship_distance_vector <- ship_distance()

        leaflet::leafletProxy('map') %>% # use the proxy to save computation
            leaflet::clearShapes() %>%
            leaflet::addCircles(lng=c(ship_distance_vector$LON),
                       lat=c(ship_distance_vector$LAT),
                       group='circles',
                       weight=1, radius=100, color='orange',
                       fillColor='orange',
                       fillOpacity=0.5,
                       opacity=1) %>%
            leaflet::addCircles(lng=c(ship_distance_vector$next_lon),
                       lat=c(ship_distance_vector$next_lat),
                       group='circles',
                       weight=1, radius=100, color='blue',
                       fillColor='blue',
                       fillOpacity=0.5,
                       opacity=1)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
