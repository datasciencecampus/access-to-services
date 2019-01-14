##' Calculates the journey for a single origin and destination.
##'
##' Calculates the journey time and details between a single origin and destination. 
##' A CSV file of journey details is saved in the output folder.
##' A map of the journey can also be saved as a PNG image and HTML file.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
##' @param startDateAndTime The start time and date, in 'YYYY-MM-DD HH:MM:SS' format
##' @param modes The mode of the journey, defaults to 'TRANSIT, WALK'
##' @param maxWalkDistance The maximum walking distance, in meters, defaults to 1000 m
##' @param walkReluctance The reluctance of walking-based routes, defaults to 2 (range 0 (lowest) - 20 (highest))
##' @param walkSpeed The walking soeed, in meters per second, defaults to 1.4 m/s
##' @param bikeSpeed The cycling speed, in meters per second, defaults to 4.3 m/s
##' @param minTransferTime The maximum transfer time, in minutes, defaults to 0 mins (no time specified)
##' @param maxTransfers The maximum number of transfers, defaults to 10
##' @param wheelchair If TRUE, uses on wheeelchair friendly stops, defaults to FALSE
##' @param arriveBy Selects whether journey starts at startDateandTime (FALSE) or finishes (TRUE), defaults to FALSE
##' @param preWaitTime The maximum waiting time before a journey cannot be found, in minutes, defaults to 60 mins
##' @param mapOutput Specifies whether you want to output a map, defaults to FALSE
##' @param mapPolylineColours A list defining the colours to assign to each mode of transport.
##' @param mapZoom The zoom level of the map, defaults to 12
##' @param mapPolylineWeight Specifies the weight of the polyline, defaults to 5 px
##' @param mapPolylineOpacity Specifies the opacity of the polyline, defaults to 1 (solid)
##' @param mapMarkerOpacity Specifies the opacity of the marker, defaults to 1 (solid)
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 1 (solid)
##' @param 
##' @return Saves journey details as CSV to output directory (optional: a map in PNG and HTML formats)
##' @author Michael Hodge
##' @examples
##'   pointToPoint(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
pointToPoint <- function(output.dir,
                         otpcon,
                         originPoints,
                         originPointsRow = 1,
                         destinationPoints,
                         destinationPointsRow = 1,
                         # otpTime args
                         startDateAndTime = "2018-08-18 12:00:00",
                         modes = "WALK, TRANSIT",
                         maxWalkDistance = 1000,
                         walkReluctance = 2,
                         walkSpeed = 1.4,
                         bikeSpeed = 4.3,
                         minTransferTime = 1,
                         maxTransfers = 10,
                         wheelchair = F,
                         arriveBy = F,
                         preWaitTime = 60,
                         # leaflet map args
                         mapOutput = FALSE,
                         mapPolylineColours = list(
                           TRANSIT = "#000000",
                           WALK = "#A14296",
                           BUS = "#48C1B1",
                           RAIL = "#4D7BC5",
                           CAR = "#E825D6",
                           BICYCLE = "#4AA6C3"
                         ),
                         mapZoom = 12,
                         mapPolylineWeight = 5,
                         mapPolylineOpacity = 1,
                         mapMarkerOpacity = 1,
                         mapLegendOpacity = 1) {
  message("Now running the propeR pointToPoint tool.\n")
  
  if (mapOutput == TRUE) {
    library(leaflet)
    pal_transport <-
      leaflet::colorFactor(
        palette = unlist(mapPolylineColours, use.names = F),
        levels = as.factor(names(mapPolylineColours)),
        reverse = FALSE
      )
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL)
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <-
    originPointsRow
  
  destination_points_row_num <-
    destinationPointsRow
  
  if (origin_points_row_num > nrow(originPoints)) {
    message("Row is not in origin file, process aborted.\n")
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE)
    break #todo: need a better system to stop code as break shouldn't be used outside a loop
  }
  
  if (destination_points_row_num > nrow(destinationPoints)) {
    message("Row is not in destination file, process aborted.\n")
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE)
    break #todo: need a better system to stop code as break shouldn't be used outside a loop
  }
  
  from_origin <-
    originPoints[origin_points_row_num,]
  
  to_destination <-
    destinationPoints[destination_points_row_num,]
  
  start_time <-
    format(as.POSIXct(startDateAndTime), "%I:%M %p")
  
  start_date <- as.Date(startDateAndTime)
  
  date_time_legend <-
    format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M")
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
  point_to_point <- propeR::otpTripTime(
    otpcon,
    detail = TRUE,
    from = from_origin$lat_lon,
    to =   to_destination$lat_lon,
    modes = modes,
    date = start_date,
    time = start_time,
    maxWalkDistance = maxWalkDistance,
    walkReluctance = walkReluctance,
    walkSpeed = walkSpeed,
    bikeSpeed = bikeSpeed,
    minTransferTime = minTransferTime,
    maxTransfers = maxTransfers,
    wheelchair = wheelchair,
    arriveBy = arriveBy,
    preWaitTime = preWaitTime
  )
  
  if (point_to_point$errorId == 'OK') {
    point_to_point_table_overview <-
      point_to_point$itineraries[1,]
    
    point_to_point_table_overview$origin <-
      from_origin$name
    
    point_to_point_table_overview$destination <-
      to_destination$name
    
    point_to_point_table_overview$distance <-
      round((sum(
        point_to_point$trip_details$distance
      )) / 1000, digits = 2)
    
    point_to_point_table_overview <-
      point_to_point_table_overview[, c(8, 9, 1, 2, 10, 3, 4, 5, 6, 7)]
    
    colnames(point_to_point_table_overview) <-
      c(
        "origin",
        "destination",
        "start_time",
        "end_time",
        "distance_km",
        "duration_mins",
        "walk_time_mins",
        "transit_time_mins",
        "waiting_time_mins",
        "transfers"
      )
    
    point_to_point_table <-
      point_to_point$output_table
    
    #########################
    #### OPTIONAL EXTRAS ####
    #########################
    
    if (mapOutput == TRUE) {
      message("Generating map, please wait.\n")
      
      poly_lines <-
        point_to_point$poly_lines # The SpatialLinesDataFrame required to create polylines for leaflet
      poly_lines <-
        sp::spTransform(poly_lines, sp::CRS("+init=epsg:4326"))
      
      popup_poly_lines <-
        paste0(
          "<strong>Mode: </strong>",
          poly_lines$mode,
          "<br><strong>Route: </strong>",
          poly_lines$route,
          "<br><strong>Operator: </strong>",
          poly_lines$agencyName,
          "<br><strong>Duration: </strong>",
          round(poly_lines$duration / 60, digits = 2),
          " mins",
          "<br><strong>Distance: </strong>",
          round(poly_lines$duration, digits = 2),
          " meters"
        )
      
      m <- leaflet()
      m <-
        addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
      m <- addScaleBar(m)
      m <-
        setView(
          m,
          lat = (from_origin$lat + to_destination$lat) / 2,
          # Focuses on midpoint between origin and destination
          lng = (from_origin$lon + to_destination$lon) / 2,
          # Focuses on midpoint between origin and destination
          zoom = mapZoom
        )
      m <-
        addAwesomeMarkers(
          m,
          data = from_origin,
          # Adds the origin as a marker
          lat = ~ lat,
          lng = ~ lon,
          popup = ~ name,
          icon = makeAwesomeIcon(
            icon = "hourglass-start",
            markerColor = "red",
            iconColor = "white",
            library = "fa"
          )
        )
      m <-
        addPolylines(
          m,
          data = poly_lines,
          # Adds polylines from origin to destination
          popup = popup_poly_lines,
          color = ~ pal_transport(poly_lines$mode),
          weight = mapPolylineWeight,
          opacity = mapPolylineOpacity
        )
      m <-
        addCircleMarkers(
          m,
          data = point_to_point_table,
          # Adds circles for each stage of the journey
          lat = ~ from_lat,
          lng = ~ from_lon,
          fillColor = ~ pal_transport(point_to_point_table$mode),
          stroke = FALSE,
          fillOpacity = mapMarkerOpacity,
          popup = ~ mode
        )
      m <-
        addLegend(
          m,
          pal = pal_transport,
          # Adds a legend for the trip
          values = point_to_point_table$mode,
          opacity = mapLegendOpacity,
          title = "Transport Mode"
        )
      m <- addLegend(
        m,
        pal = pal_time_date,
        opacity = mapLegendOpacity,
        values = date_time_legend,
        position = "bottomleft",
        title = "Date and Time"
      )
      m <-
        addAwesomeMarkers(
          m,
          data = to_destination,
          # Adds the destination as a marker
          lat = ~ lat,
          lng = ~ lon,
          popup = ~ name,
          icon = makeAwesomeIcon(
            icon = "hourglass-end",
            markerColor = "blue",
            iconColor = "white",
            library = "fa"
          )
        )
    }
    
    ######################
    #### SAVE RESULTS ####
    ######################
    
    message("Analysis complete, now saving outputs to ",
            output.dir,
            ", please wait.\n")
    
    stamp <-
      format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
    
    write.csv(
      point_to_point_table_overview,
      file = paste0(output.dir, "/p2p-", stamp, ".csv"),
      row.names = FALSE
    ) # Saves trip details as a CSV
    
    write.csv(
      point_to_point_table,
      file = paste0(output.dir, "/p2p-journey-legs", stamp, ".csv"),
      row.names = FALSE
    ) # Saves journey leg details as a CSV
    
    if (mmapOutputap == TRUE) {
      invisible(print(m)) # plots map to Viewer
      mapview::mapshot(m, file = paste0(output.dir, "/p2p-", stamp, ".png")) # Saves map to output directory
      htmlwidgets::saveWidget(
        m,
        file = paste0(output.dir, "/p2p-", stamp, ".html"),
        selfcontained = TRUE
      ) # Saves as an interactive HTML webpage
      unlink(paste0(output.dir, "/p2p-", stamp, "_files"), recursive = TRUE) # Deletes temporary folder created by mapshot
      unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    }
    
  } else {
    message("No journey found, cannot provide any outputs!\n")
  }
  message("Thanks for using propeR.")
}