##' Calculates the journey for a single origin and destination.
##'
##' Calculates the journey time and details between a single origin and destination. Outputs (and saves) a map of
##' the polyline between origin and destination, as well as a CSV file of journey details.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
##' @param startDateAndTime in 'YYYY-MM-DD HH:MM:SS' format
##' @param modes defaults to 'TRANSIT, WALK'
##' @param maxWalkDistance in meters, defaults to 1000
##' @param walkReluctance defaults to 2 (range 0 - 20)
##' @param walkSpeed in m/s, defaults to 1.4
##' @param bikeSpeed in m/s, defaults to 4.3
##' @param minTransferTime in minutes, defaults to 0
##' @param maxTransfers defaults to 10
##' @param wheelchair defaults to FALSE
##' @param arriveBy defaults to FALSE
##' @param preWaitTime in minutes, defaults to 60
##' @param transportColours A list defining the colours to assign to each mode of transport.
##' @param mapZoom defaults to 12
##' @return Saves map as a png and journey details as CSV to output directory
##' @author Michael Hodge
##' @examples
##'   pointToPoint(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startdDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
pointToPoint <- function(output.dir,
                         otpcon,
                         originPoints,
                         originPointsRow=1,
                         destinationPoints,
                         destinationPointsRow=1,
                         # otpTime args
                         startDateAndTime="2018-08-18 12:00:00",
                         modes="WALK, TRANSIT",
                         maxWalkDistance=1000,
                         walkReluctance=2,
                         walkSpeed=1.4,
                         bikeSpeed=4.3,
                         minTransferTime=1,
                         maxTransfers=10,
                         wheelchair=F,
                         arriveBy=F,
                         preWaitTime=60,
                         # colours
                         transportColours=list(TRANSIT="#000000", WALK="#A14296", BUS="#48C1B1", RAIL="#4D7BC5", CAR="#8D4084", BICYCLE="#4AA6C3"),
                         # leaflet map args
                         mapZoom=12) {
  
  message("Now running the propeR pointToPoint tool.\n")  
  
  pal_transport <- leaflet::colorFactor(palette=unlist(transportColours, use.names=F), # Creating colour palette
                                        levels=as.factor(names(transportColours)),
                                        reverse=FALSE)
  
  pal_time_date=leaflet::colorFactor(c("#FFFFFF"), domain=NULL) # Creating colour palette
  
  origin_points_row_num <- originPointsRow # Set origin using a row from the origin dataframe 
  destination_points_row_num <- destinationPointsRow # Set destination using a row from the origin dataframe
  
  if (origin_points_row_num > nrow(originPoints)){
    message("Row is not in origin file, process aborted.\n")
    unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder folder is exists
    break
  }
  
  if (destination_points_row_num > nrow(destinationPoints)){
    message("Row is not in destination file, process aborted.\n")
    unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder folder is exists
    break
  }
  
  from_origin <- originPoints[origin_points_row_num,] # Takes the specified row from the data
  to_destination <- destinationPoints[destination_points_row_num,] # Takes the specified row from the data
  
  # Tidying variables ----------
  start_time <- format(as.POSIXct(startDateAndTime), "%I:%M %p") # Sets start time
  start_date <- as.Date(startDateAndTime) # Sets start date
  date_time_legend <- format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
  
  point_to_point <- propeR::otpTripTime(
    otpcon,
    detail = TRUE, # Gives full breakdown of journey if TRUE
    from = from_origin$lat_lon, # Takes the latitude and longitude from specified origin
    to =   to_destination$lat_lon, # Takes the latitude and longitude from specified destination
    modes = modes, 
    date = start_date, # Takes the date as specified above
    time = start_time, # Takes the time as specified above
    maxWalkDistance = maxWalkDistance,
    walkReluctance = walkReluctance,
    walkSpeed = walkSpeed, 
    bikeSpeed = bikeSpeed, 
    minTransferTime = minTransferTime, 
    maxTransfers = maxTransfers, 
    wheelchair = wheelchair, 
    arriveBy = arriveBy, 
    preWaitTime = preWaitTime) # The start time and date
  
  point_to_point_table <- point_to_point$output_table # Outputs to a table
  View(point_to_point_table) # Shows the output table
  
  
  if (!is.null(point_to_point_table)){ # Creates results and maps based on whether journey is found or not
    poly_lines <- point_to_point$poly_lines # The SpatialLinesDataFrame required to create polylines for leaflet
    poly_lines <- sp::spTransform(poly_lines, sp::CRS("+init=epsg:4326"))
    
    popup_poly_lines <- # generates a popup for the poly_lines_lines feature
      paste0("<strong>Mode: </strong>",
             poly_lines$mode,
             "<br><strong>Route: </strong>",
             poly_lines$route,
             "<br><strong>Operator: </strong>",
             poly_lines$agencyName,
             "<br><strong>Duration: </strong>",
             round(poly_lines$duration/60, digits=2), " mins",
             "<br><strong>Distance: </strong>",
             round(poly_lines$duration, digits=2), " meters")
    
    # Creating a leaflet map from results
    library(leaflet)
    m <- leaflet()
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    m <- addScaleBar(m) 
    m <- setView(m, lat=(from_origin$lat+to_destination$lat)/2, # Focuses on midpoint between origin and destination
                 lng=(from_origin$lon+to_destination$lon)/2, # Focuses on midpoint between origin and destination
                 zoom=mapZoom) 
    m <- addAwesomeMarkers(m, data = from_origin, # Adds the origin as a marker
                           lat = ~lat, 
                           lng = ~lon,
                           popup = ~name,
                           icon = makeAwesomeIcon(icon= "hourglass-start", 
                                                  markerColor = "red", 
                                                  iconColor = "white", 
                                                  library = "fa")) 
    m <- addPolylines(m, data = poly_lines, # Adds polylines from origin to destination
                      popup = popup_poly_lines,
                      color = ~pal_transport(poly_lines$mode), 
                      weight = 5,
                      opacity = 1)
    m <- addCircleMarkers(m, data = point_to_point_table, # Adds circles for each stage of the journey
                          lat = ~from_lat, 
                          lng = ~from_lon, 
                          fillColor = ~pal_transport(point_to_point_table$mode),
                          stroke = FALSE, 
                          fillOpacity = 1, 
                          popup = ~mode) 
    m <- addLegend(m, pal = pal_transport, # Adds a legend for the trip
                   values = point_to_point_table$mode, 
                   opacity = 1.0, 
                   title = "Transport Mode") 
    m <- addLegend(m, pal = pal_time_date,
                   opacity = 0.0,
                   values = date_time_legend,
                   position = "bottomleft",
                   title = "Date and Time") 
    m <- addAwesomeMarkers(m, data = to_destination, # Adds the destination as a marker
                           lat = ~lat,
                           lng = ~lon,
                           popup = ~name,
                           icon = makeAwesomeIcon(icon= "hourglass-end", 
                                                  markerColor = "blue", 
                                                  iconColor = "white", 
                                                  library = "fa"))
    
  } else { # If no journey is found
    
    m <- leaflet()
    m <- addTiles(m, group = "leaf") 
    m <- addScaleBar(m) 
    m <- setView(m, lat=(from_origin$lat+to_destination$lat)/2, # Focuses on midpoint between origin and destination
                 lng=(from_origin$lon+to_destination$lon)/2, # Focuses on midpoint between origin and destination
                 zoom=mapZoom) 
    m <- addAwesomeMarkers(m, data = from_origin, # Adds the origin as a marker
                           lat = ~lat, 
                           lng = ~lon,
                           popup = ~name,
                           icon = makeAwesomeIcon(icon= "hourglass-start", 
                                                  markerColor = "blue", 
                                                  iconColor = "white", 
                                                  library = "fa")) 
    m <- addAwesomeMarkers(m, data = to_destination, # Adds the destination as a marker
                           lat = ~lat,
                           lng = ~lon,
                           popup = ~name,
                           icon = makeAwesomeIcon(icon= "hourglass-end", 
                                                  markerColor = "orange", 
                                                  iconColor = "white", 
                                                  library = "fa")) 
    m <- addLegend(m, pal = pal_time_date,
                   opacity = 0.0,
                   values = date_time_legend,
                   position = "bottomleft",
                   title = "Date and Time")
    
  }
  
  # Plots leaflet map in Viewer and saves to disk, also saves table as csv ----------
  
  message("Analysis complete, now saving outputs to ",output.dir,", please wait.\n")  
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
  invisible(print(m)) # plots map to Viewer
  mapview::mapshot(m, file = paste0(output.dir, "/p2p-",stamp,".png")) # Saves map to output directory
  htmlwidgets::saveWidget(m, file = paste0(output.dir, "/p2p-",stamp,".html"), selfcontained = TRUE) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir,"/p2p-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder created by mapshot
  
  write.csv(point_to_point$itineraries[1,], file = paste0(output.dir,"/p2p-",stamp,".csv"),row.names=FALSE) # Saves trip details as a CSV
  
  unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  
  m
}