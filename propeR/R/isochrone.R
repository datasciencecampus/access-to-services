##' Generates an isochrone map from a single origin
##'
##' Generates an isochrone map from a single origin and checks whether destinations
##' fall within isochrone, and if so, at what cutoff time amount.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param startDateAndTime in 'YYYY-MM-DD HH:MM:SS' format
##' @param modes defaults to 'TRANSIT, WALK'
##' @param maxWalkDistance in meters, defaults to 1000
##' @param walkReluctance defaults to 2 (range 0 - 20)
##' @param walkSpeed in m/s, defaults to 1.4
##' @param bikeSpeed in m/s, defaults to 4.3
##' @param minTransferTime in minutes, defaults to 1
##' @param maxTransfers defaults to 10
##' @param wheelchair defaults to FALSE
##' @param arriveBy defaults to FALSE
##' @param isochroneCutOffs a list of cutoffs in minutes, defaults to c(30, 60, 90)
##' @param map specify whether you want to output a map
##' @param palColor the color palette of the map, defaults to 'Blues'
##' @param mapZoom defaults to 12
##' @return Saves map as a png and journey details as CSV to output directory
##' @author Michael Hodge
##' @examples
##'   isochrone(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
isochrone <- function(output.dir,
                      otpcon,
                      originPoints,
                      originPointsRow = 1,
                      destinationPoints,
                      destinationPointsRow = 1,
                      # otpIsochrone args
                      startDateAndTime = "2018-08-18 12:00:00",
                      modes = "WALK, TRANSIT",
                      maxWalkDistance = 1000,
                      walkReluctance = 2,
                      walkSpeed = 1.5,
                      bikeSpeed = 5,
                      minTransferTime = 1,
                      maxTransfers = 5,
                      wheelchair = F,
                      arriveBy = F,
                      # function specific args.
                      isochroneCutOffs = c(30, 60, 90),
                      # leaflet map args
                      map = FALSE,
                      palColor = "Blues",
                      mapZoom = 12) {
  message("Now running the propeR isochrone tool.\n")
  
  if (map == TRUE) {
    library(leaflet)
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL) # Creating colour palette
    palIsochrone = leaflet::colorFactor(palColor, NULL, n = length(isochroneCutOffs)) # Creating colour palette
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <-
    originPointsRow # Set origin using a row from the origin dataframe
  
  destination_points_row_num <-
    destinationPointsRow # Set destination using a row from the destination dataframe
  
  if (origin_points_row_num > nrow(originPoints)) {
    message('Row is not in origin file, process aborted.\n')
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break
  }
  
  from_origin <-
    originPoints[origin_points_row_num, ] # Takes the specified row from the data
  
  # Tidying variables ----------
  start_time <-
    format(as.POSIXct(startDateAndTime), "%I:%M %p") # Sets start time
  start_date <- as.Date(startDateAndTime) # Sets start date
  date_time_legend <-
    format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
  isochrone <- propeR::otpIsochrone(
    otpcon,
    batch = TRUE,
    # If true, goal direction is turned off and a full path tree is built (specify only once)
    from = from_origin$lat_lon,
    to = from_origin$lat_lon,
    # Takes the latitude and longitude from specified origin
    modes = modes,
    date = start_date,
    # Takes the date as specified above
    time = start_time,
    # Takes the time as specified above
    maxWalkDistance = maxWalkDistance,
    walkReluctance = walkReluctance,
    walkSpeed = walkSpeed,
    bikeSpeed = bikeSpeed,
    minTransferTime = minTransferTime,
    maxTransfers = maxTransfers,
    wheelchair = wheelchair,
    arriveBy = arriveBy,
    cutoff = isochroneCutOffs
  )
  
  isochrone_polygons <-
    rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE) # Converts the polygons to be handled in leaflet
  
  destination_points_spdf <-
    destinationPoints # Create a spatialdataframe
  sp::coordinates(destination_points_spdf) <-
    ~ lon + lat # Take lat and lon for coordinates
  sp::proj4string(destination_points_spdf) <-
    sp::proj4string(isochrone_polygons) # Take projection
  isochrone_polygons_split <-
    sp::split(isochrone_polygons, isochrone_polygons@data$time) # Split into cutoff times
  time_df <-
    data.frame(matrix(
      ,
      ncol = length(isochroneCutOffs),
      nrow = nrow(destination_points_spdf)
    )) # Create time dataframe
  
  for (i in 1:length(isochroneCutOffs)) {
    time_df_tmp <-
      sp::over(destination_points_spdf, isochrone_polygons_split[[i]]) # Finds the polygon the destination point falls within
    time_df[, i] <- time_df_tmp[, 2]
  }
  
  for (i in 1:nrow(destination_points_spdf)) {
    # Finds the smallest duration from time_df dataframe
    if (is.na(time_df[i, length(isochroneCutOffs)])) {
      time_df[i, length(isochroneCutOffs) + 1] = NA
    } else {
      time_df[i, length(isochroneCutOffs) + 1] = min(time_df[i, 1:length(isochroneCutOffs)], na.rm =
                                                       TRUE)
    }
  }
  
  names(time_df)[ncol(time_df)] <- "travel_time"
  destinationPoints$travel_time <- time_df$travel_time / 60
  
  message(
    "The number of destinations that are within the maximum travel time is ",
    sum(!is.na(destinationPoints$travel_time)),
    "/",
    nrow(destinationPoints),
    ", or ",
    (sum(
      !is.na(destinationPoints$travel_time)
    ) / nrow(destinationPoints)) * 100,
    "%\n"
  )
  
  destination_points_non_na <-
    subset(destinationPoints, !(is.na(destinationPoints["travel_time"])))
  
  #########################
  #### OPTIONAL EXTRAS ####
  #########################
  
  if (map == TRUE) {
    message("Generating map, please wait.")
    
    library(leaflet)
    m <- leaflet()
    m <- addScaleBar(m)
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    m <- setView(m,
                 lat = from_origin$lat,
                 lng = from_origin$lon,
                 zoom = mapZoom)
    m <- addPolygons(
      m,
      data = isochrone_polygons,
      stroke = TRUE,
      color = palIsochrone(rev(isochroneCutOffs)),
      opacity = 1,
      weight = 5,
      dashArray = 2,
      smoothFactor = 0.3,
      fillOpacity = 0.6,
      fillColor = palIsochrone(rev(isochroneCutOffs))
    )
    m <- addCircleMarkers(
      m,
      data = destinationPoints,
      lat = ~ lat,
      lng = ~ lon,
      popup = ~ name,
      fillColor = "black",
      stroke = TRUE,
      color = "black",
      opacity = 1,
      weight = 2,
      fillOpacity = 1,
      radius = 10
    )
    m <- addCircleMarkers(
      m,
      data = destination_points_non_na,
      lat = ~ lat,
      lng = ~ lon,
      popup = ~ name,
      fillColor = "white",
      stroke = TRUE,
      color = "black",
      opacity = 1,
      weight = 2,
      fillOpacity = 1,
      radius = 10
    )
    m <- addLegend(
      m,
      pal = palIsochrone,
      values = isochroneCutOffs,
      opacity = 0.5,
      title = "Duration (minutes)"
    )
    m <- addLegend(
      m,
      pal = pal_time_date,
      opacity = 0.0,
      values = date_time_legend,
      position = "bottomleft",
      title = "Date and Time"
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
    destinationPoints,
    file = paste0(output.dir, "/isochrone-", stamp, ".csv"),
    row.names = FALSE
  ) # Saves trip details as a CSV
  
  if (map == TRUE) {
    invisible(print(m)) # plots map to Viewer
    mapview::mapshot(m, file = paste0(output.dir, "/isochrone-", stamp, ".png")) # Saves map to output directory
    htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochrone-", stamp, ".html")) # Saves as an interactive HTML webpage
    unlink(paste0(output.dir, "/isochrone-", stamp, "_files"),
           recursive = TRUE) # Deletes temporary folder created by mapshot
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    
    rgdal::writeOGR(
      isochrone_polygons,
      dsn = paste0(output.dir,
                   "/isochrone",
                   stamp,
                   ".geoJSON"),
      layer = "isochrone_polygons",
      driver = "GeoJSON"
    )
  }
}
