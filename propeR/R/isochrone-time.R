##' Generates an isochrone map from a single origin between a start and end
##' time and date.
##'
##' Generates an isochrone map from a single origin between a start and end
##' time and date, and checks whether destinations fall within isochrone,
##' and if so, at what cutoff time amount.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param startDateAndTime in 'YYYY-MM-DD HH:MM:SS' format
##' @param endDateAndTime in 'YYYY-MM-DD HH:MM:SS' format
##' @param timeIncrease in minutes, default 60
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
##' @param palColor the color palette of the map, defaults to 'Blues'
##' @param mapZoom defaults to 12
##' @return Saves an animated map as a gif and journey details as CSV to output directory
##' @author Michael Hodge
##' @examples
##'   isochroneTime(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00",
##'     endDateAndTime = "2018-08-18 13:00:00",
##'     timeIncrease = 60
##'   )
##' @export
isochroneTime <- function(output.dir,
                          otpcon,
                          originPoints,
                          originPointsRow = 1,
                          destinationPoints,
                          destinationPointsRow = 1,
                          # otpIsochrone args
                          startDateAndTime = "2018-08-18 12:00:00",
                          endDateAndTime = "2018-08-18 15:00:00",
                          timeIncrease = 60,
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
                          # colours
                          palColor = "Blues",
                          # leaflet map args
                          mapZoom = 12) {
  message("Now running the propeR isochroneTime tool.\n")
  
  pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL) # Creating colour palette
  palIsochrone = leaflet::colorFactor(palColor, NULL, n = length(isochroneCutOffs)) # Creating colour palette
  
  dir.create(paste0(output.dir, "/tmp_folder")) # Creates tmp_folder folder for pngs
  
  # Set Origin and Destination if multiple are in csv ----------
  origin_points_row_num <-
    originPointsRow # Set origin using a row from the origin dataframe
  
  if (origin_points_row_num > nrow(originPoints)) {
    message('Row is not in origin file')
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
  time_series = seq(as.POSIXct(startDateAndTime),
                    as.POSIXct(endDateAndTime),
                    by = timeIncrease * 60) # Creates a time series between start and end dates and times based on the increment in time
  destination_points_num_of_cols <-
    ncol(destinationPoints) # Need for later to distinguish original column length
  destination_points_output <-
    destinationPoints # Creates a copy so doesn't overwrite original data, so script can be rerun without loading data again
  destination_points_output[as.character(time_series)] <-
    NA # Creates column for each time_series which we will get journey time for later
  
  message("Creating ",
          length(time_series),
          " isochrones, please wait...")
  time.taken <- list(0)
  
  # SCRIPT
  # Calls otpIsochrone from otp script to get the isochrone from origin for
  # parameters above
  for (i in 1:length(time_series)) {
    # Start loop to calculate journey details
    
    start.time <- Sys.time()
    stamp <-
      format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
    
    date_time_legend <-
      format(time_series[i], "%B %d %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
    time <- format(time_series[i], "%I:%M %p")
    date <- format(time_series[i], "%m/%d/%Y")
    
    # Calls otpIsochrone from otp script to get the isochrone from origin for
    # parameters above
    
    isochrone <- propeR::otpIsochrone(
      otpcon,
      batch = TRUE,
      # If true, goal direction is turned off and a full path tree is built (specify only once)
      from = from_origin$lat_lon,
      to = from_origin$lat_lon,
      # Takes the latitude and longitude from specified origin
      modes = modes,
      date = date,
      # Takes the date as specified above
      time = time,
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
      destination_points_output # Create a spatialdataframe
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
    
    for (n in 1:length(isochroneCutOffs)) {
      time_df_tmp <-
        sp::over(destination_points_spdf, isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
      time_df[, n] <- time_df_tmp[, 2]
    }
    
    for (n in 1:nrow(destination_points_spdf)) {
      # Finds the smallest duration from time_df dataframe
      if (is.na(time_df[n, length(isochroneCutOffs)])) {
        time_df[n, length(isochroneCutOffs) + 1] = NA
        destination_points_output[n, destination_points_num_of_cols + i] = NA
      } else {
        time_df[n, length(isochroneCutOffs) + 1] = min(time_df[n, 1:length(isochroneCutOffs)], na.rm =
                                                         TRUE)
        destination_points_output[n, destination_points_num_of_cols + i] = (time_df[n, length(isochroneCutOffs) +
                                                                                      1]) / 60
      }
    }
    
    names(time_df)[ncol(time_df)] <- "travel_time"
    destinationPoints$travel_time <- time_df$travel_time / 60
    
    destination_points_non_na <-
      subset(destinationPoints, !(is.na(destinationPoints["travel_time"])))
    
    # Creating a leaflet map from results
    library(leaflet)
    m <- leaflet()
    m <- addScaleBar(m)
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    m <- setView(m,
                 lat = from_origin$lat,
                 # Focuses on the origin
                 lng = from_origin$lon,
                 # Focuses on the origin
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
      fillColor = "white",
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
      fillColor = "black",
      stroke = TRUE,
      color = "black",
      opacity = 1,
      weight = 2,
      fillOpacity = 1,
      radius = 10
    )
    m <-
      addLegend(
        m,
        pal = palIsochrone,
        # Adds a legend for the trip
        values = isochroneCutOffs,
        opacity = 0.5,
        title = "Duration (minutes)"
      )
    m <-
      addLegend(
        m,
        pal = pal_time_date,
        # Adds a legend for the time
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
    
    
    mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/", stamp, ".png")) # Saves map in temp folder
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits = 2)
    
    if (i < length(time_series)) {
      message(
        i,
        " out of ",
        length(time_series),
        " isochrones complete. Time taken ",
        do.call(sum, time.taken),
        " seconds. Estimated time left is approx. ",
        (do.call(mean, time.taken) * length(time_series)) - do.call(sum, time.taken),
        " seconds."
      )
    } else {
      message(
        i,
        " out of ",
        length(time_series),
        " isochrones complete. Time taken ",
        do.call(sum, time.taken),
        " seconds."
      )
    }
  }
  
  # Plots leaflet map gif in Viewer and saves to disk, also saves table as csv ----------
  
  message("Analysis complete, now saving outputs to ",
          output.dir,
          ", please wait.\n")
  
  stamp <-
    format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
  
  library(dplyr)
  list.files(
    path = paste0(output.dir, "/tmp_folder"),
    pattern = "*.png",
    full.names = T
  ) %>%  # Creates gif of results
    purrr::map(magick::image_read) %>%  # reads each path file
    magick::image_join() %>% # joins image
    magick::image_animate(fps = 5) %>%  # animates, can opt for number of loops
    magick::image_write(paste0(output.dir, "/isochrone_time-", stamp, ".gif")) # write to current dir
  
  m <-
    magick::image_read(paste0(output.dir, "/isochrone_time-", stamp, ".gif")) %>%
    magick::image_scale("600") # Loads GIF into R
  
  invisible(print(m)) # plots map to Viewer
  
  write.csv(
    destination_points_output,
    file = paste0(output.dir, "/output_isochrone_inc-", stamp, ".csv"),
    row.names = FALSE
  ) # Saves trip details as a CSV
  
  unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
}
