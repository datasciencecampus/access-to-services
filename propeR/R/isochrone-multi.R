##' Generates an isochrone map for multiple origins.
##'
##' Generates an isochrone map from multiple origins and checks whether destinations
##' fall within isochrones, and if so, at what cutoff time amount.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
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
##' @param palColor the color palette of the map, defaults to 'Blues'
##' @param mapZoom defaults to 12
##' @return Saves map as a png and destination results as CSV to output directory
##' @author Michael Hodge
##' @examples
##'   isochroneMulti(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
isochroneMulti <- function(output.dir,
                           otpcon,
                           originPoints,
                           destinationPoints,
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
                           # colours
                           palColor = "Blues",
                           # leaflet map args
                           mapZoom = 12) {
  message("Now running the propeR isochroneMulti tool.\n")
  
  palIsochrone = leaflet::colorFactor(palColor, NULL, n = length(isochroneCutOffs)) # Creating colour palette
  
  unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  
  # Tidying variables ----------
  start_time <-
    format(as.POSIXct(startDateAndTime), "%I:%M %p") # Sets start time
  start_date <- as.Date(startDateAndTime) # Sets start date
  date_time_legend <-
    format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
  
  rownames(destinationPoints) <- NULL
  destination_points_spdf <-
    destinationPoints # Create a spatialdataframe
  sp::coordinates(destination_points_spdf) <-
    ~ lon + lat # Take lat and lon for coordinates
  
  # SCRIPT
  # Calls otpIsochrone from otp script to get the isochrone from origin for
  # parameters above
  
  message("Creating ", nrow(originPoints), " isochrones, please wait...")
  time.taken <- list(0)
  
  start_num <- 1
  end_num <- nrow(originPoints)
  
  originPoints_removed <- c()
  originPoints_removed_list <- c()
  
  for (i in start_num:end_num) {
    # Start loop to calculate journey details
    
    start.time <- Sys.time()
    
    from_origin <- originPoints[i, ]
    
    # Calls otpIsochrone from otp script to get the isochrone from origin for
    # parameters above
    
    isochrone <- propeR::otpIsochrone(
      otpcon,
      batch = TRUE,
      # If true, goal direction is turned off and a full path tree is built (specify only once)
      from = from_origin$lat_lon,
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
    
    options(warn = -1)
    
    if (i == start_num) {
      # Combines outputs
      t <-
        try(isochrone_polygons <-
              rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE))
      # Converts the polygons to be handled in leaflet
      if ("try-error" %in% class(t)) {
        originPoints_removed <- c(originPoints_removed, originPoints$name[i])
        originPoints_removed_list <- c(originPoints_removed_list, i)
        message(
          "Removed ",
          originPoints$name[i],
          " from analysis as no polygon could be generated from it."
        )
        next
        
      } else {
        isochrone_polygons <-
          rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE)
        
        isochrone_polygons@data$name <-
          from_origin$name # adds name to json
        sp::proj4string(destination_points_spdf) <-
          sp::proj4string(isochrone_polygons) # Take projection
        time_df_tmp <-
          data.frame(matrix(
            ,
            ncol = nrow(destinationPoints),
            nrow = 1
          )) # Create time dataframe
        isochrone_polygons_split <-
          sp::split(isochrone_polygons, isochrone_polygons@data$time) # Split into names
        for (n in 1:length(isochroneCutOffs)) {
          if ("try-error" %in% class(t)) {
            time_df_tmp[n, ] <- Inf
            message("Some polygons could not be generated, A cutoff level may be too small for ",
                    originPoints$name[i],
                    ".")
            next
            
          } else {
            time_df_tmp2 <-
              sp::over(destination_points_spdf,
                       isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
            remove(time_df_tmp2)
          }
        }
        options(warn = -1)
        for (n in 1:ncol(time_df_tmp)) {
          time_df_tmp[length(isochroneCutOffs) + 1, n] <-
            min(time_df_tmp[1:length(isochroneCutOffs), n], na.rm = TRUE)
        }
        options(warn = 0)
        rownames(time_df_tmp)[length(isochroneCutOffs) + 1] <-
          originPoints$name[i]
        time_df <- time_df_tmp[length(isochroneCutOffs) + 1, ]
      }
    } else {
      t <-
        try(isochrone_polygons_tmp <-
              rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE))
      # Converts the polygons to be handled in leaflet
      if ("try-error" %in% class(t)) {
        originPoints_removed <- c(originPoints_removed, originPoints$name[i])
        originPoints_removed_list <- c(originPoints_removed_list, i)
        message(
          "Removed ",
          originPoints$name[i],
          " from analysis as no polygon could be generated from it."
        )
        next
        
      } else {
        isochrone_polygons_tmp <-
          rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE)
        
        isochrone_polygons_tmp@data$name <-
          from_origin$name # adds name to json
        
        if (exists("isochrone_polygons")) {
          isochrone_polygons <-
            rbind(isochrone_polygons, isochrone_polygons_tmp)
        } else {
          isochrone_polygons <- isochrone_polygons_tmp
        }
        
        sp::proj4string(destination_points_spdf) <-
          sp::proj4string(isochrone_polygons_tmp) # Take projection
        time_df_tmp <-
          data.frame(matrix(
            ,
            ncol = nrow(destinationPoints),
            nrow = 1
          )) # Create time dataframe
        isochrone_polygons_split <-
          sp::split(isochrone_polygons_tmp,
                    isochrone_polygons_tmp@data$time) # Split into names
        for (n in 1:length(isochroneCutOffs)) {
          t <-
            try(time_df_tmp2 <-
                  sp::over(destination_points_spdf,
                           isochrone_polygons_split[[n]]))
          # Converts the polygons to be handled in leaflet
          if ("try-error" %in% class(t)) {
            time_df_tmp[n, ] <- Inf
            message("Some polygons could not be generated, A cutoff level may be too small for ",
                    originPoints$name[i],
                    ".")
            next
            
          } else {
            time_df_tmp2 <-
              sp::over(destination_points_spdf,
                       isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
          }
        }
        
        for (n in 1:ncol(time_df_tmp)) {
          time_df_tmp[length(isochroneCutOffs) + 1, n] <-
            min(time_df_tmp[1:length(isochroneCutOffs), n], na.rm = TRUE)
        }
        
        rownames(time_df_tmp)[length(isochroneCutOffs) + 1] <-
          originPoints$name[i]
        
        if (exists("time_df")) {
          time_df <-
            rbind(time_df, time_df_tmp[length(isochroneCutOffs) + 1, ])
        } else {
          time_df <- time_df_tmp[length(isochroneCutOffs) + 1, ]
          
        }
        
      }
    }
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits = 2)
    
    tmp_seq <- isochrone_polygons@plotOrder
    
    for (n in 1:(length(tmp_seq))) {
      if (n == 1) {
        tmp_seq[n] = as.integer(length(tmp_seq))
      } else if (n < length(isochroneCutOffs) + 1 && n > 1) {
        num <- n - 1
        tmp_seq[n] = as.integer(tmp_seq[num] - nrow(originPoints))
      } else {
        num <- n - length(isochroneCutOffs)
        tmp_seq[n] = as.integer(tmp_seq[num] - 1)
      }
    }
    
    isochrone_polygons@plotOrder <- tmp_seq
    
    if (i < nrow(originPoints)) {
      message(
        i,
        " out of ",
        nrow(originPoints),
        " isochrones complete. Time taken ",
        do.call(sum, time.taken),
        " seconds. Estimated time left is approx. ",
        (do.call(mean, time.taken) * nrow(originPoints)) - do.call(sum, time.taken),
        " seconds."
      )
    } else {
      message(
        i,
        " out of ",
        nrow(originPoints),
        " isochrones complete. Time taken ",
        do.call(sum, time.taken),
        " seconds."
      )
    }
  }
  
  options(warn = 0)
  
  if (length(originPoints_removed > 0)) {
    originPoints <- originPoints[-c(originPoints_removed_list),]
  }
  
  for (n in 1:nrow(destinationPoints)) {
    colnames(time_df)[n] <- destinationPoints$name[n]
  }
  
  for (n in 1:nrow(originPoints)) {
    rownames(time_df)[n] <- originPoints$name[n]
  }
  
  time_df <- time_df / 60
  
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
  m <-
    addPolygons(
      m,
      data = isochrone_polygons,
      # Adds polygons from journey
      stroke = TRUE,
      color = palIsochrone(rev(isochroneCutOffs)),
      opacity = 1,
      weight = 5,
      dashArray = 2,
      smoothFactor = 0.3,
      fillOpacity = 0.6,
      fillColor = palIsochrone(rev(isochroneCutOffs))
    )
  m <-
    addCircleMarkers(
      m,
      data = destinationPoints,
      # Adds circles for each destination
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
  m <- addLegend(
    m,
    pal = palIsochrone,
    # Adds a legend for the trip
    values = isochroneCutOffs,
    opacity = 0.5,
    title = "Duration (minutes)"
  )
  m <-
    addAwesomeMarkers(
      m,
      data = originPoints,
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
  
  invisible(print(m))
  
  # Plots leaflet map gif in Viewer and saves to disk, also saves table as csv ----------
  
  message("Analysis complete, now saving outputs to ",
          output.dir,
          ", please wait.\n")
  
  stamp <-
    format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
  
  invisible(print(m)) # plots map to Viewer
  mapview::mapshot(m, file = paste0(output.dir, "/isochrone_multi-", stamp, ".png"))
  htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochrone_multi-", stamp, ".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/isochrone_multi-", stamp, "_files"),
         recursive = TRUE) # Deletes temporary folder created by mapshot
  
  write.csv(
    time_df,
    file = paste0(output.dir, "/isochrone_multi_inc-", stamp, ".csv"),
    row.names = TRUE
  ) # Saves trip details as a CSV
  
  if (length(originPoints_removed > 0)) {
    write.csv(
      originPoints_removed,
      file = paste0(output.dir, "/originPoints-removed-", stamp, ".csv"),
      row.names = TRUE
    ) # Saves trip details as a CSV
  }
  
  unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  
  rgdal::writeOGR(
    isochrone_polygons,
    dsn = paste0(output.dir,
                 "/isochrone_multi",
                 stamp,
                 ".geoJSON"),
    layer = "isochrone_polygons",
    driver = "GeoJSON"
  )
  
  isochrone_polygons
}
