##' Generates an isochrone map for multiple origins.
##'
##' Generates an isochrone map from multiple origins and checks whether destinations
##' fall within isochrones, and if so, at what cutoff time amount.
##' A CSV file of journey details is saved in the output folder.
##' A map of the journey can also be saved as a PNG image and HTML file.
##' The polygons can also be saved as a GeoJSON file.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param journeyReturn Specifies whether the journey should be calculated as a return or not (default is FALSE)
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
##' @param isochroneCutOffs Provide a list of cutoffs in minutes, defaults to c(30, 60, 90)
##' @param mapOutput Specifies whether you want to output a map, defaults to FALSE
##' @param geojsonOutput Specifies whether you want to output a GeoJSON file, defaults to FALSE
##' @param mapPolygonColours The color palette of the map, defaults to 'Blues'
##' @param mapZoom The zoom level of the map, defaults to 12
##' @param mapPolygonLineWeight Specifies the weight of the polygon, defaults to 5 px
##' @param mapPolygonLineOpacity Specifies the opacity of the polygon line, defaults to 1 (solid)
##' @param mapPolygonFillOpacity Specifies the opacity of the polygon fill, defaults to 0.6
##' @param mapMarkerOpacity Specifies the opacity of the marker, defaults to 1 (solid)
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 0.5
##' @param
##' @return Saves journey details as CSV to output directory (optional: a map in PNG and HTML formats, the polygons as a GeoJSON)
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
                           journeyReturn = FALSE,
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
                           mapOutput = FALSE,
                           geojsonOutput = FALSE,
                           mapPolygonColours = "Blues",
                           mapZoom = 12,
                           mapPolygonLineWeight = 5,
                           mapPolygonLineOpacity = 1,
                           mapPolygonFillOpacity = 0.6,
                           mapMarkerOpacity = 1,
                           mapLegendOpacity = 0.5) {
  message("Now running the propeR isochroneMulti tool.\n")
  
  if (mapOutput == TRUE) {
    library(leaflet)
    palIsochrone = leaflet::colorFactor(mapPolygonColours, NULL, n = length(isochroneCutOffs)) # Creating colour palette
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
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
  
  message("Creating ", nrow(originPoints), " isochrones, please wait...")
  time.taken <- vector()
  
  start_num <- 1
  end_num <- nrow(originPoints)
  run_num <- 0
  total_run_num <- end_num
  
  originPoints_removed <- c()
  originPoints_removed_list <- c()
  
  for (i in start_num:end_num) {
    # Start loop to calculate journey details
    
    start.time <- Sys.time()
    
    from_origin <- originPoints[i, ]
    
    run_num <- run_num + 1
    
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
    
    options(warn = -1)
    
    if (i == start_num) {
      # Combines outputs
      t <-
        try(isochrone_polygons <-
              rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE), silent = TRUE)
      # Converts the polygons to be handled in leaflet
      if ("try-error" %in% class(t)) {
        originPoints_removed <-
          c(originPoints_removed, originPoints$name[i])
        originPoints_removed_list <- c(originPoints_removed_list, i)
        run_num <- run_num - 1
        total_run_num <- total_run_num - 1
        time.taken[i] <- round(end.time - start.time, digits = 2)
        message(
          "Removed ",
          originPoints$name[i],
          " from analysis as no polygon could be generated from it. Total isochrones is now ",
          total_run_num,
          " not ",
          end_num
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
            message(
              "Some polygons could not be generated, A cutoff level may be too small for ",
              originPoints$name[i],
              "."
            )
            next
            
          } else {
            time_df_tmp2 <-
              sp::over(destination_points_spdf,
                       isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <-
              as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <-
              time_df_tmp2[order(time_df_tmp2$index),]
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
              rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE), silent = TRUE)
      # Converts the polygons to be handled in leaflet
      if ("try-error" %in% class(t)) {
        originPoints_removed <-
          c(originPoints_removed, originPoints$name[i])
        originPoints_removed_list <- c(originPoints_removed_list, i)
        run_num <- run_num - 1
        total_run_num <- total_run_num - 1
        time.taken[i] <- round(end.time - start.time, digits = 2)
        message(
          "Removed ",
          originPoints$name[i],
          " from analysis as no polygon could be generated from it. Total isochrones is now ",
          total_run_num,
          " not ",
          end_num
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
            message(
              "Some polygons could not be generated, A cutoff level may be too small for ",
              originPoints$name[i],
              "."
            )
            next
            
          } else {
            time_df_tmp2 <-
              sp::over(destination_points_spdf,
                       isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <-
              as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <-
              time_df_tmp2[order(time_df_tmp2$index),]
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
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits = 2)
    
    if (i < nrow(originPoints)) {
      message(
        run_num,
        " out of ",
        total_run_num,
        " isochrones complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * total_run_num
        ) - sum(time.taken),
        digits = 2),
        " seconds."
      )
    } else {
      message(
        run_num,
        " out of ",
        total_run_num,
        " isochrones complete. Time taken ",
        sum(time.taken),
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
  
  # for (n in 1:nrow(originPoints)) {
  #   if (n > 1){
  #     if (originPoints$name[n] %in% rownames(time_df)) {
  #       rownames(time_df)[n] <- paste0(originPoints$name[n],'_1')
  #     } else {
  #       rownames(time_df)[n] <- toString(originPoints$name[n])
  #     }
  #   } else {
  #     rownames(time_df)[n] <- toString(originPoints$name[n])
  #   }
  # }
  
  time_df <- time_df / 60
  
  #########################
  #### OPTIONAL EXTRAS ####
  #########################
  
  
  if (mapOutput == TRUE) {
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
        opacity = mapPolygonLineOpacity,
        weight = mapPolygonLineWeight,
        dashArray = 2,
        smoothFactor = 0.3,
        fillOpacity = mapPolygonFillOpacity,
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
        opacity = mapMarkerOpacity,
        weight = 2,
        fillOpacity = mapMarkerOpacity,
        radius = 10
      )
    m <- addLegend(
      m,
      pal = palIsochrone,
      # Adds a legend for the trip
      values = isochroneCutOffs,
      opacity = mapLegendOpacity,
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
  
  if (geojsonOutput == TRUE) {
    rgdal::writeOGR(
      isochrone_polygons,
      dsn = paste0(output.dir,
                   "/isochrone_multi",
                   stamp,
                   ".geoJSON"),
      layer = "isochrone_polygons",
      driver = "GeoJSON"
    )
  }
  
  if (mapOutput == TRUE) {
    invisible(print(m)) # plots map to Viewer
    mapview::mapshot(m, file = paste0(output.dir, "/isochrone_multi-", stamp, ".png"))
    htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochrone_multi-", stamp, ".html")) # Saves as an interactive HTML webpage
    unlink(paste0(output.dir, "/isochrone_multi-", stamp, "_files"),
           recursive = TRUE) # Deletes temporary folder created by mapshot
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  }
}
