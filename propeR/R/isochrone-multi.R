##' Generates an isochrone map for multiple origins.
##'
##' Generates an isochrone map from multiple origins and checks whether destinations
##' fall within isochrones, and if so, at what cutoff time amount.
##' A comma separated value file of journey times for each origin and destination is saved in the output folder.
##' A map of the journey can also be saved as a .png image and .html file.
##' The polygons can also be saved as a .GeoJSON file.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL, see ?otpcon for details
##' @param originPoints The variable containing origin(s), see ?importLocationData for details
##' @param destinationPoints The variable containing destination(s) see ?importLocationData for details
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
##' @param isochroneCutOffMax Provide the maximum cutoff time for the isochrone, defaults 90
##' @param isochroneCutOffMin Provide the minimum cutoff time for the isochrone, defaults 10
##' @param isochroneCutOffStep Provide the cutoff time step for the isochrone, defaults 10
##' @param mapOutput Specifies whether you want to output a map, defaults to FALSE
##' @param geojsonOutput Specifies whether you want to output a GeoJSON file, defaults to FALSE
##' @param mapPolygonColours The color palette of the map, defaults to 'Blues'
##' @param mapZoom The zoom level of the map as an integer (e.g. 12), defaults to bounding box approach
##' @param mapPolygonLineWeight Specifies the weight of the polygon, defaults to 5 px
##' @param mapPolygonLineOpacity Specifies the opacity of the polygon line, defaults to 1 (solid)
##' @param mapPolygonFillOpacity Specifies the opacity of the polygon fill, defaults to 0.6
##' @param mapMarkerOpacity Specifies the opacity of the marker, defaults to 1 (solid)
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 0.5
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
                           journeyReturn = F,
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
                           isochroneCutOffMax = 90,
                           isochroneCutOffMin = 10,
                           isochroneCutOffStep = 10,
                           isochroneCutOffs = seq(isochroneCutOffMin, isochroneCutOffMax, isochroneCutOffStep),
                           # leaflet map args
                           mapOutput = F,
                           geojsonOutput = F,
                           mapPolygonColours = "Blues",
                           mapZoom = "bb",
                           mapPolygonLineWeight = 5,
                           mapPolygonLineOpacity = 1,
                           mapPolygonFillOpacity = 0.6,
                           mapMarkerOpacity = 1,
                           mapLegendOpacity = 0.5) {
  
  message("Now running the propeR isochroneMulti tool.\n")
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 
  
  if (mapOutput == T) {
    library(leaflet)
    palIsochrone = leaflet::colorFactor(mapPolygonColours, NULL, n = length(isochroneCutOffs))
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  start_time <- format(as.POSIXct(startDateAndTime), "%I:%M %p")
  start_date <- as.Date(startDateAndTime) 
  date_time_legend <- format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") 
  rownames(destinationPoints) <- NULL
  destination_points_spdf <- destinationPoints 
  sp::coordinates(destination_points_spdf) <- ~ lon + lat 
  
  num.start <- 1
  num.end <- nrow(originPoints)
  num.run <- 0
  num.total <- num.end
  time.taken <- vector()
  originPoints_removed <- c()
  originPoints_removed_list <- c()
  message("Creating ", num.total, " isochrones, please wait...")
  
  for (i in num.start:num.end) {
    start.time <- Sys.time()
    num.run <- num.run + 1
    from_origin <- originPoints[num.run, ]
    
    isochrone <- propeR::otpIsochrone(
      otpcon,
      batch = T,
      from = from_origin$lat_lon,
      to = from_origin$lat_lon,
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
      cutoff = isochroneCutOffs
    )
    
    options(warn = -1)
    
    if (num.run == num.start) {
    
      t <- try(isochrone_polygons <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F), silent = T)
      
      if ("try-error" %in% class(t)) {
        originPoints_removed <- c(originPoints_removed, originPoints$name[num.run])
        originPoints_removed_list <- c(originPoints_removed_list, num.run)
        time.taken[num.run] <- round(end.time - start.time, digits = 2)
        message("Removed ", originPoints$name[num.run], " from analysis as no polygon could be generated from it.")
        next
        
      } else {
        isochrone_polygons <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F)
        isochrone_polygons@data$name <- from_origin$name # adds name to json
        sp::proj4string(destination_points_spdf) <- sp::proj4string(isochrone_polygons) 
        time_df_tmp <- data.frame(matrix(,
            ncol = nrow(destinationPoints),
            nrow = 1)) 
        isochrone_polygons_split <- sp::split(isochrone_polygons, isochrone_polygons@data$time) 
        
        for (n in 1:length(isochroneCutOffs)) {
          
          if ("try-error" %in% class(t)) {
            time_df_tmp[n, ] <- NA
            message("Some polygons could not be generated, A cutoff level may be too small for ", originPoints$name[num.run], ".")
            next
            
          } else {
            time_df_tmp2 <- sp::over(destination_points_spdf, isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
            remove(time_df_tmp2)
          }
        }
        
        options(warn = -1)
        
        for (n in 1:ncol(time_df_tmp)) {
          time_df_tmp[length(isochroneCutOffs) + 1, n] <- min(time_df_tmp[1:length(isochroneCutOffs), n], na.rm = T)
        }
        
        options(warn = 0)
        
        rownames(time_df_tmp)[length(isochroneCutOffs) + 1] <- originPoints$name[num.run]
        time_df <- time_df_tmp[length(isochroneCutOffs) + 1, ]
      }
    } else {
      t <- try(isochrone_polygons_tmp <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F), silent = T)
      
      if ("try-error" %in% class(t)) {
        originPoints_removed <- c(originPoints_removed, originPoints$name[num.run])
        originPoints_removed_list <- c(originPoints_removed_list, num.run)
        time.taken[num.run] <- round(end.time - start.time, digits = 2)
        message("Removed ", originPoints$name[num.run], " from analysis as no polygon could be generated from it.")
        next
        
      } else {
        isochrone_polygons_tmp <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F)
        isochrone_polygons_tmp@data$name <- from_origin$name # adds name to json
        
        if (exists("isochrone_polygons")) {
          isochrone_polygons <- rbind(isochrone_polygons, isochrone_polygons_tmp)
        } else {
          isochrone_polygons <- isochrone_polygons_tmp
        }
        
        sp::proj4string(destination_points_spdf) <- sp::proj4string(isochrone_polygons_tmp) # Take projection
        time_df_tmp <- data.frame(matrix(,
            ncol = nrow(destinationPoints),
            nrow = 1)) 
        isochrone_polygons_split <- sp::split(isochrone_polygons_tmp, isochrone_polygons_tmp@data$time) 
        for (n in 1:length(isochroneCutOffs)) {
          t <- try(time_df_tmp2 <- sp::over(destination_points_spdf, isochrone_polygons_split[[n]]))
          
          if ("try-error" %in% class(t)) {
            time_df_tmp[n, ] <- NA
            message("Some polygons could not be generated, A cutoff level may be too small for ", originPoints$name[num.run], ".")
            next
            
          } else {
            time_df_tmp2 <- sp::over(destination_points_spdf, isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
          }
        }
        
        for (n in 1:ncol(time_df_tmp)) {
          time_df_tmp[length(isochroneCutOffs) + 1, n] <- min(time_df_tmp[1:length(isochroneCutOffs), n], na.rm = T)
        }
        
        rownames(time_df_tmp)[length(isochroneCutOffs) + 1] <- originPoints$name[num.run]
        
        if (exists("time_df")) {
          time_df <- rbind(time_df, time_df_tmp[length(isochroneCutOffs) + 1, ])
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
    time.taken[num.run] <- round(end.time - start.time, digits = 2)
    
    if (i < nrow(originPoints)) {
      message(
        num.run,
        " out of ",
        num.total,
        " isochrones complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * num.total
        ) - sum(time.taken),
        digits = 2),
        " seconds."
      )
    } else {
      message(
        num.run,
        " out of ",
        num.total,
        " isochrones complete. Time taken ",
        sum(time.taken),
        " seconds.\n"
      )
    }
    
    if ((num.run/100) %% 1 == 0) { # fail safe for large files
      
      message("Large dataset, failsafe, saving outputs to ", output.dir, ", please wait.")
      
      is.na(time_df) <- sapply(time_df, is.infinite)
      
      write.csv(
        time_df,
        file = paste0(output.dir, "/isochroneMulti-isochrone_multi_inc-", stamp, ".csv"),
        row.names = T) 
      
      if (length(originPoints_removed > 0)) {
        write.csv(
          originPoints_removed,
          file = paste0(output.dir, "/isochroneMulti-originPoints-removed-", stamp, ".csv"),
          row.names = T) 
      }
      
      if (geojsonOutput == T) {
        rgdal::writeOGR(
          isochrone_polygons,
          dsn = paste0(output.dir,
                       "/isochroneMulti",
                       stamp,
                       ".geoJSON"),
          layer = "isochrone_polygons",
          driver = "GeoJSON")
      }
      
      if (mapOutput == T) {
        invisible(print(m)) 
        mapview::mapshot(m, file = paste0(output.dir, "/isochroneMulti-", stamp, ".png"))
        htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochroneMulti-", stamp, ".html")) 
        unlink(paste0(output.dir, "/isochroneMulti-", stamp, "_files"), recursive = T) 
        unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
      }
    }
  }
  
  options(warn = 0)
  
  if (length(originPoints_removed > 0)) {
    originPoints <- originPoints[-c(originPoints_removed_list),]
  }
  
  for (n in 1:nrow(destinationPoints)) {
    colnames(time_df)[n] <- destinationPoints$name[n]
  }
  
  time_df <- time_df / 60
  
  #########################
  #### OPTIONAL EXTRAS ####
  #########################
  
  if (mapOutput == T) {
    m <- leaflet()
    m <- addScaleBar(m)
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    
    if (is.numeric(mapZoom)){
      m <- setView(
        m,
        lat = (mean(originPoints$lat) + mean(destinationPoints$lat)) / 2,
        lng = (mean(originPoints$lon) + mean(destinationPoints$lon)) / 2,
        zoom = mapZoom
      )
    } else {
      m <- fitBounds(
        m,
        min(min(originPoints$lon),min(destinationPoints$lon),isochrone_polygons@bbox[1]),
        min(min(originPoints$lat),min(destinationPoints$lat),isochrone_polygons@bbox[2]),
        max(max(originPoints$lon),max(destinationPoints$lon),isochrone_polygons@bbox[3]),
        max(max(originPoints$lat),max(destinationPoints$lat),isochrone_polygons@bbox[4]))
    }
    
    m <- addPolygons(
        m,
        data = isochrone_polygons,
        stroke = T,
        color = palIsochrone(rev(isochroneCutOffs)),
        opacity = mapPolygonLineOpacity,
        weight = mapPolygonLineWeight,
        dashArray = 2,
        smoothFactor = 0.3,
        fillOpacity = mapPolygonFillOpacity,
        fillColor = palIsochrone(rev(isochroneCutOffs)))
    m <- addCircleMarkers(
        m,
        data = destinationPoints,
        lat = ~ lat,
        lng = ~ lon,
        fillColor = "white",
        stroke = T,
        color = "black",
        opacity = mapMarkerOpacity,
        weight = 2,
        fillOpacity = mapMarkerOpacity,
        radius = 10)
    m <- addLegend(
      m,
      pal = palIsochrone,
      values = isochroneCutOffs,
      opacity = mapLegendOpacity,
      title = "Duration (minutes)")
    m <- addAwesomeMarkers(
        m,
        data = originPoints,
        lat = ~ lat,
        lng = ~ lon,
        popup = ~ name,
        icon = makeAwesomeIcon(
          icon = "hourglass-start",
          markerColor = "red",
          iconColor = "white",
          library = "fa"))
  }
  
  ######################
  #### SAVE RESULTS ####
  ######################

  message("Analysis complete, now saving outputs to ", output.dir, ", please wait.\n")

  is.na(time_df) <- sapply(time_df, is.infinite)
    
  write.csv(
    time_df,
    file = paste0(output.dir, "/isochroneMulti-isochrone_multi_inc-", stamp, ".csv"),
    row.names = T) 
  
  if (length(originPoints_removed > 0)) {
    write.csv(
      originPoints_removed,
      file = paste0(output.dir, "/isochroneMulti-originPoints-removed-", stamp, ".csv"),
      row.names = T) 
  }
  
  if (geojsonOutput == T) {
    rgdal::writeOGR(
      isochrone_polygons,
      dsn = paste0(output.dir,
                   "/isochroneMulti",
                   stamp,
                   ".geoJSON"),
      layer = "isochrone_polygons",
      driver = "GeoJSON")
  }
  
  if (mapOutput == T) {
    invisible(print(m)) 
    mapview::mapshot(m, file = paste0(output.dir, "/isochroneMulti-", stamp, ".png"))
    htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochroneMulti-", stamp, ".html")) 
    unlink(paste0(output.dir, "/isochroneMulti-", stamp, "_files"), recursive = T) 
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
  message("Thanks for using propeR.")
}
