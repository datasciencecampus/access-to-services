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
##' @param isochroneCutOffStep Provide the cutoff time step for the isochrone, 0 denotes no step is required (returns isochroneCutOffMax only), defaults 10
##' @param mapOutput Specifies whether you want to output a map, defaults to FALSE
##' @param geojsonOutput Specifies whether you want to output a GeoJSON file, defaults to FALSE
##' @param histOutput Specifies whether you want to output a histogram, defaults to FALSE
##' @param mapZoom The zoom level of the map as an integer (e.g. 12), defaults to bounding box approach
##' @param mapPolygonLineWeight Specifies the weight of the polygon, defaults to 1 px
##' @param mapPolygonLineColor Specifies the color of the polygon, defaults to 'white'
##' @param mapPolygonLineOpacity Specifies the opacity of the polygon line, defaults to 1 (solid)
##' @param mapPolygonFillOpacity Specifies the opacity of the polygon fill, defaults to 1
##' @param originMarker Specifies if you want to output the origin markers to the map (default is True)
##' @param originMarkerColor Specifies the colour of the origin marker if it is within a isochrone (default is 'red')
##' @param destinationMarkerSize Specifies the destination marker(s) size (default is 3)
##' @param destinationMarkerOpacity Specifies the opacity of destination marker(s)if it is within a isochrone (default is 1, solid)
##' @param destinationMarkerStroke Specifies whether a destination marker(s) stroke is used (default is T)
##' @param destinationMarkerStrokeColor Specifies the stroke color for the destination marker(s) (default is 'black')
##' @param destinationMarkerStrokeWeight Specifies the marker stroke weight for the destination marker(s) (default is 1)
##' @param destinationMarkerColor Specifies the colour of destination marker(s) if it is not within a isochrone (default is '#00FFAE')
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 0.5
##' @param mapDarkMode Specifies if you want to use the dark leaflet map colour (default is FALSE)
##' @param failSafeSave Specify the failsafe save number for large datasets, default is 100
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
                           # output args
                           mapOutput = F,
                           geojsonOutput = F,
                           histOutput = F,
                           failSafeSave = 100,
                           # leaflet map args
                           mapZoom = "bb",
                           mapPolygonLineWeight = 1,
                           mapPolygonLineColor = 'white',
                           mapPolygonLineOpacity = 1,
                           mapPolygonFillOpacity = 1,
                           originMarker = T,
                           originMarkerColor = 'red',
                           destinationMarkerSize = 3,
                           destinationMarkerOpacity = 1,
                           destinationMarkerStroke = T,
                           destinationMarkerStrokeColor = 'black',
                           destinationMarkerStrokeWeight = 1,
                           destinationMarkerColor = '#00FFAE',
                           mapLegendOpacity = 0.5,
                           mapDarkMode = F) {
  
  message("Now running the propeR isochroneMulti tool.\n")
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 
  
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
  
  if (isochroneCutOffStep == 0){
    isochroneCutOffs <- isochroneCutOffMax
  } else {
    isochroneCutOffs <- seq(isochroneCutOffMin, isochroneCutOffMax, isochroneCutOffStep)
  }
  
  if (mapDarkMode == T){
    mapPolygonColours <- c("#4365BC", "#5776C4", "#6C87CC", "#8098D4", "#95A9DB", "#AABAE3", "#BFCBEA", "#D4DCF1", "#E9EEF8")
  } else {
    mapPolygonColours <- c("#192448", "#1F2B58", "#243368", "#293B78", "#2E4288", "#334A98", "#3851A8", "#3D58B9", "#4863C3")
  }
  
  if (mapOutput == T) {
    library(leaflet)
    palIsochrone = leaflet::colorFactor(mapPolygonColours, NULL, n = length(isochroneCutOffs))
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
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
        end.time <- Sys.time()
        time.taken[num.run] <- round(end.time - start.time, digits = 2)
        message(num.run,
                "/",
                num.total,
                ": Isochrone failed for ",
                originPoints$name[num.run],
                ". Removed ",
                originPoints$name[num.run],
                " from analysis as no polygon could be generated from it. Time taken ",
                round(sum(time.taken), digit = 2),
                " seconds.")
        next
        
      } else {
        isochrone_polygons <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F)
        isochrone_polygons@data$name <- from_origin$name # adds name to json
        sp::proj4string(destination_points_spdf) <- sp::proj4string(isochrone_polygons) 
        time_df_tmp <- data.frame(matrix(,
            ncol = nrow(destinationPoints),
            nrow = 1)) 
        isochrone_polygons_split <- sp::split(isochrone_polygons, isochrone_polygons@data$time) 
        
        if (length(isochrone_polygons) != length(isochroneCutOffs)){
          cutoff_error_message <- paste0(" (Note: A polygon for cutoff level(s) ", setdiff(isochroneCutOffs, (isochrone_polygons@data$time)/60), " minutes could not be produced for ", originPoints$name[num.run], ").")
        } else {
          cutoff_error_message <- '.'
        }
        
        for (n in 1:length(isochrone_polygons)) {
        
            time_df_tmp2 <- sp::over(destination_points_spdf, isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
            remove(time_df_tmp2)
          
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
        end.time <- Sys.time()
        time.taken[num.run] <- round(end.time - start.time, digits = 2)
        message(num.run,
                "/",
                num.total,
                ": Isochrone failed for ",
                originPoints$name[num.run],
                ". Removed ", 
                originPoints$name[num.run],
                " from analysis as no polygon could be generated from it. Time taken ",
                round(sum(time.taken), digit = 2),
                " seconds")
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
        
        if (length(isochrone_polygons_tmp) != length(isochroneCutOffs)){
          cutoff_error_message <- paste0(" (Note: A polygon for cutoff level(s) ", setdiff(isochroneCutOffs, (isochrone_polygons_tmp@data$time)/60), " minutes could not be produced for ", originPoints$name[num.run], ").")
        } else {
          cutoff_error_message <- '.'
        }
        
        for (n in 1:length(isochrone_polygons_tmp)) {

            time_df_tmp2 <- sp::over(destination_points_spdf, isochrone_polygons_split[[n]]) # Finds the polygon the destination point falls within
            time_df_tmp2$index <- as.numeric(row.names(time_df_tmp2))
            time_df_tmp2 <- time_df_tmp2[order(time_df_tmp2$index),]
            time_df_tmp[n, ] <- time_df_tmp2[, 2]
          
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
        "/",
        num.total,
        ": Isochrone complete for ",
        originPoints$name[num.run],
        ". Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * num.total
        ) - sum(time.taken),
        digits = 2),
        " seconds",
        cutoff_error_message
      )
    } else {
      message(
        num.run,
        "/",
        num.total,
        ": Isochrone complete for ",
        originPoints$name[num.run],
        ". Time taken ",
        sum(time.taken),
        " seconds",
        cutoff_error_message,
        "\n"
      )
    }
    
    if ((num.run+1/failSafeSave) %% 1 == 0) { # fail safe for large files
      
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
          driver = "GeoJSON",
          overwrite_layer = TRUE)
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
    
    if (mapDarkMode != T) {
      m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    } else {
      m <- addProviderTiles(m, providers$CartoDB.DarkMatter)
    }    
    
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
        color = mapPolygonLineColor,
        opacity = mapPolygonLineOpacity,
        weight = mapPolygonLineWeight,
        smoothFactor = 0.3,
        fillOpacity = mapPolygonFillOpacity,
        fillColor = palIsochrone(isochrone_polygons@data$time))
    m <- addCircleMarkers(
        m,
        data = destinationPoints,
        lat = ~ lat,
        lng = ~ lon,
        fillColor = destinationMarkerColor,
        stroke = destinationMarkerStroke,
        color = destinationMarkerStrokeColor,
        opacity = destinationMarkerOpacity,
        weight = destinationMarkerStrokeWeight,
        fillOpacity = destinationMarkerOpacity,
        radius = destinationMarkerSize)
    m <- addLegend(
      m,
      pal = palIsochrone,
      values = isochroneCutOffs,
      opacity = mapLegendOpacity,
      title = "Duration (mins)")
    if (originMarker == T){
      m <-
        addAwesomeMarkers(
          m,
          data = originPoints,
          lat = ~ lat,
          lng = ~ lon,
          popup = ~ name,
          icon = makeAwesomeIcon(
            icon = "hourglass-start",
            markerColor = originMarkerColor,
            iconColor = "white",
            library = "fa"))
    }
  }
  
  ######################
  #### SAVE RESULTS ####
  ######################

  message("Analysis complete. Isochrones were generated for ",
          num.total-length(originPoints_removed_list),
          "/",
          num.total,
          " (",
          round(((num.total0length(originPoints_removed_list))/num.total)*100,2),
          "%) origin points. Now saving outputs to ", 
          output.dir,
          ", please wait.\n")

  is.na(time_df) <- sapply(time_df, is.infinite)
    
  if (histOutput == T) {
    
    travel_times <- c()
    for (i in 1:nrow(time_df)){
      travel_times <- c(travel_times,as.numeric(time_df[i,1:ncol(time_df)]))
    }
  }
  
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
      driver = "GeoJSON",
      overwrite_layer = TRUE)
  }
  
  if (mapOutput == T) {
    invisible(print(m)) 
    mapview::mapshot(m, file = paste0(output.dir, "/isochroneMulti-map-", stamp, ".png"))
    htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochroneMulti-map-", stamp, ".html")) 
    unlink(paste0(output.dir, "/isochroneMulti-map-", stamp, "_files"), recursive = T) 
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
  if (histOutput == T) {
    png(paste0(output.dir, "/isochroneMulti-histogram-", stamp, ".png"))
    hist(travel_times,
         xlab = "Travel time (minutes)",
         main = "",
         col = "black",
         border="white")
    dev.off()
  }
  message("Thanks for using propeR.")

}
