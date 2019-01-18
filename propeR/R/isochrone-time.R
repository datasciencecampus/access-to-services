##' Generates an isochrone map from a single origin between a start and end
##' time and date.
##'
##' Generates isochrone polygons from a single origin between a start and end
##' time and date, and checks whether destinations fall within isochrone,
##' and if so, at what cutoff time amount.
##' A comma separated value file of journey times for each destination (and time) is saved in the output folder.
##' An animated map of the journey can also be saved as a .gif image.
##' 
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL, see ?otpcon for details
##' @param originPoints The variable containing origin(s), see ?importLocationData for details
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData for details
##' @param startDateAndTime The start time and date, in 'YYYY-MM-DD HH:MM:SS' format
##' @param endDateAndTime The end time and date, in 'YYYY-MM-DD HH:MM:SS' format
##' @param timeIncrease The time increase in minutes, default 60
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
##' @param mapZoom The zoom level of the map as an integer (e.g. 12), defaults to bounding box approach
##' @param mapPolygonLineWeight Specifies the weight of the polygon, defaults to 5 px
##' @param mapPolygonLineOpacity Specifies the opacity of the polygon line, defaults to 1 (solid)
##' @param mapPolygonFillOpacity Specifies the opacity of the polygon fill, defaults to 0.6
##' @param mapMarkerOpacity Specifies the opacity of the marker, defaults to 1 (solid)
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 0.5
##' @return Saves journey details as comma separated value file to output directory. An animated map in .gif format may also be saved.
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
                          # leaflet map args
                          mapOutput = F,
                          mapPolygonColours = "Blues",
                          mapZoom = "bb",
                          mapPolygonLineWeight = 5,
                          mapPolygonLineOpacity = 1,
                          mapPolygonFillOpacity = 0.6,
                          mapMarkerOpacity = 1,
                          mapLegendOpacity = 0.5) {
  
  message("Now running the propeR isochroneTime tool.\n")
  
  if (mapOutput == T) {
    library(leaflet)
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL)
    palIsochrone = leaflet::colorFactor(mapPolygonColours, NULL, n = length(isochroneCutOffs))
    dir.create(paste0(output.dir, "/tmp_folder"))
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <-
    originPointsRow
  
  if (origin_points_row_num > nrow(originPoints)) {
    message('Row is not in origin file')
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T)
    break #todo: need a better system to stop code as break shouldn't be used outside a loop
  }
  
  from_origin <-
    originPoints[origin_points_row_num,]
  
  start_time <-
    format(as.POSIXct(startDateAndTime), "%I:%M %p") 
  
  start_date <- as.Date(startDateAndTime) 
  
  date_time_legend <-
    format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") 
  
  time_series = seq(as.POSIXct(startDateAndTime),
                    as.POSIXct(endDateAndTime),
                    by = timeIncrease * 60)
  
  destination_points_num_of_cols <-
    ncol(destinationPoints) 
  
  destination_points_output <-
    destinationPoints
  
  destination_points_output[as.character(time_series)] <-
    NA
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
  num.start <- 1
  num.end <- length(time_series)
  num.run <- 0
  num.total <- num.end
  
  message("Creating ",
          num.end ,
          " isochrones, please wait...")
  
  time.taken <- vector()
  
  for (i in num.start:num.end) {
    
    num.run <- num.run + 1
    
    start.time <- Sys.time()
    
    stamp <-
      format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
    
    date_time_legend <-
      format(time_series[i], "%B %d %Y %H:%M")
    
    time <- format(time_series[i], "%I:%M %p")
    
    date <- format(time_series[i], "%m/%d/%Y")
    
    isochrone <- propeR::otpIsochrone(
      otpcon,
      batch = T,
      from = from_origin$lat_lon,
      to = from_origin$lat_lon,
      modes = modes,
      date = date,
      time = time,
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
      rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F) # Converts the polygons to be handled in leaflet
    
    destination_points_spdf <-
      destination_points_output
    sp::coordinates(destination_points_spdf) <-
      ~ lon + lat
    sp::proj4string(destination_points_spdf) <-
      sp::proj4string(isochrone_polygons)
    isochrone_polygons_split <-
      sp::split(isochrone_polygons, isochrone_polygons@data$time)
    time_df <-
      data.frame(matrix(
        ,
        ncol = length(isochroneCutOffs),
        nrow = nrow(destination_points_spdf)
      )) # Create time dataframe
    
    for (n in 1:length(isochroneCutOffs)) {
      time_df_tmp <-
        sp::over(destination_points_spdf, isochrone_polygons_split[[n]])
      time_df[, n] <- time_df_tmp[, 2]
    }
    
    for (n in 1:nrow(destination_points_spdf)) {
      if (is.na(time_df[n, length(isochroneCutOffs)])) {
        time_df[n, length(isochroneCutOffs) + 1] = NA
        destination_points_output[n, destination_points_num_of_cols + i] = NA
      } else {
        time_df[n, length(isochroneCutOffs) + 1] = min(time_df[n, 1:length(isochroneCutOffs)], na.rm =
                                                         T)
        destination_points_output[n, destination_points_num_of_cols + i] = (time_df[n, length(isochroneCutOffs) +
                                                                                      1]) / 60
      }
    }
    
    names(time_df)[ncol(time_df)] <- "travel_time"
    destinationPoints$travel_time <- time_df$travel_time / 60
    
    destination_points_non_na <-
      subset(destinationPoints,!(is.na(destinationPoints["travel_time"])))
    
    #########################
    #### OPTIONAL EXTRAS ####
    #########################
    
    if (mapOutput == T) {
      
      if (num.run == 1) {
        lon.min <- min(min(from_origin$lon),min(destinationPoints$lon),isochrone_polygons@bbox[1])
        lat.min <- min(min(from_origin$lat),min(destinationPoints$lat),isochrone_polygons@bbox[2])
        lon.max <- max(max(from_origin$lon),max(destinationPoints$lon),isochrone_polygons@bbox[3])
        lat.max <- max(max(from_origin$lat),max(destinationPoints$lat),isochrone_polygons@bbox[4])
      }
      
      m <- leaflet()
      m <- addScaleBar(m)
      m <-
        addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
     
       if (is.numeric(mapZoom)){
        m <- setView(
          m,
          lat = (from_origin$lat + mean(destinationPoints$lat)) / 2,
          lng = (from_origin$lon + mean(destinationPoints$lon)) / 2,
          zoom = mapZoom
        )
      } else {
        m <- fitBounds(
          m,
          lon.min,
          lat.min,
          lon.max,
          lat.max)
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
        fillColor = palIsochrone(rev(isochroneCutOffs))
      )
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
        radius = 10
      )
      m <- addCircleMarkers(
        m,
        data = destination_points_non_na,
        lat = ~ lat,
        lng = ~ lon,
        fillColor = "black",
        stroke = T,
        color = "black",
        opacity = mapMarkerOpacity,
        weight = 2,
        fillOpacity = mapMarkerOpacity,
        radius = 10
      )
      m <-
        addLegend(
          m,
          pal = palIsochrone,
          values = isochroneCutOffs,
          opacity = mapLegendOpacity,
          title = "Duration (minutes)"
        )
      m <-
        addLegend(
          m,
          pal = pal_time_date,
          values = date_time_legend,
          position = "bottomleft",
          title = "Date and Time"
        )
      m <-
        addAwesomeMarkers(
          m,
          data = from_origin,
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
      
      mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/", stamp, ".png")) 
    }
    
    end.time <- Sys.time()
    time.taken[num.run] <- round(end.time - start.time, digits = 2)
    
    if (num.run < num.total) {
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
        " seconds."
      )
    }
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
    destination_points_output,
    file = paste0(output.dir, "/isochroneTime-", stamp, ".csv"),
    row.names = F
  ) # Saves trip details as a CSV
  
  if (mapOutput == T) {
    message("Making and saving GIF, this may take a while.")
    library(dplyr)
    list.files(
      path = paste0(output.dir, "/tmp_folder"),
      pattern = "*.png",
      full.names = T
    ) %>%  # Creates gif of results
      purrr::map(magick::image_read) %>%  # reads each path file
      magick::image_join() %>% # joins image
      magick::image_animate(fps = 5) %>%  # animates, can opt for number of loops
      magick::image_write(paste0(output.dir, "/isochroneTime-", stamp, ".gif")) # write to current dir
    
    m <-
      magick::image_read(paste0(output.dir, "/isochroneTime-", stamp, ".gif")) %>%
      magick::image_scale("600") # Loads GIF into R
    
    invisible(print(m)) # plots map to Viewer
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) # Deletes tmp_folder if exists
  }
}
