##' Calculates the journey between for a single origin and destination between a
##' start and end time and date.
##'
##' Calculates the journey time and details between a single origin and destination between a start
##' and end time and date. A comma separated value file of journey details is saved in the output folder.
##' An animated map of the journey can also be saved as a .gif image.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL, see ?otpcon for details
##' @param originPoints The variable containing origin(s), see ?importLocationData for details
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData for details
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
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
##' @param preWaitTime The maximum waiting time before a journey cannot be found, in minutes, defaults to 15 mins
##' @param mapOutput Specifies whether you want to output a map, defaults to FALSE
##' @param mapPolylineColours A list defining the colours to assign to each mode of transport.
##' @param mapZoom The zoom level of the map as an integer (e.g. 12), defaults to bounding box approach
##' @param mapPolylineWeight Specifies the weight of the polyline, defaults to 5 px
##' @param mapPolylineOpacity Specifies the opacity of the polyline, defaults to 1 (solid)
##' @param mapMarkerOpacity Specifies the opacity of the marker, defaults to 1 (solid)
##' @param mapLegendOpacity Specifies the opacity of the legend, defaults to 1 (solid)
##' @return Saves journey details as comma separated value file to output directory. An animated map in .gif format may also be saved.
##' @author Michael Hodge
##' @examples
##'   pointToPointTime(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00",
##'     endDateAndTime = "2018-08-18 13:00:00",
##'     timeIncrease = 60
##'   )
##' @export
pointToPointTime <- function(output.dir,
                             otpcon,
                             originPoints,
                             originPointsRow = 1,
                             destinationPoints,
                             destinationPointsRow = 1,
                             # otpTime args
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
                             preWaitTime = 15,
                             # leaflet map args
                             mapOutput = F,
                             mapPolylineColours = list(
                               TRANSIT = "#000000",
                               WALK = "#A14296",
                               BUS = "#48C1B1",
                               RAIL = "#4D7BC5",
                               CAR = "#8D4084",
                               BICYCLE = "#4AA6C3"
                             ),
                             mapZoom = "bb",
                             mapPolylineWeight = 5,
                             mapPolylineOpacity = 1,
                             mapMarkerOpacity = 1,
                             mapLegendOpacity = 1) {
  
  message("Now running the propeR pointToPointTime tool.\n")
  
  if (mapOutput == T) {
    pal_transport <- leaflet::colorFactor(
        palette = unlist(mapPolylineColours, use.names = F),
        # Creating colour palette
        levels = as.factor(names(mapPolylineColours)),
        reverse = F)
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL)
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
    dir.create(paste0(output.dir, "/tmp_folder")) 
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <- originPointsRow
  from_origin <- originPoints[origin_points_row_num, ]
  if (origin_points_row_num > nrow(originPoints)) {
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
    stop('Row is not in origin file, process aborted.\n')
  }
  
  destination_points_row_num <- destinationPointsRow
  to_destination <- destinationPoints[destination_points_row_num, ]
  if (destination_points_row_num > nrow(destinationPoints)) {
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
    stop('Row is not in destination file, process aborted.\n')
  }
  
  start_date <- as.Date(startDateAndTime)
  time_series = seq(as.POSIXct(startDateAndTime), as.POSIXct(endDateAndTime), by = (timeIncrease) * 60) 
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
  num.start <- 1
  num.end <- length(time_series)
  num.run <- 0
  num.total <- num.end
  time.taken <- vector()
  message("Creating ", num.total, " point to point connections, please wait...")
  
  for (i in num.start:num.end) {
    start.time <- Sys.time()
    num.run <- num.run + 1
    stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 
    time <- format(time_series[num.run], "%I:%M %p")
    time_twenty_four <- strftime(as.POSIXct(time_series[i], origin = "1970-01-01"), format = "%H:%M:%S")
    date <- as.Date(time_series[num.run])
    
    if (mapOutput == T) { date_time_legend <- format(time_series[num.run], "%d %B %Y %H:%M") }
    
    point_to_point <- propeR::otpTripTime(
      otpcon,
      detail = T,
      from = from_origin$lat_lon,
      to = to_destination$lat_lon,
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
      preWaitTime = preWaitTime
    ) 
    
    point_to_point_table <- point_to_point$output_table
    
    #########################
    #### OPTIONAL EXTRAS ####
    #########################
    
    if (mapOutput == T) {
      
      if (!is.null(point_to_point_table) && exists("point_to_point_table")) {
        poly_lines <- point_to_point$poly_lines 
        poly_lines <- sp::spTransform(poly_lines, sp::CRS("+init=epsg:4326"))
        
        if (num.run == 1) {
          lon.min <- min(min(from_origin$lon),min(to_destination$lon),poly_lines@bbox[1])
          lat.min <- min(min(from_origin$lat),min(to_destination$lat),poly_lines@bbox[2])
          lon.max <- max(max(from_origin$lon),max(to_destination$lon),poly_lines@bbox[3])
          lat.max <- max(max(from_origin$lat),max(to_destination$lat),poly_lines@bbox[4])
        }
        
        m <- leaflet()
        m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
        m <- addScaleBar(m)
        
        if (is.numeric(mapZoom)){
          m <- setView(
            m,
            lat = (from_origin$lat + to_destination$lat) / 2,
            lng = (from_origin$lon + to_destination$lon) / 2,
            zoom = mapZoom)
        } else {
          m <- fitBounds(
            m,
            lon.min,
            lat.min,
            lon.max,
            lat.max)
        }
        
        m <- addAwesomeMarkers(
            m,
            data = from_origin,
            lat = ~ lat,
            lng = ~ lon,
            popup = ~ name,
            icon = makeAwesomeIcon(
              icon = "hourglass-start",
              markerColor = "red",
              iconColor = "white",
              library = "fa"))
        m <- addPolylines(
            m,
            data = poly_lines,
            color = ~ pal_transport(poly_lines$mode),
            weight = mapPolylineWeight,
            opacity = mapPolylineOpacity)
        m <- addCircleMarkers(
            m,
            data = point_to_point_table,
            lat = ~ from_lat,
            lng = ~ from_lon,
            fillColor = ~ pal_transport(point_to_point_table$mode),
            stroke = F,
            fillOpacity = mapMarkerOpacity,
            popup = ~ mode)
        m <- addLegend(
            m,
            pal = pal_transport,
            values = point_to_point_table$mode,
            opacity = mapLegendOpacity,
            title = "Transport Mode")
        m <- addLegend(
          m,
          pal = pal_time_date,
          values = date_time_legend,
          position = "bottomleft",
          title = "Date and Time")
        m <- addAwesomeMarkers(
            m,
            data = to_destination,
            lat = ~ lat,
            lng = ~ lon,
            popup = ~ name,
            icon = makeAwesomeIcon(
              icon = "hourglass-end",
              markerColor = "blue",
              iconColor = "white",
              library = "fa"))
        remove(poly_lines)
      } else {
        
        if (num.run == 1) {
          lon.min <- min(min(from_origin$lon),min(to_destination$lon))
          lat.min <- min(min(from_origin$lat),min(to_destination$lat))
          lon.max <- max(max(from_origin$lon),max(to_destination$lon))
          lat.max <- max(max(from_origin$lat),max(to_destination$lat))
        }
        
        m <- leaflet()
        m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
        m <- addScaleBar(m)
        
        if (is.numeric(mapZoom)){
          m <- setView(
            m,
            lat = (from_origin$lat + to_destination$lat) / 2,
            lng = (from_origin$lon + to_destination$lon) / 2,
            zoom = mapZoom)
        } else {
          m <- fitBounds(
            m,
            lon.min,
            lat.min,
            lon.max,
            lat.max)
        }
        
        m <- addAwesomeMarkers(
            m,
            data = from_origin,
            lat = ~ lat,
            lng = ~ lon,
            popup = ~ name,
            icon = makeAwesomeIcon(
              icon = "hourglass-start",
              markerColor = "red",
              iconColor = "white",
              library = "fa"))
        m <- addAwesomeMarkers(
            m,
            data = to_destination,
            lat = ~ lat,
            lng = ~ lon,
            popup = ~ name,
            icon = makeAwesomeIcon(
              icon = "hourglass-end",
              markerColor = "blue",
              iconColor = "white",
              library = "fa"))
        m <- addLegend(
            m,
            pal = pal_transport,
            values = NA,
            opacity = mapLegendOpacity,
            title = "Transport Mode")
        m <- addLegend(
          m,
          pal = pal_time_date,
          values = date_time_legend,
          position = "bottomleft",
          title = "Date and Time")
      }
      mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/", stamp, ".png"))
    }
    
    if (num.run == 1) {
      
      if (!is.null(point_to_point$errorId)){
        
        if (point_to_point$errorId == "OK") {
          point_to_point_table_overview <- point_to_point$itineraries
          point_to_point_table_overview["origin"] <- from_origin$name
          point_to_point_table_overview["destination"] <- to_destination$name
          point_to_point_table_overview["distance_km"] <- round((sum(point_to_point$trip_details$distance)) / 1000, digits = 2)
          point_to_point_table_overview["startTime"] <- time_twenty_four
        } else {
          # Cannot find journey
          point_to_point_table_overview <- data.frame(
            "start" = NA,
            "end" = NA,
            "duration" = NA,
            "walkTime" = NA,
            "transitTime" = NA,
            "waitingTime" = NA,
            "transfers" = NA,
            "origin" = from_origin$name,
            "destination" = to_destination$name,
            "distance_km" = NA,
            "startTime" = time_twenty_four)
        }
        
      } else {
        point_to_point_table_overview <- data.frame(
          "start" = NA,
          "end" = NA,
          "duration" = NA,
          "walkTime" = NA,
          "transitTime" = NA,
          "waitingTime" = NA,
          "transfers" = NA,
          "origin" = from_origin$name,
          "destination" = to_destination$name,
          "distance_km" = NA,
          "startTime" = time_twenty_four)
      }
      
    } else {
      
      if (!is.null(point_to_point$errorId)){
        
        if (point_to_point$errorId == "OK") {
          point_to_point_table_overview_tmp <- point_to_point$itineraries
          point_to_point_table_overview_tmp$origin <- from_origin$name
          point_to_point_table_overview_tmp$destination <- to_destination$name
          point_to_point_table_overview_tmp$distance_km <- round((sum(point_to_point$trip_details$distance)) / 1000, digits = 2)
          point_to_point_table_overview_tmp$startTime <- time_twenty_four
        } else {
          point_to_point_table_overview_tmp <- data.frame(
            "start" = NA,
            "end" = NA,
            "duration" = NA,
            "walkTime" = NA,
            "transitTime" = NA,
            "waitingTime" = NA,
            "transfers" = NA,
            "origin" = from_origin$name,
            "destination" = to_destination$name,
            "distance_km" = NA,
            "startTime" = time_twenty_four)
        }
        
        point_to_point_table_overview <- rbind(point_to_point_table_overview, point_to_point_table_overview_tmp)
      } else {
        point_to_point_table_overview_tmp <- data.frame(
          "start" = NA,
          "end" = NA,
          "duration" = NA,
          "walkTime" = NA,
          "transitTime" = NA,
          "waitingTime" = NA,
          "transfers" = NA,
          "origin" = from_origin$name,
          "destination" = to_destination$name,
          "distance_km" = NA,
          "startTime" = time_twenty_four)
        point_to_point_table_overview <- rbind(point_to_point_table_overview, point_to_point_table_overview_tmp)
      }
    }
    
    end.time <- Sys.time()
    time.taken[num.run] <- round(end.time - start.time, digits = 2)
    
    if (num.run < num.total) {
      message(
        num.run,
        " out of ",
        num.total,
        " connections complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * num.total
        ) - sum(time.taken),
        digits = 2),
        " seconds.")
    } else {
      message(
        num.run,
        " out of ",
        num.total,
        " connections complete. Time taken ",
        sum(time.taken),
        " seconds.\n")
    }
  }
  
  ######################
  #### SAVE RESULTS ####
  ######################
  
  message("Analysis complete, now saving outputs to ", output.dir, ", please wait.\n")
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  
  for (i in 1:nrow(point_to_point_table_overview)) {
    
    if (i == 1) {
      point_to_point_table_overview[i,"date"] = format(time_series[i], "%m/%d/%Y")
    } else {
      
      if (format(time_series[i], "%I:%M %p") >= format(time_series[i-1], "%I:%M %p")) {
        point_to_point_table_overview$date[i] = point_to_point_table_overview[i - 1,"date"]
      } else {
        point_to_point_table_overview$date[i] = format(as.Date(strptime(point_to_point_table_overview[i - 1,"date"], "%m/%d/%Y")) + 1, "%m/%d/%Y")
      }
    }
  }
  
  point_to_point_table_overview <- point_to_point_table_overview[, c(8, 9, 12, 11, 1, 2, 10, 3, 4, 5, 6, 7)]
  
  colnames(point_to_point_table_overview) <-
    c(
      "origin",
      "destination",
      "date",
      "calc_start_time",
      "journey_start_time",
      "journey_end_time",
      "distance_km",
      "duration_mins",
      "walk_time_mins",
      "transit_time_mins",
      "waiting_time_mins",
      "transfers")
  
  write.csv(
    point_to_point_table_overview,
    file = paste0(output.dir, "/pointToPointTime-", stamp, ".csv"),
    row.names = F) 
  
  if (mapOutput == T) {
    library(dplyr)
    list.files(
      path = paste0(output.dir, "/tmp_folder"),
      pattern = "*.png",
      full.names = T
    ) %>% 
      purrr::map(magick::image_read) %>%
      magick::image_join() %>%
      magick::image_animate(fps = 5) %>% 
      magick::image_write(paste0(output.dir, "/pointToPointTime-", stamp, ".gif"))
    
    m <-
      magick::image_read(paste0(output.dir, "/pointToPointTime-", stamp, ".gif")) %>%
      magick::image_scale("600") 
  
    invisible(print(m)) 
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
  message("Thanks for using propeR.")
}
