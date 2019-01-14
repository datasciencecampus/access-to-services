##' Calculates the journey between for a single origin and destination between a
##' start and end time and date.
##'
##' Calculates the journey time and details between a single origin and destination between a start
##' and end time and date. Default is to save a CSV file of journey details. Optional output is an animated
##' map of the polyline between origin and destination, which also saves as a GIF.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
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
##' @param preWaitTime in minutes, defaults to 60
##' @param map specify whether you want to output a map
##' @param transportColours A list defining the colours to assign to each mode of transport.
##' @param mapZoom defaults to 12
##' @return Saves an animated map as a gif and journey details as CSV to output directory
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
                             preWaitTime = 60,
                             # leaflet map args
                             map = FALSE,
                             transportColours = list(
                               TRANSIT = "#000000",
                               WALK = "#A14296",
                               BUS = "#48C1B1",
                               RAIL = "#4D7BC5",
                               CAR = "#8D4084",
                               BICYCLE = "#4AA6C3"
                             ),
                             mapZoom = 12) {
  message("Now running the propeR pointToPointTime tool.\n")
  
  if (map == TRUE) {
    pal_transport <-
      leaflet::colorFactor(
        palette = unlist(transportColours, use.names = F),
        # Creating colour palette
        levels = as.factor(names(transportColours)),
        reverse = FALSE
      )
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL) # Creating colour palette
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    dir.create(paste0(output.dir, "/tmp_folder")) # Creates tmp_folder for pngs
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <-
    originPointsRow
  
  destination_points_row_num <-
    destinationPointsRow
  
  if (origin_points_row_num > nrow(originPoints)) {
    message('Row is not in origin file, process aborted.\n')
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break #todo: need a better system to stop code as break shouldn't be used outside a loop
  }
  
  if (destination_points_row_num > nrow(destinationPoints)) {
    message('Row is not in destination file, process aborted.\n')
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break #todo: need a better system to stop code as break shouldn't be used outside a loop
  }
  
  from_origin <-
    originPoints[origin_points_row_num, ]
  
  to_destination <-
    destinationPoints[destination_points_row_num, ]
  
  start_date <- as.Date(startDateAndTime) # Sets start date
  
  time_series = seq(as.POSIXct(startDateAndTime),
                    as.POSIXct(endDateAndTime),
                    by = (timeIncrease) * 60) # Creates a time series between start and end dates and times based on the increment in time
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
  message("Creating ",
          length(time_series),
          " point to point connections, please wait...")
  
  time.taken <- vector()
  
  for (i in 1:length(time_series)) {
    
    start.time <- Sys.time()
    
    stamp <-
      format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
    
    if (map == TRUE) {
      date_time_legend <-
        format(time_series[i], "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
    }
    
    time <- format(time_series[i], "%I:%M %p")
    
    time_twenty_four <- strftime(as.POSIXct(time_series[i], origin = "1970-01-01"),
      format = "%H:%M:%S")
    
    date <- as.Date(time_series[i])
    
    point_to_point <- propeR::otpTripTime(
      otpcon,
      detail = TRUE,
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
    
    point_to_point_table <-
      point_to_point$output_table
    
    #########################
    #### OPTIONAL EXTRAS ####
    #########################
    
    if (map == TRUE) {
      if (!is.null(point_to_point_table)) {
        poly_lines <-
          point_to_point$poly_lines 
        poly_lines <-
          sp::spTransform(poly_lines, sp::CRS("+init=epsg:4326"))
        
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
            color = ~ pal_transport(poly_lines$mode),
            weight = 5,
            opacity = 1
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
            fillOpacity = 1,
            popup = ~ mode
          )
        m <-
          addLegend(
            m,
            pal = pal_transport,
            # Adds a legend for the trip
            values = point_to_point_table$mode,
            opacity = 1.0,
            title = "Transport Mode"
          )
        m <- addLegend(
          m,
          pal = pal_time_date,
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
        
        remove(poly_lines) # Deletes poly_lines for next loop
        
      } else {
        # If no journey is found
        
        m <- leaflet()
        m <- addTiles(m, group = "leaf")
        m <- addScaleBar(m)
        m <-
          setView(
            m,
            lat = (from_origin$lat + to_destination$lat) / 2,
            # Focuses on midpoint between origin and destination
            lng = (from_origin$lon + to_destination$lon) / 2,
            # Focuses on midpoint between origin and destination
            zoom = 12
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
              markerColor = "blue",
              iconColor = "white",
              library = "fa"
            )
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
              markerColor = "orange",
              iconColor = "white",
              library = "fa"
            )
          )
        m <- addLegend(
          m,
          pal = pal_time_date,
          values = date_time_legend,
          position = "bottomleft",
          title = "Date and Time"
        )
      }
      
      mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/", stamp, ".png")) # Saves map in temp folder
      
    }
    
    if (i == 1) {
      
      if (point_to_point$errorId == "OK") {
        
        point_to_point_table_overview <- point_to_point$itineraries
        
        
        point_to_point_table_overview["origin"] <- 
          from_origin$name
        point_to_point_table_overview["destination"] <-
          to_destination$name
        point_to_point_table_overview["status"] <-
          point_to_point$errorId
        point_to_point_table_overview["distance_km"] <-
          round((sum(
            point_to_point$trip_details$distance
          )) / 1000, digits = 2)
      } else {
        # Cannot find journey
        point_to_point_table_overview <- data.frame(
          "start" = time_twenty_four,
          "end" = NA,
          "duration" = NA,
          "walkTime" = NA,
          "transitTime" = NA,
          "waitingTime" = NA,
          "transfers" = NA,
          "origin" = from_origin$name,
          "destination" = to_destination$name,
          "status" = point_to_point$errorId,
          "distance_km" = NA
        )
        
        
        # point_to_point_table_overview["start"] <- 'N/A'
        # point_to_point_table_overview["end"] <- 'N/A'
        # point_to_point_table_overview["duration"] <- 'N/A'
        # point_to_point_table_overview["walkTime"] <- 'N/A'
        # point_to_point_table_overview["transitTime"] <-
        #   'N/A'
        # point_to_point_table_overview["waitingTime"] <-
        #   'N/A'
        # point_to_point_table_overview["transfers"] <- 'N/A'
        # point_to_point_table_overview["origin"] <-
        #   from_origin$name
        # point_to_point_table_overview["destination"] <-
        #   to_destination$name
        # point_to_point_table_overview["status"] <-
        #   point_to_point$errorId
        # point_to_point_table_overview["distance_km"] < 'N/A'
      }
      
    } else {
      
      if (point_to_point$errorId == "OK") {
        
      point_to_point_table_overview_tmp <- point_to_point$itineraries
      point_to_point_table_overview_tmp$origin <- from_origin$name
      point_to_point_table_overview_tmp$destination <-
        to_destination$name
      point_to_point_table_overview_tmp$status <-
        point_to_point$errorId
      point_to_point_table_overview_tmp$distance_km <-
        round((sum(
          point_to_point$trip_details$distance
        )) / 1000, digits = 2)
      
      print(point_to_point_table_overview_tmp)
      } else {
        
        point_to_point_table_overview_tmp <- data.frame(
          "start" = time_twenty_four,
          "end" = NA,
          "duration" = NA,
          "walkTime" = NA,
          "transitTime" = NA,
          "waitingTime" = NA,
          "transfers" = NA,
          "origin" = from_origin$name,
          "destination" = to_destination$name,
          "status" = point_to_point$errorId,
          "distance_km" = NA
        )
        
        # point_to_point_table_overview_tmp["start"] <- 'N/A'
        # point_to_point_table_overview_tmp["end"] <- 'N/A'
        # point_to_point_table_overview_tmp["duration"] <- 'N/A'
        # point_to_point_table_overview_tmp["walkTime"] <- 'N/A'
        # point_to_point_table_overview_tmp["transitTime"] <-
        #   'N/A'
        # point_to_point_table_overview_tmp["waitingTime"] <-
        #   'N/A'
        # point_to_point_table_overview_tmp["transfers"] <- 'N/A'
        # point_to_point_table_overview_tmp["origin"] <-
        #   from_origin$name
        # point_to_point_table_overview_tmp["destination"] <-
        #   to_destination$name
        # point_to_point_table_overview_tmp["status"] <-
        #   point_to_point$errorId
        # point_to_point_table_overview_tmp["distance_km"] < 'N/A'
        
      }
      
      
      point_to_point_table_overview <-
        rbind(point_to_point_table_overview,
              point_to_point_table_overview_tmp)
      
      print(point_to_point_table_overview)
    }
    
    end.time <- Sys.time()
    
    time.taken[i] <- round(end.time - start.time, digits = 2)
    
    if (i < length(time_series)) {
      message(
        i,
        " out of ",
        length(time_series),
        " connections complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * length(time_series)
        ) - sum(time.taken),
        digits = 2),
        " seconds."
      )
    } else {
      message(
        i,
        " out of ",
        length(time_series),
        " connections complete. Time taken ",
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
  
  for (i in 1:nrow(point_to_point_table_overview)) {
    if (i == 1) {
      point_to_point_table_overview[i,"date"] = format(time_series[i], "%m/%d/%Y")
    } else {
      if (point_to_point_table_overview[i,"start"] >= point_to_point_table_overview[i - 1,"start"]) {
        point_to_point_table_overview$date[i] = point_to_point_table_overview[i - 1,"date"]
      } else {
        point_to_point_table_overview$date[i] = format(as.Date(
          strptime(point_to_point_table_overview[i - 1,"date"], "%m/%d/%Y")
        ) + 1, "%m/%d/%Y")
      }
    }
  }
  
  point_to_point_table_overview <-
    point_to_point_table_overview[, c(10, 8, 9, 12, 1, 2, 11, 3, 4, 5, 6, 7)]
  
  colnames(point_to_point_table_overview) <-
    c(
      "status",
      "origin",
      "destination",
      "date",
      "start_time",
      "end_time",
      "distance_km",
      "duration_mins",
      "walk_time_mins",
      "transit_time_mins",
      "waiting_time_mins",
      "transfers"
    )
  
  write.csv(
    point_to_point_table_overview,
    file = paste0(output.dir, "/p2p_time-", stamp, ".csv"),
    row.names = FALSE
  ) # Saves trip details as a CSV
  
  if (map == TRUE) {
    library(dplyr)
    list.files(
      path = paste0(output.dir, "/tmp_folder"),
      pattern = "*.png",
      full.names = T
    ) %>%  # Creates gif of results
      purrr::map(magick::image_read) %>% # reads each path file
      magick::image_join() %>%  # joins image
      magick::image_animate(fps = 5) %>%  # animates, can opt for number of loops
      magick::image_write(paste0(output.dir, "/p2p_time-", stamp, ".gif")) # write to current dir
    
    m <-
      magick::image_read(paste0(output.dir, "/p2p_time-", stamp, ".gif")) %>%
      magick::image_scale("600") # Loads GIF into R
    
    invisible(print(m)) # plots map to Viewer
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  }
  message("Thanks for using propeR.")
  point_to_point_table_overview
}
