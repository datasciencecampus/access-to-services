##' Calculates the journey between for a single origin and destination between a
##' start and end time and date. 
##'
##' Calculates the journey time and details between a single origin and destination between a start
##' and end time and date. Outputs (and saves) a animated gif map of the polyline between origin and 
##' destination, as well as a CSV file of journey details.
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
                             originPointsRow=1,
                             destinationPoints,
                             destinationPointsRow=1,
                             # otpTime args
                             startDateAndTime="2018-08-18 12:00:00",
                             endDateAndTime="2018-08-18 15:00:00",
                             timeIncrease = 60,
                             modes="WALK, TRANSIT",
                             maxWalkDistance=1000,
                             walkReluctance=2,
                             walkSpeed=1.5,
                             bikeSpeed=5,
                             minTransferTime=1,
                             maxTransfers=5,
                             wheelchair=F,
                             arriveBy=F,
                             preWaitTime=60,
                             # colours
                             transportColours=list(TRANSIT="#000000", WALK="#A14296", BUS="#48C1B1", RAIL="#4D7BC5", CAR="#8D4084", BICYCLE="#4AA6C3"),
                             # leaflet map args
                             mapZoom=12) {
  
  message("Now running the propeR pointToPointTime tool.\n")  
  
  pal_transport <- colorFactor(palette=unlist(transportColours, use.names=F), # Creating colour palette
                               levels=as.factor(names(transportColours)),
                               reverse=FALSE)
  
  pal_time_date=colorFactor(c("#FFFFFF"), domain=NULL) # Creating colour palette
  
  dir.create(paste0(output.dir,"/tmp_folder")) # Creates tmp_folder for pngs
  
  origin_points_row_num <- originPointsRow # Set origin using a row from the origin dataframe 
  destination_points_row_num <- destinationPointsRow # Set destination using a row from the origin dataframe
  
  if (origin_points_row_num > nrow(originPoints)){
    message('Row is not in origin file, process aborted.\n')
    unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break
  }
  
  if (destination_points_row_num > nrow(destinationPoints)){
    message('Row is not in destination file, process aborted.\n')
    unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break
  }
  
  from_origin <- originPoints[origin_points_row_num,] # Takes the specified row from the data
  to_destination <- destinationPoints[destination_points_row_num,] # Takes the specified row from the data
  
  # Tidying variables ----------
  start_date <- as.Date(startDateAndTime) # Sets start date
  time_series = seq(as.POSIXct(startDateAndTime), as.POSIXct(endDateAndTime), by = (timeIncrease)*60) # Creates a time series between start and end dates and times based on the increment in time
  
  message("Creating ",length(time_series)," point to point connections, please wait...")
  time.taken <- list(0)
  
  for (i in 1:length(time_series)){ # Start loop to calculate journey details
    
    start.time <- Sys.time()
    
    date_time_legend <- format(time_series[i], "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
    time <- format(time_series[i], "%I:%M %p")
    date <- as.Date(time_series[i])
    
    point_to_point <- propeR::otpTripTime(
      otpcon,
      detail = TRUE, # Gives full breakdown of journey if TRUE
      from = from_origin$lat_lon, # Takes the latitude and longitude from specified origin
      to = to_destination$lat_lon, # Takes the latitude and longitude from specified destination
      modes = modes,
      date = date, # Takes the date as specified above
      time = time, # Takes the time as specified above
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
    
    if (!is.null(point_to_point_table)){ # Creates results and maps based on whether journey is found or not
      poly_lines <- point_to_point$poly_lines # The SpatialLinesDataFrame required to create polylines for leaflet
      poly_lines <- sp::spTransform(poly_lines, sp::CRS("+init=epsg:4326"))
      
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
      
      remove(poly_lines) # Deletes poly_lines for next loop
      
    } else {  # If no journey is found
      
      m <- leaflet() 
      m <- addTiles(m, group = "leaf") 
      m <- addScaleBar(m) 
      m <- setView(m, lat=(from_origin$lat+to_destination$lat)/2, # Focuses on midpoint between origin and destination
                   lng=(from_origin$lon+to_destination$lon)/2, # Focuses on midpoint between origin and destination
                   zoom=12) 
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
                     values = date_time_legend,
                     position = "bottomleft",
                     title = "Date and Time")
    }
    
    mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/",gsub(":","",i,ignore.case = TRUE),".png")) # Saves map in temp folder
    
    if (i == 1){
      point_to_point_output_table <- point_to_point$itineraries[1,]
    } else {
      point_to_point_output_table <- rbind(point_to_point_output_table,point_to_point$itineraries[1,])
    }
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits=2)
    
    if (i < length(time_series)){
      message(i," out of ",length(time_series),
              " connections complete. Time taken ",
              do.call(sum,time.taken)," seconds. Estimated time left is approx. ",
              (do.call(mean,time.taken)*length(time_series))-do.call(sum,time.taken),
              " seconds.")
    } else {
      message(i," out of ",length(time_series),
              " connections complete. Time taken ",
              do.call(sum,time.taken)," seconds.")
    }
  }
  
  # Plots leaflet map gif in Viewer and saves to disk, also saves table as csv ---------- 
  
  message("Analysis complete, now saving outputs to ",output.dir,", please wait.\n")  
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
  
  library(dplyr)
  list.files(path = paste0(output.dir,"/tmp_folder"), pattern = "*.png", full.names = T) %>%  # Creates gif of results
    purrr::map(magick::image_read) %>% # reads each path file
    magick::image_join() %>%  # joins image
    magick::image_animate(fps=5) %>%  # animates, can opt for number of loops
    magick::image_write(paste0(output.dir,"/p2p_time-",stamp,".gif")) # write to current dir
  
  m <- magick::image_read(paste0(output.dir,"/p2p_time-",stamp,".gif")) %>%
    magick::image_scale("600") # Loads GIF into R
  
  invisible(print(m)) # plots map to Viewer
  
  for (i in 1:nrow(point_to_point_output_table)){
    if (i == 1){
      point_to_point_output_table$date[i] = format(time_series[i], "%m/%d/%Y")
    } else {
      if (point_to_point_output_table$start[i]>point_to_point_output_table$start[i-1]){
        point_to_point_output_table$date[i] = point_to_point_output_table$date[i-1]
      } else {
        point_to_point_output_table$date[i] = format(as.Date(strptime(point_to_point_output_table$date[i-1], "%m/%d/%Y"))+1, "%m/%d/%Y")
      }
    }
    
  }
  
  point_to_point_output_table$date_time <- paste0(point_to_point_output_table$date, " ", point_to_point_output_table$start)
  
  write.csv(point_to_point_output_table, file = paste0(output.dir,"/p2p_time-",stamp,".csv"),row.names=FALSE) # Saves trip details as a CSV
  
  unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  
  # todo: get this to work!
  # png(paste0(output.dir,"/p2p_time_graph-",stamp,".png"))
  # ggplot2::theme_set(ggplot2::theme_bw())
  # p <- ggplot2::ggplot(aes(x = date_time, y = duration, group = 1), data = point_to_point_output_table) + geom_area() + geom_line()
  # p <- p + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) 
  # p <- p + labs(x = "") 
  # p <- p + labs(y = "Duration (minutes)")
  # invisible(print(p))
  # dev.off()
}
