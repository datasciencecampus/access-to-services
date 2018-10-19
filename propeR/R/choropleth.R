##' Calculates an isochrone map from a single origin
##'
##' .. content for \details{} ..
##'
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPolygons The variable containing origin(s) polygon(s), see ?importGeojsonData
##' @param destinationPoints The variable containing destination(s), see ?importLocationData
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
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
##' @param durationCutoff in minutes, defaults to 60
##' @param waitingCutoff in minutes, defaults to 10
##' @param transferCutoff defaults to 1
##' @param palColor the color palette of the map, defaults to 'Blues'
##' @param palColorCat the color palette of the catergorical map, defaults to c("#820e0e", "#407746")
##' @param mapZoom defaults to 12
##' @return Returns a number of maps (duration, wait time, transfers) to the output directory
##' @author Michael Hodge
##' @examples
##'   choropleth(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     originPolygons,
##'     destinationPoints,
##'     startdDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
choropleth <- function(output.dir,
                       otpcon,
                       originPoints,
                       originPolygons,
                       destinationPoints,
                       destinationPointsRow=1,
                       # otpChoropleth args
                       startDateAndTime="2018-08-18 12:00:00",
                       modes="WALK, TRANSIT",
                       maxWalkDistance=1000,
                       walkReluctance=2,
                       walkSpeed=1.5,
                       bikeSpeed=5,
                       minTransferTime=1,
                       maxTransfers=5,
                       wheelchair=F,
                       arriveBy=F,
                       # func specific args (originally in config.R)
                       durationCutoff=60,
                       waitingCutoff=10,
                       transferCutoff=1,
                       # colours
                       palColor="Blues",
                       palColorCat=c("#820e0e", "#407746"),
                       # leaflet map args
                       mapZoom=12) {
  
  message("Now running the propeR choropleth tool.\n")  
  
  # colours
  pal_choropleth = leaflet::colorNumeric(palColor, domain=NULL, na.color = "#ffffff") # Creating colour palette
  pal_choropleth_transfers = leaflet::colorFactor(palColor, domain=NULL, na.color = "#ffffff") # Creating colour palette
  pal_choropleth_cat = leaflet::colorFactor(palColorCat, na.color = "#ffffff", domain=NULL) # Creating colour palette
  
  dir.create(paste0(output.dir,"/tmp_folder")) # Creates tmp_folder for pngs
  
  # Set Origin and Destination if multiple are in csv ----------
  destination_points_row_num <- destinationPointsRow # Set destination using a row from the origin dataframe
  
  if (destination_points_row_num > nrow(destinationPoints)){
    message('Row is not in destination file')
    unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
    break
  }
  
  to_destination <- destinationPoints[destination_points_row_num,] # Takes the specified row from the data
  
  # Tidying variables ----------
  start_time <- format(as.POSIXct(startDateAndTime), "%I:%M %p") # Sets start time
  start_date <- as.Date(startDateAndTime) # Sets start date
  date_time_legend <- format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
  
  message("Creating ",nrow(originPoints)," point to point connections, please wait...")
  time.taken <- list(0)
  
  for (i in 1:nrow(originPoints)) { # Loops for the number of origin points
    
    start.time <- Sys.time()
    
    from_origin <- originPoints[i,]
    
    choropleth <- propeR::otpChoropleth(
      otpcon,
      detail = TRUE, # Gives full breakdown of journey if TRUE
      from = from_origin$lat_lon, # Takes the latitude and longitude from specified origin
      to = to_destination$lat_lon, # Takes the latitude and longitude from specified destination
      modes = modes, 
      date = start_date, # Takes the date as specified above
      time = start_time, # Takes the time as specified above
      maxWalkDistance = maxWalkDistance, 
      walkReluctance = walkReluctance, 
      walkSpeed = walkSpeed, 
      bikeSpeed = bikeSpeed,
      minTransferTime = minTransferTime,
      maxTransfers = maxTransfers,
      wheelchair = wheelchair,
      arriveBy = arriveBy
    )
    
    if (i == 1){ # Start appending results
      choropleth_result <- choropleth$itineraries
    } else {
      choropleth_result = rbind(choropleth_result,choropleth$itineraries)
    }
    
    # If response is OK update origin dataframe with results
    if (choropleth$errorId == "OK") {
      originPoints[i, "status"] <- choropleth$errorId
      originPoints[i, "duration"] <- choropleth$itineraries$duration
      originPoints[i, "waitingtime"] <- choropleth$itineraries$waitingTime
      originPoints[i, "transfers"] <-choropleth$itineraries$transfers
      
      if (originPoints[i, "duration"] > (durationCutoff)) { # Apply duration cutoff
        originPoints[i, "duration_cat"] <- sprintf('Over %s minutes', durationCutoff)
      } else {
        originPoints[i, "duration_cat"] <- sprintf('Under %s minutes', durationCutoff)
      }
      
      if (originPoints[i, "waitingtime"] > (waitingCutoff)) { # Apply waiting time cutoff
        originPoints[i, "waitingtime_cat"] <- sprintf('Over %s minutes', waitingCutoff)
      } else {
        originPoints[i, "waitingtime_cat"] <- sprintf('Under %s minutes', waitingCutoff)
      }        
      
      if (originPoints[i, "transfers"] >= (transferCutoff)) { # Apply transfers cutoff
        originPoints[i, "transfers_cat"] <- sprintf('Over %s transfer(s)', transferCutoff)
      } else {
        originPoints[i, "transfers_cat"] <- sprintf('Under %s transfer(s)', transferCutoff)
      }
      
    } else {
      # Cannot find journey
      originPoints[i, "status"] <- choropleth$errorId
    }
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits=2)
    
    if (i < nrow(originPoints)){
      message(i," out of ",nrow(originPoints),
              " connections complete. Time taken ",
              round(do.call(sum,time.taken), digits = 2)," seconds. Estimated time left is approx. ",
              round((do.call(mean,time.taken)*nrow(originPoints))-do.call(sum,time.taken), digits = 2),
              " seconds.")
    } else {
      message(i," out of ",nrow(originPoints),
              " connections complete. Time taken ",
              do.call(sum,time.taken)," seconds.")
    }
    
  }
  
  # todo: this needs refactoring. lots of code duplication.
  
  colnames(originPolygons@data)[3] = "name"
  choropleth_map <- sp::merge(originPolygons, originPoints[,c('name','duration','waitingtime','transfers','duration_cat','waitingtime_cat','transfers_cat')], by=c("name")) # Merges the information from the input polygon and the function called
  choropleth_table <- originPoints[,c('name','duration','waitingtime','transfers','duration_cat','waitingtime_cat','transfers_cat')] # Creates a table of information based on the function
  
  popup_duration <- # Creates a popup for duration time for leaflet
    paste0("<strong>Name: </strong>",
           choropleth_map$name,
           "<br><strong>Duration: </strong>",
           choropleth_map$duration, " mins")
  
  popup_waitingtime <- # Creates a popup for waiting time for leaflet
    paste0("<strong>Name: </strong>",
           choropleth_map$name,
           "<br><strong>Waiting: </strong>",
           choropleth_map$waitingtime, " mins")
  
  popup_transfers <- # Creates a popup for transfers for leaflet
    paste0("<strong>Name: </strong>",
           choropleth_map$name,
           "<br><strong>Transfers: </strong>",
           choropleth_map$transfers)
  
  # Creating a leaflet map for duration
  library(leaflet)
  m <- leaflet()
  m <- addScaleBar(m)
  m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
  m <- setView(m, lat=to_destination$lat, # Focuses on destination
               lng=to_destination$lon, # Focuses on destination
               zoom=mapZoom)
  m <- addPolygons(m, data = choropleth_map, # Adds polygons for origins
                   fillColor = ~pal_choropleth(choropleth_map$duration),
                   fillOpacity = 1,
                   color = "#BDBDC3",
                   weight = 1,
                   popup = popup_duration)
  m <- addLegend(m, pal = pal_choropleth, # Adds legend
                 values = choropleth_map$duration,
                 opacity = 1.0,
                 bins = 5,
                 na.label = "NA",
                 title = "Duration (minutes)")
  m <- addAwesomeMarkers(m, data = to_destination, # Adds marker for destination
                         lat = ~lat,
                         lng = ~lon,
                         popup = ~name,
                         icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                markerColor = 'red',
                                                iconColor = 'white',
                                                library = "fa"))
  
  
  m_cat <- leaflet()
  m_cat <- addScaleBar(m_cat)
  m_cat <- addProviderTiles(m_cat, providers$OpenStreetMap.BlackAndWhite)
  m_cat <- setView(m_cat, lat=to_destination$lat, # Focuses on destination
                   lng=to_destination$lon, # Focuses on destination
                   zoom=mapZoom)
  m_cat <- addPolygons(m_cat, data = choropleth_map, # Adds polygons for origins
                       fillColor = ~pal_choropleth_cat(choropleth_map$duration_cat),
                       fillOpacity = 1,
                       color = "#BDBDC3",
                       weight = 1,
                       popup = popup_duration)
  m_cat <- addLegend(m_cat, pal = pal_choropleth_cat, # Adds legend
                     values = choropleth_map$duration_cat,
                     opacity = 1.0,
                     title = "Duration (minutes)")
  m_cat <- addAwesomeMarkers(m_cat, data = to_destination, # Adds marker for destination
                             lat = ~lat,
                             lng = ~lon,
                             popup = ~name,
                             icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                    markerColor = 'red',
                                                    iconColor = 'white',
                                                    library = "fa"))
  
  # Creating a leaflet map for waiting time
  
  n <- leaflet()
  n <- addScaleBar(n)
  n <- addProviderTiles(n, providers$OpenStreetMap.BlackAndWhite)
  n <- setView(n, lat=to_destination$lat, # Focuses on destination
               lng=to_destination$lon, # Focuses on destination
               zoom=mapZoom)
  n <- addPolygons(n, data = choropleth_map, # Adds polygons for origins
                   fillColor = ~pal_choropleth(choropleth_map$waitingtime),
                   fillOpacity = 1,
                   color = "#BDBDC3",
                   weight = 1,
                   popup = popup_waitingtime)
  n <- addLegend(n, pal = pal_choropleth, # Adds legend
                 values = choropleth_map$waitingtime,
                 opacity = 1.0,
                 bins = 5,
                 na.label = "NA",
                 title = "Wait Time (minutes)")
  n <- addAwesomeMarkers(n, data = to_destination, # Adds marker for destination
                         lat = ~lat,
                         lng = ~lon,
                         popup = ~name,
                         icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                markerColor = 'red',
                                                iconColor = 'white',
                                                library = "fa"))
  
  
  n_cat <- leaflet()
  n_cat <- addScaleBar(n_cat)
  n_cat <- addProviderTiles(n_cat, providers$OpenStreetMap.BlackAndWhite)
  n_cat <- setView(n_cat, lat=to_destination$lat, # Focuses on destination
                   lng=to_destination$lon, # Focuses on destination
                   zoom=mapZoom)
  n_cat <- addPolygons(n_cat, data = choropleth_map, # Adds polygons for origins
                       fillColor = ~pal_choropleth_cat(choropleth_map$waitingtime_cat),
                       fillOpacity = 1,
                       color = "#BDBDC3",
                       weight = 1,
                       popup = popup_waitingtime)
  n_cat <- addLegend(n_cat, pal = pal_choropleth_cat, # Adds legend
                     values = choropleth_map$waitingtime_cat,
                     opacity = 1.0,
                     title = "Wait Time (minutes)")
  n_cat <- addAwesomeMarkers(n_cat, data = to_destination, # Adds marker for destination
                             lat = ~lat,
                             lng = ~lon,
                             popup = ~name,
                             icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                    markerColor = 'red',
                                                    iconColor = 'white',
                                                    library = "fa"))
  
  # Creating a leaflet map for transfers
  
  o <- leaflet()
  o <- addScaleBar(o)
  o <- addProviderTiles(o, providers$OpenStreetMap.BlackAndWhite)
  o <- setView(o, lat=to_destination$lat, # Focuses on destination
               lng=to_destination$lon, # Focuses on destination
               zoom=mapZoom)
  o <- addPolygons(o, data = choropleth_map, # Adds polygons for origins
                   fillColor = ~pal_choropleth_transfers(choropleth_map$transfers),
                   fillOpacity = 1,
                   color = "#BDBDC3",
                   weight = 1,
                   popup = popup_transfers)
  o <- addLegend(o, pal = pal_choropleth_transfers, # Adds legend
                 values = choropleth_map$transfers,
                 opacity = 1.0,
                 na.label = "NA",
                 title = "Transfers")
  o <- addAwesomeMarkers(o, data = to_destination, # Adds marker for destination
                         lat = ~lat,
                         lng = ~lon,
                         popup = ~name,
                         icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                markerColor = 'red',
                                                iconColor = 'white',
                                                library = "fa"))
  
  
  o_cat <- leaflet()
  o_cat <- addScaleBar(o_cat)
  o_cat <- addProviderTiles(o_cat, providers$OpenStreetMap.BlackAndWhite)
  o_cat <- setView(o_cat, lat=to_destination$lat, # Focuses on destination
                   lng=to_destination$lon, # Focuses on destination
                   zoom=mapZoom)
  o_cat <- addPolygons(o_cat, data = choropleth_map, # Adds polygons for origins
                       fillColor = ~pal_choropleth_cat(choropleth_map$transfers_cat),
                       fillOpacity = 1,
                       color = "#BDBDC3",
                       weight = 1,
                       popup = popup_transfers)
  o_cat <- addLegend(o_cat, pal = pal_choropleth_cat, # Adds legend
                     values = choropleth_map$transfers_cat,
                     opacity = 1.0,
                     title = "Transfers")
  o_cat <- addAwesomeMarkers(o_cat, data = to_destination, # Adds marker for destination
                             lat = ~lat,
                             lng = ~lon,
                             popup = ~name,
                             icon = makeAwesomeIcon(icon= 'hourglass-end',
                                                    markerColor = 'red',
                                                    iconColor = 'white',
                                                    library = "fa"))
  
  
  # Plots leaflet map in Viewer and saves to disk, also saves table as csv ----------

  message("Analysis complete, now saving outputs to ",output.dir,", please wait.\n")

  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp

  mapview::mapshot(m, file = paste0(output.dir, "/choropleth_duration-",stamp,".png"))
  htmlwidgets::saveWidget(m, file = paste0(output.dir, "/choropleth_duration-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_duration-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(m)) # plots map to Viewer

  mapview::mapshot(m_cat, file = paste0(output.dir, "/choropleth_duration_cat-",stamp,".png"))
  htmlwidgets::saveWidget(m_cat, file = paste0(output.dir, "/choropleth_duration_cat-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_duration_cat-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(m_cat)) # plots map to Viewer

  mapview::mapshot(n, file = paste0(output.dir, "/choropleth_waitingtime-",stamp,".png"))
  htmlwidgets::saveWidget(n, file = paste0(output.dir, "/choropleth_waitingtime-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_waitingtime-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(n)) # plots map to Viewer

  mapview::mapshot(n_cat, file = paste0(output.dir, "/choropleth_waitingtime_cat-",stamp,".png"))
  htmlwidgets::saveWidget(n_cat, file = paste0(output.dir, "/choropleth_waitingtime_cat-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_waitingtime_cat-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(n_cat)) # plots map to Viewer

  mapview::mapshot(o, file = paste0(output.dir, "/choropleth_transfers-",stamp,".png"))
  htmlwidgets::saveWidget(o, file = paste0(output.dir, "/choropleth_transfers-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_transfers-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(o)) # plots map to Viewer

  mapview::mapshot(o_cat, file = paste0(output.dir, "/choropleth_transfers_cat-",stamp,".png"))
  htmlwidgets::saveWidget(o_cat, file = paste0(output.dir, "/choropleth_transfers_cat-",stamp,".html")) # Saves as an interactive HTML webpage
  unlink(paste0(output.dir, "/choropleth_transfers_cat-",stamp,"_files"), recursive = TRUE) # Deletes temporary folder that mapshot creates
  invisible(print(o_cat)) # plots map to Viewer

  write.csv(choropleth_table, file = paste0(output.dir, "/choropleth-",stamp,".csv"),row.names=FALSE) # Saves trip details as a CSV

  unlink(paste0(output.dir,"/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  
}
