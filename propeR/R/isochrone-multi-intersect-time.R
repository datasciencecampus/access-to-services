##' Calculates an isochrone map for multiple origins.
##'
##' Finds the intersection between a start and end time and date.
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
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
##' @param isochroneCutOffs in minutes, defaults to 60
##' @param palColorMarkers the color palette of the markers, defaults to 'Greys'
##' @param palColorPolygon the color palette of the poygon, defaults to 'Blue'
##' @param mapZoom defaults to 12
##' @return Saves an animated map as a gif to output directory
##' @author Michael Hodge
##' @examples
##'   isochroneMultiIntersect(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
isochroneMultiIntersectTime <- function(output.dir,
                                        otpcon,
                                        originPoints,
                                        destinationPoints,
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
                                        isochroneCutOffs = 60,
                                        # colours
                                        palColorMarker = "Greys",
                                        palColorPolygon = "#6BAED6",
                                        # leaflet map args
                                        mapZoom = 12) {
  message("Now running the propeR isochroneMultiIntersectTime tool.\n")
  
  pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL) # Creating colour palette
  palIsochrone = leaflet::colorFactor(palColorMarker, NULL, n = length(originPoints)) # Creating colour palette
  
  # Tidying variables ----------
  date_time_legend <-
    format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
  time_series <-
    seq(as.POSIXct(startDateAndTime),
        as.POSIXct(endDateAndTime),
        by = timeIncrease * 60) # Creates a time series between start and end dates and times based on the increment in time
  
  unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  dir.create(paste0(output.dir, "/tmp_folder")) # Creates tmp_folder
  
  message(
    "Creating ",
    nrow(originPoints) * length(time_series),
    " isochrones, please wait...\n"
  )
  
  isoNum <- 0
  
  for (num in 1:length(time_series)) {
    # Start loop to calculate journey details
    for (i in 1:nrow(originPoints)) {
      # Start loop to calculate journey details
      
      isoNum <- isoNum + 1
      
      date_time_legend <-
        format(time_series[num], "%B %d %Y %H:%M") # Creates a legend value for date in day, month, year and time in 24 clock format
      time <- format(time_series[num], "%I:%M %p")
      date <- format(time_series[num], "%m/%d/%Y")
      from_origin <- originPoints[i,]
      
      # Calls otpIsochrone from otp script to get the isochrone from origin for
      # parameters above
      
      isochrone <- propeR::otpIsochrone(
        otpcon,
        batch = TRUE,
        # If true, goal direction is turned off and a full path tree is built (specify only once)
        from = from_origin$lat_lon,
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
        cutoff = isochroneCutOffs # A time cutoff as described above
      )
      
      if (i == 1) {
        # Combines outputs
        isochrone_multi <- isochrone
      } else {
        isochrone_multi$status <-
          c(isochrone_multi$status, isochrone$status)
        isochrone_multi$response <-
          c(isochrone_multi$response, isochrone$response)
      }
      
      message(
        isoNum,
        " out of ",
        nrow(originPoints) * length(time_series),
        " isochrones complete."
      )
      
    }
    
    for (n in 1:length(isochrone_multi$status)) {
      if (n == 1) {
        isochrone_polygons <-
          rgdal::readOGR(isochrone_multi$response[1], "OGRGeoJSON", verbose = FALSE) # Reads first response and greates SpatialPolygonsDataFrame
        poly_df <-
          as.data.frame(isochrone_polygons) # Converts data element of SpatialPolygonsDataFrame to a dataframe
        isochrone_polygons <-
          rgeos::gSimplify(isochrone_polygons, tol = 0.001) # Cleans polygons by simplyfing them
        s_poly <-
          sp::SpatialPolygonsDataFrame(isochrone_polygons, poly_df) # Merges back to SpatialPolygonsDataFrame
        s_poly_intersect <- s_poly
        s_poly_all <- s_poly
      } else {
        # Cleans and appends all other SpatialPolygonsDataFrames together
        isochrone_polygons_tmp <-
          rgdal::readOGR(isochrone_multi$response[n], "OGRGeoJSON", verbose = FALSE)
        poly_df_tmp <-
          as.data.frame(isochrone_polygons_tmp) # Converts data element of SpatialPolygonsDataFrame to a dataframe
        isochrone_polygons_tmp <-
          rgeos::gSimplify(isochrone_polygons_tmp, tol = 0.001) # Cleans polygons by simplyfing them
        s_poly_tmp <-
          sp::SpatialPolygonsDataFrame(isochrone_polygons_tmp, poly_df_tmp) # Merges back to SpatialPolygonsDataFrame
        s_poly_all <- rbind(s_poly_all, s_poly_tmp)
        s_poly_intersect <-
          rgeos::gIntersection(s_poly_intersect, s_poly_tmp)
        if (is(s_poly_intersect, "SpatialCollections")) {
          s_poly_intersect <- s_poly_intersect@polyobj
        }
      }
    }
    
    # Creating a leaflet map from results
    library(leaflet)
    m <- leaflet()
    m <- addScaleBar(m)
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    m <-
      setView(
        m,
        lat = mean(originPoints$lat),
        # Focuses on the origin
        lng = mean(originPoints$lon),
        # Focuses on the origin
        zoom = mapZoom
      )
    m <-
      addPolygons(
        m,
        data = s_poly_intersect,
        # Adds polygons from journey
        stroke = TRUE,
        weight = 5,
        color = palColorPolygon,
        opacity = 1,
        smoothFactor = 0.3,
        fillOpacity = 0.6,
        fillColor = palColorPolygon
      )
    m <-
      addCircleMarkers(
        m,
        data = originPoints,
        # Adds circles for each stage of the journey
        lat = ~ lat,
        lng = ~ lon,
        radius = 8,
        fillColor = palIsochrone(originPoints$name),
        stroke = TRUE,
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~ mode
      )
    m <-
      addLegend(
        m,
        pal = palIsochrone,
        # Adds a legend for the trip
        values = paste(originPoints$name),
        opacity = 0.8
      )
    m <- addLegend(
      m,
      pal = pal_time_date,
      values = date_time_legend,
      position = "bottomleft",
      title = "Date and Time"
    )
    
    mapview::mapshot(m, file = paste0(output.dir, "/tmp_folder/", i, "_", num, ".png")) # Saves map in temp folder
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
    magick::image_write(paste0(output.dir, "/isochrone_time_intersect-", stamp, ".gif")) # write to current dir
  
  m <-
    magick::image_read(paste0(output.dir, "/isochrone_time_intersect-", stamp, ".gif")) %>%
    magick::image_scale("600")
  
  invisible(print(m)) # plots map to Viewer
  
  unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes maps_tmp folder of pngs
}
