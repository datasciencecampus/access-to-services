##' Generates a GeoJSON of intersecting polygons for multiple origins
##'
##' Calculates an polygon for the intersection between multiple origins and finds the intersection.
##' Saves polygon as a GeoJSON file.
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
##' @param isochroneCutOffs in minutes, defaults to 60
##' @param map specify whether you want output a leaflet map, defaults to FALSE
##' @param geojson specific whether you want to output a GeoJSON file, defaults to TRUE
##' @param palColorMarkers the color palette of the markers, defaults to 'Greys'
##' @param palColorPolygon the color palette of the poygon, defaults to 'Blue'
##' @param mapZoom defaults to 12
##' @return Saves map as a png and geojson of the intersection area to output directory
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
isochroneMultiIntersect <- function(output.dir,
                                    otpcon,
                                    originPoints,
                                    destinationPoints,
                                    # otp args
                                    # todo: since most functions rely on these could maybe use a otp args list or look into using S3.
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
                                    # function specific args
                                    isochroneCutOffs = 60,
                                    # leaflet map args
                                    map = FALSE,
                                    geojson = TRUE,
                                    palColorMarker = "Greys",
                                    palColorPolygon = "#6BAED6",
                                    mapZoom = 12) {
  message("Now running the propeR isochroneMultiIntersect tool.\n")
  
  if (map == TRUE) {
    library(leaflet)
    palIsochrone = leaflet::colorFactor(palColorMarker, NULL, n = length(originPoints)) # Creating colour palette
    unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  if (is.null(originPoints$mode)) {
    originPoints$mode <- modes
  }
  if (is.null(originPoints$max_duration)) {
    originPoints$max_duration <- isochroneCutOffs
  }
  if (is.null(originPoints$time)) {
    originPoints$time <-
      format(as.POSIXct(startDateAndTime), "%I:%M %p")
  }
  if (is.null(originPoints$date)) {
    originPoints$date <- as.Date(startDateAndTime)
  }
  
  message("Creating ", nrow(originPoints), " isochrones, please wait...")
  time.taken <- vector()
  
  for (i in 1:nrow(originPoints)) {
    #Changes transport modes to OTP transport modes
    from_origin <- originPoints[i,]
    if (from_origin$mode == "Public Transport") {
      mode <- "TRANSIT,WALK"
    } else if (from_origin$mode == "Driving") {
      mode <- "CAR"
    } else if (from_origin$mode == "Train") {
      mode <- "RAIL,WALK"
    } else if (from_origin$mode == "Bus") {
      mode <- "BUS,WALK"
    } else if (from_origin$mode == "Walking") {
      mode <- "WALK"
    } else if (from_origin$mode == "Cycling") {
      mode <- "BICYCLE"
    } else {
      mode <- modes
    }
    
    start.time <- Sys.time()
    
    isochrone <- propeR::otpIsochrone(
      otpcon,
      batch = TRUE,
      # If true, goal direction is turned off and a full path tree is built (specify only once)
      from = from_origin$lat_lon,
      # Takes the latitude and longitude from specified origin
      modes = from_origin$mode,
      date = from_origin$date,
      # Takes the date as specified above
      time = from_origin$time,
      # Takes the time as specified above
      maxWalkDistance = maxWalkDistance,
      walkReluctance = walkReluctance,
      walkSpeed = walkSpeed,
      bikeSpeed = bikeSpeed,
      minTransferTime = minTransferTime,
      maxTransfers = maxTransfers,
      wheelchair = wheelchair,
      arriveBy = arriveBy,
      cutoff = from_origin$max_duration # A time cutoff as described above
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
    
    end.time <- Sys.time()
    time.taken[i] <- round(end.time - start.time, digits = 2)
    
    if (i < nrow(originPoints)) {
      message(
        i,
        " out of ",
        nrow(originPoints),
        " isochrones complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * nrow(originPoints)
        ) - sum(time.taken),
        digits = 2),
        " seconds."
      )
    } else {
      message(
        i,
        " out of ",
        nrow(originPoints),
        " isochrones complete. Time taken ",
        sum(time.taken),
        " seconds."
      )
    }
    
  }
  
  message("Finding the intersect between ",
          nrow(originPoints),
          " isochrones, please wait...")
  
  time.taken <- vector()
  
  for (n in 1:length(isochrone_multi$status)) {
    start.time <- Sys.time()
    
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
    
    if (n < length(isochrone_multi$status)) {
      message(
        n,
        " out of ",
        length(isochrone_multi$status),
        " intersections complete. Time taken ",
        round(sum(time.taken), digit = 2),
        " seconds. Estimated time left is approx. ",
        round((
          mean(time.taken) * nrow(originPoints)
        ) - sum(time.taken),
        digits = 2),
        " seconds."
      )
    } else {
      message(
        n,
        " out of ",
        length(isochrone_multi$status),
        " intersections complete. Time taken ",
        sum(time.taken),
        " seconds."
      )
    }
    
  }
  
  #########################
  #### OPTIONAL EXTRAS ####
  #########################
  
  
  if (map == TRUE) {
    popup_originPoints <-
      # generates a popup for the poly_lines_lines feature
      paste0(
        "<strong>Name: </strong>",
        originPoints$name,
        "<br><strong>Mode: </strong>",
        originPoints$mode,
        "<br><strong>Duration: </strong>",
        round(originPoints$max_duration, digits = 2),
        " mins",
        "<br><strong>Date: </strong>",
        originPoints$date,
        "<br><strong>Time: </strong>",
        originPoints$time
      )
    
    # todo: all these leaflet options can be provided in function args.
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
        popup = popup_originPoints
      )
    m <- addLegend(
      m,
      pal = palIsochrone,
      # Adds a legend for the trip
      values = paste(
        originPoints$name,
        " by ",
        originPoints$mode,
        " in ",
        originPoints$max_duration,
        " mins",
        sep = ""
      ),
      opacity = 0.8
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
  
  s_poly_intersect <-
    as(s_poly_intersect, "SpatialPolygonsDataFrame")
  
  if (geojson == TRUE) {
    rgdal::writeOGR(
      s_poly_intersect,
      dsn = paste0(
        output.dir,
        "/isochrone_multi_intersect-",
        stamp,
        ".geoJSON"
      ),
      layer = "s_poly_intersect",
      driver = "GeoJSON"
    )
  }
  
  if (map == TRUE) {
    invisible(print(m)) # plots map to Viewer
    mapview::mapshot(m,
                     file = paste0(output.dir, "/isochrone_multi_intersect-", stamp, ".png"))
    htmlwidgets::saveWidget(m,
                            file = paste0(output.dir, "/isochrone_multi_merge-", stamp, ".html")) # Saves as an interactive HTML webpage
    unlink(paste0(output.dir, "/isochrone_multi_merge-", stamp, "_files"),
           recursive = TRUE) # Deletes tmp_folder
  }

}
