##' Generates an isochrone map from a single origin
##'
##' Generates an isochrone polygon from a single origin and checks whether destinations
##' fall within isochrone, and if so, at what cutoff time amount. 
##' A comma separated value file of journey times for each destination is saved in the output folder.
##' A map of the journey can also be saved as a .png image and .html file.
##' The polygon can also be saved as a .GeoJSON file.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL, see ?otpcon for details
##' @param originPoints The variable containing origin(s), see ?importLocationData for details
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s) see ?importLocationData for details
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
##' @return Saves journey details as comma separated value file to output directory. A map in .png and .html formats, and/or a polygon as a .GeoJSON format, may also be saved
##' @author Michael Hodge
##' @examples
##'   isochrone(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
isochrone <- function(output.dir,
                      otpcon,
                      originPoints,
                      originPointsRow = 1,
                      destinationPoints,
                      destinationPointsRow = 1,
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
  
  message("Now running the propeR isochrone tool.\n")
  
  if (mapOutput == T) {
    library(leaflet)
    pal_time_date = leaflet::colorFactor(c("#FFFFFF"), domain = NULL) 
    palIsochrone = leaflet::colorFactor(mapPolygonColours, NULL, n = length(isochroneCutOffs))
  }
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  origin_points_row_num <- originPointsRow 
  from_origin <- originPoints[origin_points_row_num,]
  if (origin_points_row_num > nrow(originPoints)) {
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
    stop('Row is not in origin file, process aborted.\n')
  }
  
  destination_points_row_num <- destinationPointsRow 
  
  start_time <- format(as.POSIXct(startDateAndTime), "%I:%M %p") 
  start_date <- as.Date(startDateAndTime) 
  date_time_legend <- format(as.POSIXct(startDateAndTime), "%d %B %Y %H:%M") 
  
  ###########################
  #### CALL OTP FUNCTION ####
  ###########################
  
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
  
  isochrone_polygons <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = F) 
  destination_points_spdf <- destinationPoints 
  sp::coordinates(destination_points_spdf) <- ~ lon + lat 
  sp::proj4string(destination_points_spdf) <- sp::proj4string(isochrone_polygons) 
    isochrone_polygons_split <- sp::split(isochrone_polygons, isochrone_polygons@data$time) 
  
  time_df <- data.frame(matrix(,
      ncol = length(isochroneCutOffs),
      nrow = nrow(destination_points_spdf))) 
  
  for (i in 1:length(isochroneCutOffs)) {
    time_df_tmp <- sp::over(destination_points_spdf, isochrone_polygons_split[[i]])
    time_df[, i] <- time_df_tmp[, 2]
  }
  
  for (i in 1:nrow(destination_points_spdf)) {
    
    if (is.na(time_df[i, length(isochroneCutOffs)])) {
      time_df[i, length(isochroneCutOffs) + 1] = NA
    } else {
      time_df[i, length(isochroneCutOffs) + 1] = min(time_df[i, 1:length(isochroneCutOffs)], na.rm = T)
    }
  }
  
  names(time_df)[ncol(time_df)] <- "travel_time"
  destinationPoints$travel_time <- time_df$travel_time / 60
  
  destination_points_non_na <- subset(destinationPoints,!(is.na(destinationPoints["travel_time"])))
  
  #########################
  #### OPTIONAL EXTRAS ####
  #########################
  
  if (mapOutput == T) {
    message("Generating map, please wait.")
    library(leaflet)
    m <- leaflet()
    m <- addScaleBar(m)
    m <- addProviderTiles(m, providers$OpenStreetMap.BlackAndWhite)
    
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
        min(min(from_origin$lon),min(destinationPoints$lon),isochrone_polygons@bbox[1]),
        min(min(from_origin$lat),min(destinationPoints$lat),isochrone_polygons@bbox[2]),
        max(max(from_origin$lon),max(destinationPoints$lon),isochrone_polygons@bbox[3]),
        max(max(from_origin$lat),max(destinationPoints$lat),isochrone_polygons@bbox[4]))
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
      popup = ~ name,
      fillColor = "black",
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
      popup = ~ name,
      fillColor = "white",
      stroke = T,
      color = "black",
      opacity = mapMarkerOpacity,
      weight = 2,
      fillOpacity = mapMarkerOpacity,
      radius = 10
    )
    m <- addLegend(
      m,
      pal = palIsochrone,
      values = isochroneCutOffs,
      opacity = mapLegendOpacity,
      title = "Duration (minutes)"
    )
    m <- addLegend(
      m,
      pal = pal_time_date,
      opacity = mapLegendOpacity,
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
          library = "fa"))
  }
  
  ######################
  #### SAVE RESULTS ####
  ######################
  
  message("Analysis complete, now saving outputs to ", output.dir, ", please wait.\n")
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  
  write.csv(
    destinationPoints,
    file = paste0(output.dir, "/isochrone-", stamp, ".csv"),
    row.names = F)
  
  if (geojsonOutput == T) {
    rgdal::writeOGR(
      isochrone_polygons,
      dsn = paste0(output.dir,
                   "/isochrone",
                   stamp,
                   ".geoJSON"),
      layer = "isochrone_polygons",
      driver = "GeoJSON")
  }
  
  if (mapOutput == T) {
    invisible(print(m))
    mapview::mapshot(m, file = paste0(output.dir, "/isochrone-", stamp, ".png")) 
    htmlwidgets::saveWidget(m, file = paste0(output.dir, "/isochrone-", stamp, ".html")) 
    unlink(paste0(output.dir, "/isochrone-", stamp, "_files"),
           recursive = T) 
    unlink(paste0(output.dir, "/tmp_folder"), recursive = T) 
  }
  
  message("Thanks for using propeR.")
}
