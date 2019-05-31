##' Imports Location Data
##'
##' Imports location data from a comma separated file containing either latitude and longitude values, or postcode values
##'
##' @param src The source of the .CSV file
##' @param idcol The title of the header of the column containing unique identifier names, default is "name"
##' @param loncol The title of the header of the column containing the longitudinal values, default is "lon"
##' @param latcol The title of the header of the column containing the latitudinal values, default is "lat"
##' @param postcodecol  The title of the header of the column containing the postcode values, default is "postcode"
##' @return R dataframe of location points
##' @author Michael Hodge
##' @examples originPoints <- importLocationData('C:\Users\User\Documents\origins.csv', idcol = "name", loncol = "lon", latcol = "lat")
##' @export
importLocationData <- function(src,
                               idcol = "name",
                               loncol = "lon",
                               latcol = "lat",
                               postcodecol = "postcode") {
  
  data_points <-
    read.csv(src, sep = ",", as.is = TRUE) # Opens file box
  
  colnames(data_points)[which(names(data_points) == idcol)] <- "name"
  colnames(data_points)[which(names(data_points) == loncol)] <- "lon"
  colnames(data_points)[which(names(data_points) == latcol)] <- "lat"
  colnames(data_points)[which(names(data_points) == postcodecol)] <- "postcode"
  
  if ("lat" %in% colnames(data_points))
  {
    if (!("lon" %in% colnames(data_points))) {
      stop(
        paste0(
          "No longitudinal column present with the name ",
          loncol,
          "\n"
          )
        )
    }
  } else {
    message(
      paste0(
        "No latitude column present with the name ",
        latcol,
        "\n"
        )
      )
    
    message(
      paste0(
        "Will try and find a postcode column in the data with the name ",
        postcodecol,
        ". If one is found, it may be able to be converted to latitude and longitude values.\n"
        )
      )
    
    if (postcodecol %in% colnames(data_points)) {
      message(
        "Postcodes were found, and will be converted to latitude and longitude values now. 
        Warning: If a postcode doesn't exist as a latitude and longitude in the API call,
        the location will be removed from the list.\n"
      )
      
      data_points$postcode <- gsub('\\s+', '', data_points$postcode)
      
      for (i in 1:nrow(data_points)) {
        pc_content <-
          propeR::postcodeToDecimalDegrees(data_points$postcode[i])
        if (pc_content$status == 'no_match') {
          pc_content <-
            propeR::postcodeToDecimalDegrees_backup(data_points$postcode[i])
          if (pc_content$status == '404') {
            warning(
              "Warning: Postcode ",
              data_points$postcode[i],
              " for location ",
              data_points$name[i],
              " cannot be convert to a latitude and longitude.
              This location shall be removed from the list.\n"
            )
            data_points <- data_points[-i,]
          } else {
            data_points$lat[i] <- pc_content$result$latitude
            data_points$lon[i] <- pc_content$result$longitude
          }
        } else {
          data_points$lat[i] <- as.double(pc_content$data$latitude)
          data_points$lon[i] <- as.double(pc_content$data$longitude)
        }
      }
    } else {
      stop(
        paste0(
          "A postcode column with the name ",
          postcodecol,
          " cannot be found, please review the data and either provide a latitude and longitude
          column (best), or postcode, for each location.\n"
        )
      )
    }
  }
  
  # data_points <-
  #   data_points[order(data_points$name),] # Sort by name
  data_points <-
    as.data.frame(data_points) # Converts to dataframes and create lat, lon field needed by otp
  data_points$lat_lon <-
    with(data_points, paste0(lat, ",", lon)) # Adds a lat_lon column as needed by otp
  data_points
}

##' Imports GeoJSON Data
##'
##' Imports polygon data from a GeoJSON file
##'
##' @param src The source for the .GeoJSON file
##' @return Large SpatialPolygonsDataFrame of origin polygons
##' @author Michael Hodge
##' @examples originPolygons <- importGeojsonData('C:\Users\User\Documents\origins.geojson')
##' @export
importGeojsonData <- function(src) {
  ## todo: refactor this. looks the same as ^
  data_polygons <- rgdal::readOGR(src, verbose = FALSE)
  message("GeoJSON data successfully loaded.\n")
  data_polygons
}