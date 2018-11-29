##' Imports Location Data
##'
##' Imports location data (origin or destination) from a comma separated file
##'
##' @param src .CSV file source
##' @return R dataframe of origin or destination points
##' @author Michael Hodge
##' @examples originPoints <- importLocationData('C:\Users\User\Documents\origins.csv')
##' destinationPoints <- importLocationData('C:\Users\User\Documents\destinations.csv')
##' @export
importLocationData <- function(src) {
  data_points <- read.csv(src, sep = ",", as.is = TRUE) # Opens file box
  names(data_points) <-
    tolower(names(data_points)) # Convert column names to lower case
  if ("lat" %in% colnames(data_points))
    # Checks data to see if lat and lon columns are present
  {
    if (!("lon" %in% colnames(data_points))) {
      stop("No longitudinal 'lon' column present.")
      
    }
  } else {
    message("No latitude 'lat' column present.\n")
    
    message(
      "Will try and find a postcode column in the data. If one is found, it may be able to be converted to latitude and longitude.\n"
    )
    # Checks to see if postcode has been given, if so we can convert to lat, lon
    if ("postcode" %in% colnames(data_points)) {
      message(
        "Postcodes were found, will convert to latitude and longitude now. Warning: If a postcode doesn't exist
        as a latitude and longitude in the predefined database, the location will be removed from the list.\n"
      )
      
      for (i in 1:nrow(data_points)) {
        pc_content <-
          propeR::postcodeToDecimalDegrees(data_points$postcode[i])
        if (pc_content$status == '404') {
          warning(
            "Warning: Postcode",
            data_points$name[i],
            "cannot be convert to a latitude and longitude.
            This location shall be removed from the list.\n"
          )
          
          data_points <- data_points[-i, ]
        } else {
          # todo: not sure this will work...
          data_points$lat[i] <- pc_content$result$latitude
          data_points$lon[i] <- pc_content$result$longitude
        }
      }
    } else {
      stop(
        "A postcode column cannot be found, please review the data and either provide a latitude 'lat' and longitude
        'lon' column (best), or postcode, for each location.\n"
      )
      
    }
  }
  data_points <-
    data_points[order(data_points$name), ] # Sort by name
  data_points <-
    as.data.frame(data_points) # Converts to dataframes and create lat, lon field needed by otp
  data_points$lat_lon <-
    with(data_points, paste0(lat, ",", lon)) # Adds a lat_lon column as needed by otp
  data_points
}

##' Imports Geojson Data
##'
##' Imports polygon data (origin) from a geojson file
##'
##' @param src .Geojson file source
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