testthat::test_that("Test otpChoropleth",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  origin_points <- propeR::importLocationData(paste0(getwd(),"/data/origin.csv"))
  destination_points <- propeR::importLocationData(paste0(getwd(),"/data/destination.csv"))
  origin_polygons <- propeR::importGeojsonData(paste0(getwd(),"/data/origin.geojson"))
  
  choropleth <- propeR::otpChoropleth(
    otpcon,
    detail = TRUE,
    from = origin_points$lat_lon,
    to = destination_points$lat_lon,
    modes = 'WALK,TRANSIT', 
    date = '2018-10-01',
    time = '08:00am'
  )
  
  times <- floor(choropleth$itineraries$duration)
  testthat::expect_equal(times, 34)
})