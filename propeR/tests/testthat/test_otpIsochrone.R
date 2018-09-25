testthat::test_that("Test otpIsochrone",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  origin_points <- propeR::importLocationData(paste0(getwd(),"/data/origin.csv"))
  destination_points <- propeR::importLocationData(paste0(getwd(),"/data/destination.csv"))
  
  isochrone <- propeR::otpIsochrone(otpcon,
                                    from = origin_points$lat_lon,
                                    modes = 'TRANSIT,WALK',
                                    date =  '2018-10-01',
                                    time = '08:00am')
  
  isochrone_polygons <- rgdal::readOGR(isochrone$response, "OGRGeoJSON", verbose = FALSE)
  testthat::expect_equal(length(isochrone_polygons@polygons[[1]]@Polygons), 49)
})