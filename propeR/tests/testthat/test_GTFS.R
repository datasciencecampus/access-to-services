testthat::test_that("Test GTFS",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  origin_points <- propeR::importLocationData(paste0(getwd(),"/data/origin.csv"))
  destination_points <- propeR::importLocationData(paste0(getwd(),"/data/destination.csv"))
  
  time <- propeR::otpTripTime(otpcon,
                              detail = TRUE,
                              from = origin_points$lat_lon,
                              to = destination_points$lat_lon,
                              modes = 'WALK,TRANSIT', 
                              date = '2018-10-01',
                              time = '08:00am'
  )
  
  testthat::expect_equal(time$errorId, "OK")
})