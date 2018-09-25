testthat::test_that("Test otpTriptime",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  origin_points <- propeR::importLocationData(paste0(getwd(),"/data/origin.csv"))
  destination_points <- propeR::importLocationData(paste0(getwd(),"/data/destination.csv"))
  
  time <- propeR::otpTripTime(otpcon,
                              from = origin_points$lat_lon,
                              to = destination_points$lat_lon,
                              modes = 'TRANSIT,WALK',
                              date =  '2018-10-01',
                              time = '08:00am',
                              detail = TRUE)
  testthat::expect_equal(floor(time$itineraries$duration), 34)
})