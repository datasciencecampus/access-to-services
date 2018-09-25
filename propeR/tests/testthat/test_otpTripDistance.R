testthat::test_that("Test otpTripDistance",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  origin_points <- propeR::importLocationData(paste0(getwd(),"/data/origin.csv"))
  destination_points <- propeR::importLocationData(paste0(getwd(),"/data/destination.csv"))
  
  distance <- propeR::otpTripDistance(otpcon,
                                      from = origin_points$lat_lon,
                                      to = destination_points$lat_lon,
                                      modes = 'TRANSIT,WALK')
  testthat::expect_equal(floor(distance$duration[1]), 1518)
})