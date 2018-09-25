testthat::test_that("Test otpConnect",{
  otpcon <- propeR::otpConnect(hostname = "localhost",router = "default",port = "8080",ssl = "false")
  testthat::expect_equal(otpcon, 'http://localhost:8080/otp/routers/default')
})