library(propeR)

connect <- (function() {
  otp <- NULL
  function() {
    if(is.null(otp)) {
      otp <<- otpConnect()
    }
    otp
  }
})()

generate <- function(fun, src_file, dst_file, ...) {
  otp <- connect()
  src.points <- importLocationData(src_file)
  dst.points <- importLocationData(dst_file)
  do.call(fun, list(otpcon=otp, originPoints=src.points, destinationPoints=dst.points, ...))
}

generate_point_to_point <- function(src_file, dst_file, ...) {
  generate(pointToPoint, src_file, dst_file, ...)
}

generate_isochrone <- function(src_file, dst_file, ...) {
  generate(isochrone, src_file, dst_file, ...)
}

#generate_point_to_point(src_file="propeR/inst/extdata/origin.csv", dst_file="propeR/inst/extdata/destination.csv", output.dir="/tmp/a2s", startDateAndTime="2019-08-02 12:00:00", mapOutput=T)
