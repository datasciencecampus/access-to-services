suppressMessages(library(propeR))

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

if(!interactive()) {
  args.str <- commandArgs(trailingOnly=T)
  args.lst <- eval(parse(text=paste0("list(", args.str, ")")))
  stopifnot("fun" %in% names(args.lst))
  do.call(generate, args.lst)
}
