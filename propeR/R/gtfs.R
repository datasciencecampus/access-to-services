##' Cleans GTFS feed data
##'
##' Generates a new, cleaned GTFS zip file.
##'
##' @param gtfs.dir The directory for the GTFS files
##' @param gtfs.filename The name of the GTFS files
##' @param train defaults to TRUE
##' @return Returns a cleaned GTFS zip file
##' @author Michael Hodge
##' @examples
##' cleanGTFS(gtfs.dir, gtfs.filename)
##'
##' @export
cleanGTFS <- function(gtfs.dir,
                      gtfs.filename) {
  # read_csv <- function(src) read.csv(paste0(tmp.dir, "/", src, sep=",", as.is=T))
  # write_csv <- function(d, dst) write.csv(d, file=paste0(tmp.dir, "/", dst), row.names=F)
  
  gtfs.file <- paste0(gtfs.dir, "/", gtfs.filename)
  tmp.dir <- paste0(gtfs.dir, "/tmp_folder")
  dir.create(tmp.dir)
  prefix <- paste0(tmp.dir, "/")
  
  unzip(zipfile = gtfs.file, exdir = tmp.dir)
  calendar <-
    read.csv(paste0(prefix, "calendar.txt"),
             sep = ",",
             as.is = TRUE)
  stops <-
    read.csv(paste0(prefix, "stops.txt"),
             sep = ",",
             as.is = TRUE)
  stop_times <-
    read.csv(paste0(prefix, "stop_times.txt"),
             sep = ",",
             as.is = TRUE)
  trips <-
    read.csv(paste0(prefix, "trips.txt"),
             sep = ",",
             as.is = TRUE)
  routes <-
    read.csv(paste0(prefix, "routes.txt"),
             sep = ",",
             as.is = TRUE)
  
  
  message("the length of stop_times.txt before cleaning was: ",
          nrow(stop_times))
  
  # Cleans routes.txt file
  routes$agency_id[routes$agency_id == ""] <-
    "ZZ" # changes all blank agency_id to ZZ
  
  # Cleans stops.txt file
  if ("stop_desc" %in% colnames(stops)) {
    stops <- stops[!is.na(stops$stop_desc),] # removes blank stops
  }
  # Cleans stop_times.txt file
  stop_times <-
    stop_times[stop_times$stop_id %in% stops$stop_id,] # removes stop in stop_time.txt if stop isn't in stop.txt
  stop_times$unique_id <-
    paste(stop_times$trip_id, stop_times$stop_id) # creates a unique id column
  #stop_times <- stop_times[!duplicated(stop_times[c(11)]),] # removes stop if duplicated
  stop_times <-
    stop_times[!duplicated(stop_times[,-ncol(stop_times)]),] # removes stop if duplicated (last col)
  stop_times$unique_id <- NULL # removes unique id column
  stop_times <-
    stop_times[!stop_times$arrival_time == "",] # removes stop if arrival time is empty
  stop_times <-
    stop_times[!stop_times$departure_time == "",] # removes stop if departure time is empty
  # todo: look at this.
  stop_times$stop_sequence <-
    ave(
      stop_times$trip_id,
      stop_times$trip_id,
      FUN = function(y)
        1:100
    ) # creates new sequence for stops
  
  # Cleans calendar.txt file
  calendar <-
    calendar[!duplicated(calendar),] # removes calendar if duplicated
  
  # Cleans trips.txt file
  trips <- trips[!duplicated(trips),] # removes trip if duplicated
  
  message("the length of stop_times.txt after cleaning was: ",
          nrow(stop_times))
  
  # todo: no need to read/write all of these
  # Writes files to txt
  
  write.csv(calendar, file = (paste0(prefix, "calendar.txt")), row.names =
              FALSE)
  write.csv(stops, file = (paste0(prefix, "stops.txt")), row.names = FALSE)
  write.csv(trips, file = (paste0(prefix, "trips.txt")), row.names = FALSE)
  write.csv(stop_times, file = (paste0(prefix, "stop_times.txt")), row.names =
              FALSE)
  
  # Zips files back to filename
  files <- list.files(tmp.dir, recursive = TRUE)
  setwd(tmp.dir)
  zip(zipfile = paste0(prefix, gsub(".zip", "", gtfs.filename), "_new", ".zip"),
      files = file.path(files))
  file.rename(paste0(prefix, gsub(".zip", "", gtfs.filename), "_new", ".zip"),
              paste0(gsub(".zip", "", gtfs.file), "_new", ".zip"))
  unlink(tmp.dir, recursive = TRUE) # Deletes tmp_folder
  
}
