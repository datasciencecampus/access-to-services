##' Calculates the journey for a number of origins and/or destinations.
##'
##' Calculates the journey time and details between multiple origins and/or destinations. 
##' A comma separated value file of journey details is saved in the output folder.
##'
##' @param output.dir The directory for the output files
##' @param otpcon The OTP router URL, see ?otpcon for details
##' @param originPoints The variable containing origin(s), see ?importLocationData for details
##' @param destinationPoints The variable containing destination(s) see ?importLocationData for details
##' @param journeyReturn Specifies whether the journey should be calculated as a return or not (default is TRUE)
##' @param startDateAndTime The start time and date, in 'YYYY-MM-DD HH:MM:SS' format
##' @param modes The mode of the journey, defaults to 'TRANSIT, WALK'
##' @param maxWalkDistance The maximum walking distance, in meters, defaults to 1000 m
##' @param walkReluctance The reluctance of walking-based routes, defaults to 2 (range 0 (lowest) - 20 (highest))
##' @param walkSpeed The walking soeed, in meters per second, defaults to 1.4 m/s
##' @param bikeSpeed The cycling speed, in meters per second, defaults to 4.3 m/s
##' @param minTransferTime The maximum transfer time, in minutes, defaults to 0 mins (no time specified)
##' @param maxTransfers The maximum number of transfers, defaults to 10
##' @param wheelchair If TRUE, uses on wheeelchair friendly stops, defaults to FALSE
##' @param arriveBy Selects whether journey starts at startDateandTime (FALSE) or finishes (TRUE), defaults to FALSE
##' @param preWaitTime The maximum waiting time before a journey cannot be found, in minutes, defaults to 15 mins
##' @param nearestNum Specify the number of nearest pairs to return (default is 1)
##' @return Saves journey details as comma separated value file to output directory
##' @author Michael Hodge
##' @examples
##'   pointToPointNearest(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     journeyReturn = TRUE,
##'     startDateAndTime = "2018-08-18 12:00:00",
##'     nearestNum = 1,
##'   )
##' @export
pointToPointNearest <- function(output.dir,
                             otpcon,
                             originPoints,
                             destinationPoints,
                             journeyReturn = F,
                             # otpTime args
                             startDateAndTime = "2018-08-13 09:00:00",
                             modes = "WALK, TRANSIT",
                             maxWalkDistance = 1000,
                             walkReluctance = 2,
                             walkSpeed = 1.5,
                             bikeSpeed = 5,
                             minTransferTime = 1,
                             maxTransfers = 5,
                             wheelchair = F,
                             arriveBy = F,
                             preWaitTime = 15,
                             nearestNum = 1) {
  
  message("Now running the propeR pointToPointNearest tool.\n")
  
  stamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  if (journeyReturn == T) { multiplier <- 2 } else { multiplier <- 1 }
  
  message("Finding nearest pairs of origin and destination points using KNN...")
  originPointsSpatial <- originPoints
  destinationPointsSpatial <- destinationPoints
  
  coordinates(originPointsSpatial) <- c("lon","lat")
  coordinates(destinationPointsSpatial) <- c("lon","lat")
  g = FNN::get.knnx(coordinates(destinationPointsSpatial), coordinates(originPointsSpatial),k=nearestNum)
  pair = g$nn.index
  
  num.run <- 0
  
  num.total <- nrow(originPoints) * multiplier
  start_time <- format(as.POSIXct(startDateAndTime), "%I:%M %p") 
  start_date <- as.Date(startDateAndTime)
  time.taken <- vector()
  calls.list <- c(0)
  message("Creating ", num.total, " point to point connections, please wait...")
  
  for (j in 1:multiplier) {
    
      for (i in 1:nrow(originPoints)) {
        
        num.run <- num.run + 1
        
        start.time <- Sys.time()
        
        from_origin <- originPoints[i,]
        destination_pair <- pair[i,nearestNum]
        to_destination <- destinationPoints[destination_pair,]
        
        if (j == 1) {
          from <- from_origin
          to <- to_destination
        } else {
          to <- from_origin
          from <- to_destination
        }
        
        if (to$name == from$name) {
          num.run <- num.run - 1
          num.total <- num.total - 1
          message("Dropped a connection call as origin and destination were the same!")
          next
        }
        
        point_to_point <- propeR::otpTripTime(
          otpcon,
          detail = T,
          from = from$lat_lon,
          to = to$lat_lon,
          modes = modes,
          date = start_date,
          time = start_time,
          maxWalkDistance = maxWalkDistance,
          walkReluctance = walkReluctance,
          walkSpeed = walkSpeed,
          bikeSpeed = bikeSpeed,
          minTransferTime = minTransferTime,
          maxTransfers = maxTransfers,
          wheelchair = wheelchair,
          arriveBy = arriveBy,
          preWaitTime = preWaitTime
        )
        
        if (num.run == 1) {
          
          if (!is.null(point_to_point$errorId)){
            
            if (point_to_point$errorId == "OK") {
              
              point_to_point_table_overview <- point_to_point$itineraries
              point_to_point_table_overview["origin"] <- from$name
              point_to_point_table_overview["destination"] <- to$name
              point_to_point_table_overview["distance_km"] <- round(sum(point_to_point$output_table$distance) / 1000, digits = 2)
              point_to_point_table_overview["journey_details"] <- jsonlite::toJSON(point_to_point$output_table)
            } else {
              point_to_point_table_overview <- data.frame(
                "start" = NA,
                "end" = NA,
                "duration" = NA,
                "walkTime" = NA,
                "transitTime" = NA,
                "waitingTime" = NA,
                "transfers" = NA,
                "origin" = from$name,
                "destination" = to$name,
                "distance_km" = NA,
                "journey_details" = NA)
            }
            
          } else {
            point_to_point_table_overview <- data.frame(
              "start" = NA,
              "end" = NA,
              "duration" = NA,
              "walkTime" = NA,
              "transitTime" = NA,
              "waitingTime" = NA,
              "transfers" = NA,
              "origin" = from$name,
              "destination" = to$name,
              "distance_km" = NA,
              "journey_details" = NA)
          }
          
        } else {
          
          if (!is.null(point_to_point$errorId)){
            
            if (point_to_point$errorId == "OK") {
              point_to_point_table_overview_tmp <- point_to_point$itineraries
              point_to_point_table_overview_tmp["origin"] <- from$name
              point_to_point_table_overview_tmp["destination"] <- to$name
              point_to_point_table_overview_tmp["distance_km"] <- round(sum(point_to_point$output_table$distance) / 1000, digits = 2)
              point_to_point_table_overview_tmp["journey_details"] <- jsonlite::toJSON(point_to_point$output_table)
            } else {
              point_to_point_table_overview_tmp <- data.frame(
                "start" = NA,
                "end" = NA,
                "duration" = NA,
                "walkTime" = NA,
                "transitTime" = NA,
                "waitingTime" = NA,
                "transfers" = NA,
                "origin" = from$name,
                "destination" = to$name,
                "distance_km" = NA,
                "journey_details" = NA)
            }
            
            if (modes == "CAR") {
              colnames(point_to_point_table_overview)[which(names(point_to_point_table_overview) == "walkTime")] <- "driveTime"
              colnames(point_to_point_table_overview_tmp)[which(names(point_to_point_table_overview_tmp) == "walkTime")] <- "driveTime"
            } else if (modes == "BICYCLE") {
              colnames(point_to_point_table_overview)[which(names(point_to_point_table_overview) == "walkTime")] <- "cycleTime"
              colnames(point_to_point_table_overview_tmp)[which(names(point_to_point_table_overview_tmp) == "walkTime")] <- "cycleTime"
            }
            
            point_to_point_table_overview = rbind(point_to_point_table_overview, point_to_point_table_overview_tmp)
            
          } else {
            point_to_point_table_overview_tmp <- data.frame(
              "start" = NA,
              "end" = NA,
              "duration" = NA,
              "walkTime" = NA,
              "transitTime" = NA,
              "waitingTime" = NA,
              "transfers" = NA,
              "origin" = from$name,
              "destination" = to$name,
              "distance_km" = NA,
              "journey_details" = NA)
            
            if (modes == "CAR") {
              colnames(point_to_point_table_overview)[which(names(point_to_point_table_overview) == "walkTime")] <- "driveTime"
              colnames(point_to_point_table_overview_tmp)[which(names(point_to_point_table_overview_tmp) == "walkTime")] <- "driveTime"
            } else if (modes == "BICYCLE") {
              colnames(point_to_point_table_overview)[which(names(point_to_point_table_overview) == "walkTime")] <- "cycleTime"
              colnames(point_to_point_table_overview_tmp)[which(names(point_to_point_table_overview_tmp) == "walkTime")] <- "cycleTime"
            }
            
            point_to_point_table_overview <- rbind(point_to_point_table_overview, point_to_point_table_overview_tmp)
          }
        }
        
        end.time <- Sys.time()
        time.taken[num.run] <- round(end.time - start.time, digits = 2)
        
        if (num.run < num.total) {
          message(
            num.run,
            " out of ",
            num.total,
            " connections complete. Time taken ",
            round(sum(time.taken), digits = 2),
            " seconds. Estimated time left is approx. ",
            round((mean(
              time.taken
            ) * num.total) - sum(time.taken),
            digits = 2),
            " seconds."
          )
        } else {
          message(
            num.run,
            " out of ",
            num.total,
            " connections complete. Time taken ",
            sum(time.taken),
            " seconds.\n"
          )
        }
        
        if ((num.run/100) %% 1 == 0) { # fail safe for large files
          
          message("Large dataset, failsafe, saving outputs to ", output.dir, ", please wait.")
          
          point_to_point_table_overview_out <- point_to_point_table_overview[, c(8, 9, 1, 2, 10, 3, 4, 5, 6, 7, 11)]
          colnames(point_to_point_table_overview_out) <-
            c(
              "origin",
              "destination",
              "start_time",
              "end_time",
              "distance_km",
              "duration_mins",
              "walk_time_mins",
              "transit_time_mins",
              "waiting_time_mins",
              "transfers",
              "journey_details")
          
          if (modes == "CAR") {
            colnames(point_to_point_table_overview_out)[which(names(point_to_point_table_overview_out) == "walk_time_mins")] <- "drive_time_mins"
          } else if (modes == "BICYCLE") {
            colnames(point_to_point_table_overview_out)[which(names(point_to_point_table_overview_out) == "walk_time_mins")] <- "cycle_time_mins"
          }
          
          write.csv(
            point_to_point_table_overview_out,
            file = paste0(output.dir, "/pointToPointNearest-", stamp, ".csv"),
            row.names = F)
          
        }
        
      }
    }
  
  message("Analysis complete, now saving outputs to ", output.dir, ", please wait.\n")
  
  point_to_point_table_overview_out <- point_to_point_table_overview[, c(8, 9, 1, 2, 10, 3, 4, 5, 6, 7, 11)]
  colnames(point_to_point_table_overview_out) <-
    c(
      "origin",
      "destination",
      "start_time",
      "end_time",
      "distance_km",
      "duration_mins",
      "walk_time_mins",
      "transit_time_mins",
      "waiting_time_mins",
      "transfers",
      "journey_details")
  
  if (modes == "CAR") {
    colnames(point_to_point_table_overview_out)[which(names(point_to_point_table_overview_out) == "walk_time_mins")] <- "drive_time_mins"
  } else if (modes == "BICYCLE") {
    colnames(point_to_point_table_overview_out)[which(names(point_to_point_table_overview_out) == "walk_time_mins")] <- "cycle_time_mins"
  }
  
  write.csv(
    point_to_point_table_overview_out,
    file = paste0(output.dir, "/pointToPointNearest-", stamp, ".csv"),
    row.names = F)
  
  message("Thanks for using propeR.")
}
