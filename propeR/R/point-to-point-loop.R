##' Calculates an isochrone map from a single origin
##'
##' .. content for \details{} ..
##'
##' @param output.dir The directory for the output files
##' @param otpcon OTP router URL
##' @param originPoints The variable containing origin(s), see ?importLocationData
##' @param originPointsRow The row of originPoints to be used, defaults to 1
##' @param destinationPoints The variable containing destination(s), see ?importLocationData
##' @param destinationPointsRow The row of destinationPoints to be used, defaults to 1
##' @param loopType Specify the type of loop, origin (1), destination (2) or both (0, default)
##' @param return Specify whether the journey should be calculated as a return or not (default is True)
##' @param startDateAndTime in 'YYYY-MM-DD HH:MM:SS' format
##' @param modes defaults to 'TRANSIT, WALK'
##' @param maxWalkDistance in meters, defaults to 1000
##' @param walkReluctance defaults to 2 (range 0 - 20)
##' @param walkSpeed in m/s, defaults to 1.4
##' @param bikeSpeed in m/s, defaults to 4.3
##' @param minTransferTime in minutes, defaults to 1
##' @param maxTransfers defaults to 10
##' @param wheelchair defaults to FALSE
##' @param arriveBy defaults to FALSE
##' @return Returns a csv of journey details to the output directory
##' @author Michael Hodge
##' @examples
##'   pointToPointLoop(
##'     output.dir = 'C:\Users\User\Documents',
##'     otpcon,
##'     originPoints,
##'     destinationPoints,
##'     startDateAndTime = "2018-08-18 12:00:00"
##'   )
##' @export
pointToPointLoop <- function(output.dir,
                             otpcon,
                             originPoints,
                             originPointsRow = 1,
                             destinationPoints,
                             destinationPointsRow = 1,
                             loopType = 0,
                             return = TRUE,
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
                             arriveBy = F) {
  message("Now running the propeR pointToPointLoop tool.\n")
  
  #########################
  #### SETUP VARIABLES ####
  #########################
  
  if (return == TRUE) {
    multiplier <- 2
  } else {
    multiplier <- 1
  }
  
  if (loopType == 0) {
    originPointsEnd <- nrow(originPoints)
    destinationPointsEnd <- nrow(destinationPoints)
  } else if (loopType == 1) {
    originPointsEnd <- nrow(originPoints)
    destinationPointsEnd <- 1
    
    destination_points_row_num <-
      destinationPointsRow # Set destination using a row from the origin dataframe
    
    if (destination_points_row_num > nrow(destinationPoints)) {
      message('Row is not in destination file')
      unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
      break
    }
    
  } else if (loopType == 2) {
    originPointsEnd <- 1
    destinationPointsEnd <- nrow(destinationPoints)
    
    origin_points_row_num <-
      originPointsRow # Set origin using a row from the origin dataframe
    
    if (origin_points_row_num > nrow(originPoints)) {
      message('Row is not in origin file')
      unlink(paste0(output.dir, "/tmp_folder"), recursive = TRUE) # Deletes tmp_folder if exists
      break
    }
    
  } else {
    message('Parameter type for loopType unknown')
    break
  }
  
  num <- 1
  total_num <- (originPointsEnd * destinationPointsEnd) * multiplier
  
  start_time <-
    format(as.POSIXct(startDateAndTime), "%I:%M %p") # Sets start time
  
  start_date <- as.Date(startDateAndTime) # Sets start date
  
  message("Creating ",
          total_num,
          " point to point connections, please wait...")
  
  time.taken <- vector()
  
  calls.list <- c(0)
  
  for (j in 1:multiplier) {
    for (k in 1:destinationPointsEnd) {
      for (i in 1:originPointsEnd) {
        start.time <- Sys.time()
        
        if (loopType == 0) {
          from_origin <- originPoints[i,]
          to_destination <- destinationPoints[k,]
        } else if (loopType == 1) {
          from_origin <- originPoints[i,]
          to_destination <- destinationPoints[destinationPointsRow, ]
        } else if (loopType == 2) {
          from_origin <- originPoints[originPointsRow,]
          to_destination <- destinationPoints[k, ]
        }
        
        if (j == 1) {
          from = from_origin
          to = to_destination
        } else {
          to = from_origin
          from = to_destination
        }
        
        call <- paste0(from$name, to$name)
        
        if (call %in% calls.list) {
          num <- num
          total_num <- total_num - 1
          message("Dropped a connection call as it has already been processed!")
          next
        } else {
          calls.list <- c(calls.list, call)
        }
        
        if (to$name == from$name) {
          num <- num
          total_num <- total_num - 1
          message("Dropped a connection call as origin and destination were the same!")
          next
        }
        
        point_to_point <- propeR::otpTripTime(
          otpcon,
          detail = TRUE,
          # Gives full breakdown of journey if TRUE
          from = from$lat_lon,
          # Takes the latitude and longitude from specified origin
          to = to$lat_lon,
          # Takes the latitude and longitude from specified destination
          modes = modes,
          date = start_date,
          # Takes the date as specified above
          time = start_time,
          # Takes the time as specified above
          maxWalkDistance = maxWalkDistance,
          walkReluctance = walkReluctance,
          walkSpeed = walkSpeed,
          bikeSpeed = bikeSpeed,
          minTransferTime = minTransferTime,
          maxTransfers = maxTransfers,
          wheelchair = wheelchair,
          arriveBy = arriveBy
        )
        
        if (num == 1) {
          # Start appending results
          point_to_point_table_overview <-
            point_to_point$itineraries
          
          if (point_to_point$errorId == "OK") {
            point_to_point_table_overview["origin"] <- from$name
            point_to_point_table_overview["destination"] <-
              to$name
            point_to_point_table_overview["status"] <-
              point_to_point$errorId
            point_to_point_table_overview["distance_km"] <-
              round(sum(point_to_point$output_table$distance) / 1000,
                    digits = 2)
            
            
          } else {
            # Cannot find journey
            point_to_point_table_overview["start"] <- 'N/A'
            point_to_point_table_overview["end"] <- 'N/A'
            point_to_point_table_overview["duration"] <- 'N/A'
            point_to_point_table_overview["walkTime"] <- 'N/A'
            point_to_point_table_overview["transitTime"] <- 'N/A'
            point_to_point_table_overview["waitingTime"] <- 'N/A'
            point_to_point_table_overview["transfers"] <- 'N/A'
            point_to_point_table_overview["origin"] <-
              from$name
            point_to_point_table_overview["destination"] <-
              to$name
            point_to_point_table_overview["status"] <-
              point_to_point$errorId
            point_to_point_table_overview["distance_km"] < 'N/A'
            
            
          }
          
        } else {
          point_to_point_table_overview_tmp <- point_to_point$itineraries
          
          if (point_to_point$errorId == "OK") {
            point_to_point_table_overview_tmp["origin"] <- from$name
            point_to_point_table_overview_tmp["destination"] <-
              to$name
            point_to_point_table_overview_tmp["status"] <-
              point_to_point$errorId
            point_to_point_table_overview_tmp["distance_km"] <-
              round(sum(point_to_point$output_table$distance) / 1000,
                    digits = 2)
            
            
          } else {
            # Cannot find journey
            point_to_point_table_overview_tmp["start"] <- 'N/A'
            point_to_point_table_overview_tmp["end"] <- 'N/A'
            point_to_point_table_overview_tmp["duration"] <- 'N/A'
            point_to_point_table_overview_tmp["walkTime"] <- 'N/A'
            point_to_point_table_overview_tmp["transitTime"] <-
              'N/A'
            point_to_point_table_overview_tmp["waitingTime"] <-
              'N/A'
            point_to_point_table_overview_tmp["transfers"] <- 'N/A'
            point_to_point_table_overview_tmp["origin"] <-
              from$name
            point_to_point_table_overview_tmp["destination"] <-
              to$name
            point_to_point_table_overview_tmp["status"] <-
              point_to_point$errorId
            point_to_point_table_overview_tmp["distance_km"] < 'N/A'
            
            
          }
          
          point_to_point_table_overview = rbind(point_to_point_table_overview,
                                                point_to_point_table_overview_tmp)
        }
        
        end.time <- Sys.time()
        
        time.taken[num] <- round(end.time - start.time, digits = 2)
        
        if (num < total_num) {
          message(
            num,
            " out of ",
            total_num,
            " connections complete. Time taken ",
            round(sum(time.taken), digits = 2),
            " seconds. Estimated time left is approx. ",
            round((mean(
              time.taken
            ) * total_num) - sum(time.taken),
            digits = 2),
            " seconds."
          )
        } else {
          message(
            num,
            " out of ",
            total_num,
            " connections complete. Time taken ",
            sum(time.taken),
            " seconds."
          )
        }
        num <- num + 1
      }
    }
  }
  message("Analysis complete, now saving outputs to ",
          output.dir,
          ", please wait.\n")
  
  stamp <-
    format(Sys.time(), "%Y_%m_%d_%H_%M_%S") # Windows friendly time stamp
  
  point_to_point_table_overview <-
    point_to_point_table_overview[, c(10, 8, 9, 1, 2, 11, 3, 4, 5, 6, 7)]
  
  colnames(point_to_point_table_overview) <-
    c(
      "status",
      "origin",
      "destination",
      "start_time",
      "end_time",
      "distance_km",
      "duration_mins",
      "walk_time_mins",
      "transit_time_mins",
      "waiting_time_mins",
      "transfers"
    )
  
  
  write.csv(
    point_to_point_table_overview,
    file = paste0(output.dir, "/p2p_loop-", stamp, ".csv"),
    row.names = FALSE
  ) # Saves trip details as a CSV
}
