#' Adds an intervalType column to the dataFrame containing an integer from 1 till number of intervalTypes
#'
#' @param dataFrame A data frame with activity data
#' @param intervalType A character value containing the used intervalType
#' @param startTime The starting time (1-24) used for when specified intervalType is selected
#' @param endTime The ending time (1-24) used for when specified intervalType is selected

#' @importFrom dplyr select mutate %>%

#' @export

addInterval <- function(dataFrame, intervalType, startTime = c(), endTime = c())
{

      ## INPUT CHECKS ==========================================================

      if (!class(dataFrame)[1] %in% c("data.table", "data.frame", "tbl_dt")) {
            stop(sprintf('The classification data is not provided as a data.frame, data.table or tbl_dt. Instead it is: %s', class(dataFrame)))
      }

      if  (!all(c("start", "duration") %in% colnames(dataFrame))) {
            stop('Column names are incorrect')
      }

      if (class(dataFrame$start)[1] != "POSIXct") {
            stop('start column is not of type POSIXct')
      }

      if ((class(intervalType) != "character") | (length(intervalType) > 1)) {
            stop('intervalType is incorrect')
      }

      if (intervalType == "specified") {
            if (length(startTime) == 0 | length(endTime) == 0) {
                  stop('startTime and/or endTime are not defined')
            }
            if (((class(startTime) != 'numeric') & (class(startTime) != 'integer')) |
                ((class(endTime) != 'numeric') & (class(endTime) != 'integer'))) {
                  stop('startTime and/or endTime is not of class numeric or integer')
            }
            if ((startTime > endTime) | (startTime < 0) | (endTime > 24))
            {
                  stop('startTime and/or endTime is not correct')
            }
      }

      if (exists("intervalTypeVec", envir = environment())) {
            rm(intervalTypeVec)
      }

      if (all(is.na(dataFrame$duration))) {
            dataFrame$interval <- factor(1)
            return(dataFrame)
      }

      ## ALGORITHM =============================================================
      data <- select(dataFrame, start, duration)

      if (intervalType == "total") {
            # Everything is 1
            intervalTypeVec <- rep(1, nrow(data))

      } else if (intervalType == "day") {
            if (any(dataFrame$duration > 24*60*60)) {stop(sprintf('Duration values above 1 day occur, while trying to add a "day" interval. Max length was %1.3f hours.', max(dataFrame$duration)/60/60))}

            # Get the day number (1-31)
            result <- data.frame(intervalType = as.numeric(format(data$start,"%d")))

      } else if (intervalType == "hour") {
            if (any(dataFrame$duration > 60*60)) {stop(sprintf('Duration values above 1 hour occur, while trying to add a "hour" interval. Max length was %1.3f minutes.', max(dataFrame$duration)/60))}

            # Get a combination value of day and hour
            result <- data.frame(intervalType = as.numeric(format(data$start,"%d"))*100 + as.numeric(format(data$start,"%H")))

      } else if (intervalType == "minute") {
            if (any(dataFrame$duration > 60.1)) {stop(sprintf('Duration values above 1 min occur, while trying to add a "minute" interval. Max length was %1.3f seconds.', max(dataFrame$duration)))}

            # Get a combination value of day and minute
            result <- data.frame(intervalType = as.numeric(format(data$start,"%d"))*10000 + as.numeric(format(data$start,"%H"))*100 + as.numeric(format(data$start,"%M")))

      } else if (intervalType == "noon") {
            if (any(dataFrame$duration > 24*60*60)) {stop(sprintf('Duration values above 1 day occur, while trying to add a "noon" interval. Max length was %1.3f hours.', max(dataFrame$duration)/60/60))}

            # Add half a day to timestamp and get the day number (1-31)
            result <- data.frame(intervalType = as.numeric(format(data$start + 43200,"%d")))

      } else if (intervalType == "daypart") {
            if (any(dataFrame$duration > 6*60*60)) {stop(sprintf('Duration values above 0.25 day occur, while trying to add a "daypart" interval. Max length was %1.3f hours.', max(dataFrame$duration)/60/60))}

            hoursVector <- as.numeric(format(data$start,"%H"))
            dayVec   <- as.numeric(format(data$start,"%d"))*100

            # Get the hoursVector and give a value to every daypart
            hoursVector[ hoursVector < 6 ] <- 1
            hoursVector[ hoursVector >= 6  & hoursVector < 12 ] <- 2
            hoursVector[ hoursVector >= 12 & hoursVector < 18 ] <- 3
            hoursVector[ hoursVector >= 18 ] <- 4

            # Create a interval list based on the result
            intervalTypeVec <- 1 + c(0,cumsum(pmin(abs(diff(hoursVector + dayVec)),1)))

      } else if (intervalType == "24h") {
            if (any(dataFrame$duration > 24*60*60)) {stop(sprintf('Duration values above 1 day occur, while trying to add a "24h" interval. Max length was %1.3f hours.', max(dataFrame$duration)/60/60))}

            # Get the elapsed time from the beginning and divide by 24 hours
            intervalTypeVec <- ceiling(((as.numeric(data$start) + data$duration - as.numeric(data$start[1])))
                                       /60/60/24)
            intervalTypeVec[1] <- 1

      } else if (intervalType == "week") {

            # Get the elapsed time from the beginning and divide by weeks
            intervalTypeVec <- ceiling(((as.numeric(data$start) + data$duration - as.numeric(data$start[1])))
                                       /60/60/24/7)
            intervalTypeVec[1] <- 1

      } else if (intervalType == "specified") {
            if (any(dataFrame$duration > (endTime-startTime)*60*60)) {
                  stop(sprintf('Duration values above %1.1f hour occur, while trying to add a "specified" interval with start %1.1f hour and end %1.1f hour. Longest was %1.3f hours.', endTime-startTime, startTime, endTime, max(dataFrame$duration)/60/60))}

            # Get hours and remove not wanted hours
            result <- data.frame(intervalType = as.numeric(format(data$start,"%d")),
                                 hour = as.numeric(format(data$start,"%H"))) %>%
                  filter(hour >= startTime & hour < endTime) %>%
                  select(-hour)

            dataFrame <- dataFrame %>%
                  mutate(hour = as.numeric(format(data$start,"%H"))) %>%
                  filter(hour >= startTime & hour < endTime) %>%
                  select(-hour)

      } else if (intervalType == "week_day") {
            if (any(dataFrame$duration > 24*60*60)) {stop(sprintf('Duration values above 1 day occur, while trying to add a "week_day" interval. Max length was %1.3f hours', max(dataFrame$duration)/60/60))}

            # Divide per week + divide by day and combine results
            intervalTypeVec <- ceiling(((as.numeric(data$start) + data$duration - as.numeric(data$start[1])))
                                       /60/60/24/7)
            intervalTypeVec[1] <- 1

            intervalTypeVec <- intervalTypeVec + as.numeric(format(data$start,"%d"))

      } else {
            stop("Invalid intervalType argument")
      }

      # Create an interval list for results that are not an interval list yet.
      if (!exists("intervalTypeVec", envir = environment())) {
            intervalTypeVec <-   1 + c(0,cumsum(pmin(abs(diff(result$intervalType)),1)))
      }

      ## Add the intervalType vector to the output
      output <- mutate(dataFrame, interval = factor(intervalTypeVec))

      return(output)
}