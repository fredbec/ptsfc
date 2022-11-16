#' Converts to UTC and aggregates by hour
#'
#' @param currDate
#' @param pathFrom
#' @param pathTo
#'
#' @import data.table
#' @import lubridate
#'
#' @return
#' @export
#'
#' @examples
format.energy.byhour <- function(currDate,
                                 pathFrom = here("data", "energy", "raw"),
                                 pathTo = here("data", "energy")){

  rawData <- data.table::fread(here(pathFrom, paste0("energy", currDate, ".csv")))

  #to merge holiday info
  holidayData <- data.table::fread(here("data", "holidays.csv"))

  #for piping data.table
  .d <- `[`

  #first, convert everything to UTC
  processedData <- rawData |>
    #to identify shifts in Daylight Savings Time
    .d(, dupid := paste0(date, time)) |>
    .d(, `:=` (count = .N, scount = 1:.N), by = dupid) |>
    .d(, dupid := NULL) |>
    .d(, time := time |> paste0(":00")|> hms::as.hms(tz = "Europe/Berlin")) |>
    .d(, date := date |> lubridate::parse_date_time(c("dmy"), tz = "Europe/Berlin")) |>
    .d(, cestdt := lubridate::as_datetime(paste(date, time), tz = "Europe/Berlin")) |>
    .d(, utcdt := lubridate::with_tz(cestdt, tzone = "utc")) |>
    #manually change UCT for October time shift
    .d(count == 2 & scount == 1, utcdt := utcdt - hours(1)) |>
    #remove counters
    .d(, count := NULL) |>
    .d(, scount := NULL) |>

    #now, aggregate by hour
    .d(, utchour := lubridate::floor_date(utcdt, unit = "hour")) |>
    .d(, `:=` (count = .N), by = list(utchour)) |>
    #remove if count < 4 (this only happens at the end of the data, if no full hour was available)
    .d(count == 4) |>
    .d(,count := NULL) |>
    .d(, .(dhr = sum(demand)), by = utchour) |>


    #include some variables for hour, weekday, date, ...
    .d(, date := lubridate::date(utchour)) |>
    .d(, hour := lubridate::hour(utchour)) |>
    .d(, wday := lubridate::wday(utchour)) |>
    .d(, month := lubridate::month(utchour)) |>
    .d(, wkend := data.table::fcase(
      wday %in% c(1,7), 1,
      wday %in% seq(2,6), 0
    )) |>

    #merge holiday info
    merge(holidayData, by = "date", all.x = TRUE) |>
    .d(is.na(holiday), holiday := 0) |>
    .d()


  data.table::fwrite(processedData, here(pathTo, paste0("energy_processed", currDate, ".csv")))
  return(processedData)


}


#' Converts to UTC
#'
#' @param currDate
#' @param pathFrom
#' @param pathTo
#'
#' @import data.table
#' @import lubridate
#'
#' @return
#' @export
#'
#' @examples
format.dax<- function(currDate,
                      pathFrom = here("data", "dax", "raw"),
                      pathTo = here("data", "dax")){

  rawData <- data.table::fread(here(pathFrom, paste0("dax", currDate, ".csv")))

  #for piping data.table
  .d <- `[`

  columns <- paste0("logR", seq(1,5))
  colapp <- paste0("logcLag", seq(1,5))

  #first, convert everything to UTC
  processedData <- rawData |>
    setDT() |>
    #impute nulls with average of surrounding values
    #crude, but only applies to two values
    .d(!is.na(close)) |>
    .d(, close := as.numeric(close)) |>
    .d(, leadC := shift(close, -1)) |>
    .d(, lagC := shift(close, 1)) |>

    #make log returns
    .d(, logc := log(close)) |>
    .d(, paste0("logcLag", seq(1,5)) := shift(logc,seq(1,5))) |>
    .d(, (columns) := lapply(.SD, function(x) 100* (logc-x)), .SDcols = colapp) |>
    .d(!is.na(logR5))


  data.table::fwrite(processedData, here(pathTo, paste0("dax", currDate, ".csv")))

  return(processedData)


}

