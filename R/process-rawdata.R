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
format.energy.byhour <- function(currDate,
                                 pathFrom = here("data", "energy", "raw"),
                                 pathTo = here("data", "energy")){

  rawData <- data.table::fread(here(pathFrom, paste0("energy", currDate, ".csv")))

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
    .d(, utchour := floor_date(utcdt, unit = "hour")) |>
    .d(, `:=` (count = .N), by = list(utchour)) |>
    #remove if count < 4 (this only happens at the end of the data, if no full hour was available)
    .d(count == 4) |>
    .d(,count := NULL) |>
    .d(, .(dhr = sum(demand)), by = utchour)
    .d()

  return(processedData)


}

