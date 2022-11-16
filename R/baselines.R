#' Title
#'
#' @param pastDax
#' @param pastObs
#'
#' @importFrom purrr map_dfr
#' @import data.table
#'
#' @return data,table object with predictions
#' @export
#'
#' @examples
dax.baseline <- function(pastDax,
                         currDate,
                         pastObs = 1000){

  .d <- `[`

  columns <- paste0("logR", seq(1,5))
  colapp <- paste0("logcLag", seq(1,5))

  logRs <- pastDax |>
    setDT() |>
    .d(, Date := as.Date(Date, origin = "1970-01-01")) |>
    .d(, .(Date, Close)) |>

    #impute nulls with average of surrounding values
    #crude, but only applies to two values
    .d(Close == "null", Close := 0) |>
    .d(, Close := as.numeric(Close)) |>
    .d(, leadC := shift(Close, -1)) |>
    .d(, lagC := shift(Close, 1)) |>
    .d(Close == 0, Close := (leadC+lagC)/2) |>

    #make log returns
    .d(, logc := log(Close)) |>
    .d(, paste0("logcLag", seq(1,5)) := shift(logc,seq(1,5))) |>
    .d(, (columns) := lapply(.SD, function(x) 100* (logc-x)), .SDcols = colapp) |>
    .d(, c("Date", paste0("logR", seq(1,5)))) |>
    .d(!is.na(logR5))

  preds <- purrr::map_dfr(logRs[, paste0("logR", seq(1,5))], getquants) |>
    setDT()

  daxshell(preds, currDate = currDate) |>
    setcolorder(neworder = c("forecast_date", "target", "horizon", paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))))

  return(preds)
}



#' Title
#'
#' @param pastDax
#' @param pastObs
#'
#' @importFrom purrr map_dfr
#' @import data.table
#'
#' @return data,table object with predictions
#' @export
#'
#' @examples
energy.baseline <- function(currDate,
                            hours = c(11,15,19),
                            wdays = c(6,7), #Friday and Saturday
                            pastObs = 100){

  .d <- `[`

  #read in past Energy data
  pastEner <- data.table::fread(here("data", "energy",
                                     paste0("energy_processed" , currDate, ".csv")))

  #filter out relevant hours
  #and keep only last pastObs relevant instances
  FSEner <- pastEner |>
    .d(hour %in% hours & wday %in% wdays) |>
    .d(, count := .N:1, by = c("hour", "wday")) |>
    .d(count <= pastObs) |>
    .d(, count := NULL) |>
    .d()

  #reshape to wide to be able to use getquants() function
  wideFSEner <- FSEner |>
    #change weekday to labeled (for dcast)
    .d(, wday := lubridate::wday(utchour, label = TRUE)) |>
    #for correct dcast attribution
    .d(, year := lubridate::year(utchour)) |>
    .d(, week := lubridate::week(utchour)) |>
    .d(, utchour := NULL) |>
    .d(, month := NULL) |>
    data.table::dcast(year + week ~ wday + hour, value.var = "dhr") |>
    .d()

  #get quantiles
  wlabs <- lubridate::wday(c(6,7), label = TRUE)
  columns <- c(sapply(paste0(wlabs, "_"), function(x) paste0(x, c(11,15,19))))

  predsEner <- purrr::map_dfr(wideFSEner[, ..columns], getquants, na.rm = TRUE) |>
    setDT() |>
    energyshell(as.Date(currDate, format = c("%Y%m%d")), targetFullDate = TRUE) |>
    .d()

  return(predsEner)
}


