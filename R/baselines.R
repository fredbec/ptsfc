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
