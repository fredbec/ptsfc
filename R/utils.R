#' Title
#'
#' @param tser Time Series
#' @param probs quantile levels
#'
#' @return
#' @export
#'
#' @examples
getquants <- function(tser,
                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      na.rm = FALSE){

  quants <- quantile(tser, probs = probs, na.rm = na.rm) |>
    setNames(paste0("q", probs))

  return(quants)
}



#' Title
#'
#' @param preds
#' @param currDate
#' @param weekday
#' @param nameSeries
#'
#' @return a data.table corresponding to the challenge's format requirements
#'
#'
daxshell <- function(preds,
                     currDate,
                     weekday = "Wednesday",
                     nameSeries = "DAX"){

  .d <- `[`

  maxhor <- nrow(preds)

  preddates <- seq.Date(from = currDate + 1,
                        to = (currDate + maxhor + 2),
                        by = 1)
  horizons <- paste0(seq(1:(maxhor+2)), " day")

  nWk <- !weekdays(preddates) %in% c("Saturday", "Sunday")

  preddates <- preddates[nWk]
  horizons <- horizons[nWk]

  preds <- preds |>
    .d(, forecast_date := rep(currDate, maxhor)) |>
    .d(, target := rep(nameSeries, maxhor)) |>
    .d(, horizon := horizons) |>
    setcolorder(c("forecast_date", "target", "horizon", paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))))

  return(preds)
}


#' Title
#'
#' @param preds
#' @param currDate
#' @param weekday
#' @param nameSeries
#'
#' @return a data.table corresponding to the challenge's format requirements
#'
#'
energyshell <- function(preds,
                        currDate,
                        targetFullDate = FALSE,
                        targetDiff = c(2,3),
                        weekday = "Wednesday",
                        nameSeries = "energy"){

  .d <- `[`

  maxhor <- nrow(preds)

  horizons <- paste0(c(36,40,44,60,64,68), " hour")

  if(targetFullDate){
    FSDates <- c(as.Date(currDate) + lubridate::days(targetDiff[1]),
                 as.Date(currDate) + lubridate::days(targetDiff[2]))
    FSTimes <- c("11:00:00", "15:00:00", "19:00:00")


    targetDates <- c(lubridate::as_datetime(paste(FSDates[1], FSTimes)),
                     lubridate::as_datetime(paste(FSDates[2], FSTimes)))
  }

  preds <- preds |>
    .d(, forecast_date := currDate) |>
    .d(, target := rep(nameSeries, maxhor)) |>
    .d(, horizon := horizons) |>
    setcolorder(neworder = c("forecast_date", "target", "horizon",
                             paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))))

  if(targetFullDate){
    preds |>
      .d(, target_date := targetDates)
  }
  return(preds)
}




windshell <- function(preds,
                      currDate,
                      nameSeries = "wind"){

  .d <- `[`

  maxhor <- nrow(preds)

  horizons <- paste0(c(36,48,60,72,84), " hour")

  preds <- preds |>
    .d(, forecast_date := currDate) |>
    .d(, target := rep(nameSeries, maxhor)) |>
    .d(, horizon := horizons) |>
    setcolorder(neworder = c("forecast_date", "target", "horizon", paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))))

  return(preds)
}
