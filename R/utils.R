#' Function that simply returns the empirical quantiles of a time series
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

#' Function that returns quantiles based on a table of mean and standard deviations
#'
#' @param tser Time Series
#' @param probs quantile levels
#'
#' @return
#' @export
#'
#' @examples
getquants.garch <- function(tableMuSig,
                            taus = c(0.025, 0.25, 0.5, 0.75, 0.975)){

  getqs <- function(mu, sig, taus){
    qVals <- sapply(taus, function(qlvl) qnorm(qlvl, mean = mu, sd = sig))
    names(qVals) <- paste0("q", taus)

    return(qVals)
  }

  quants <- apply(tableMuSig, 2, function(x) getqs(x[1], x[2], taus = taus))

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

  if(!lubridate::is.Date(currDate)){
    stop("currDate needs to be in Date format")
  }

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
#' @param currDate Date in the format YYYY-MM-DD
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


  if(!lubridate::is.Date(currDate)){
    stop("currDate needs to be in Date format")
  }

  .d <- `[`

  maxhor <- nrow(preds)

  horizons <- paste0(c(36,40,44,60,64,68), " hour")

  if(targetFullDate){
    FSDates <- c(currDate + lubridate::days(targetDiff[1]),
                 currDate + lubridate::days(targetDiff[2]))
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




windshell <- function(preds = NULL,
                      currDate,
                      returnNA = FALSE,
                      nameSeries = "wind"){

  if(is.null(preds) & !returnNA){

    stop("If not supplying predictions, returnNA needs to be set to TRUE")
  }

  if(!lubridate::is.Date(currDate)){
    stop("currDate needs to be in Date format")
  }

  if(is.null(preds)){
    preds <- data.table(q0.025 = rep(NA, 5),
                        q0.25 = rep(NA, 5),
                        q0.5 = rep(NA, 5),
                        q0.75 = rep(NA, 5),
                        q0.975 = rep(NA, 5))
  }

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


appendtargetFullDate <- function(predTable, targetDiff = c(2,3)){

  .d <- `[`

  currDate <- unique(predTable$forecast_date) |> as.Date()

  #return(currDate)
  if(unique(predTable$target) == "energy"){

    FSDates <- c(currDate + lubridate::days(targetDiff[1]),
                 currDate + lubridate::days(targetDiff[2]))
    FSTimes <- c("11:00:00", "15:00:00", "19:00:00")


    targetDates <- c(lubridate::as_datetime(paste(FSDates[1], FSTimes)),
                     lubridate::as_datetime(paste(FSDates[2], FSTimes)))

    predTable |>
      .d(, target_date := targetDates) |>
      .d()

  } else if(unique(predTable$target) == "dax"){
    stop("not implemented yet")
  }

  return(predTable)
}
