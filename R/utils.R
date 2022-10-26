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
                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975)){

  quants <- quantile(tser, probs = probs) |>
    setNames(paste0("q", probs))

  return(quants)
}



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
    .d(, forecast_date := preddates) |>
    .d(, target := rep(nameSeries, maxhor)) |>
    .d(, horizon := horizons)

  return(preds)
}

