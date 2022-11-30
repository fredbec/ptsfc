garch.predictions <- function(currDate,
                              armaOrder = c(0,0),
                              garchOrder = c(1,1),
                              targetFullDate = FALSE){


  .d <- `[`

  #reformat Date
  if(!lubridate::is.Date(currDate)){
    stop("currDate needs to be in Date format")
  }
  currDateForm <- as.character(format(currDate, format = "%Y%m%d"))

  #read in past Energy data
  pastDax <- data.table::fread(here("data", "dax",
                                    paste0("dax" , currDateForm, ".csv"))) |>
    #convert from IDate to Date (for garch fitting)
    .d(, date := as.Date(date))

  #get predictions for mean and standard deviation
  seriesNames <- paste0("logR", 1:5)
  tableMuSig <- sapply(seriesNames,
                       garch.model,
                       data = pastDax,
                       garchOrder = garchOrder,
                       armaOrder = armaOrder)

  #ensure strictly increasing uncertainty
  #it's crude I know
  for(i in 2:5){
    if(tableMuSig[2,i] < tableMuSig[2,i-1]){
      tableMuSig[2,i] <- tableMuSig[2,i-1] + 0.001
    }
  }

  #make predictions
  predictions <- getquants.garch(tableMuSig) |> t() |>
    data.frame() |>
    setDT() |>
    daxshell(currDate) |>
    .d()


  return(predictions)
}

#' Title
#'
#' @param pastDax
#' @param pastObs
#'
#' @importFrom purrr map_dfr
#' @import data.table
#' @import zoo
#'
#' @return data,table object with predictions
#' @export
#'
#' @examples
garch.model <- function(logRx,
                        data,
                        garchOrder = c(1,1),
                        armaOrder = c(0,0)){

  #make time series object
  daxTS <- zoo(x = data[[logRx]], order.by = data$date)

  #specify model
  varModel <- list(model = "sGARCH", garchOrder = garchOrder)
  garchSpec <- ugarchspec(varModel,
                          mean.model = list(armaOrder = armaOrder),
                          distribution.model = "std") # without fixed parameters here

  #fit Model
  garchFit <- ugarchfit(garchSpec, data = daxTS)

  #make forecast (each one step ahead)
  valsMuSig <- ugarchforecast(garchFit, n.ahead = 1,data = daxTS)

  return(c(mu = fitted(valsMuSig),
           sig = sigma(valsMuSig)))
}
