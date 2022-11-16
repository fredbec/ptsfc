rq.energy.model <- function(currDate,
                            formula,
                            targetFullDate = FALSE){


  .d <- `[`
  cdatform <- as.character(format(currDate, format = "%Y%m%d"))

  energy <- data.table::fread(here("data", "energy", paste0("energy_processed", cdatform, ".csv")))

  energy <- energy |>
    .d(, holwkend := data.table::fcase(
      wkend == 1 | holiday == 1, 1,
      wkend == 0 & holiday == 0, 0
    )) |>
    .d(, summer := data.table::fcase(
      month %in% seq(4,9), 1,
      month %in% c(1,2,3,10,11,12),0)
    ) |>
    .d(, year := lubridate::year(utchour))


  #fit model
  rqmodel <- quantreg::rq(formula,
                          tau = c(0.025, 0.25, 0.5, 0.75, 0.975),
                          #weights = weights,
                          data = energy)

  #data to predict on
  preddat <- data.table(
    hour = c(11, 15, 19, 11, 15, 19),
    wday = rep(c(6,7), each = 3),
    holwkend = rep(c(0,1), each = 3),
    month = rep(11, times = 6)
  )


  qrpred <- predict(rqmodel, preddat) |>
    as.data.table()
  names(qrpred) <- paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))

  qrpred <- qrpred |>
    energyshell(currDate, targetFullDate = targetFullDate)

  return(qrpred)
}
