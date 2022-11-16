cv.energy.rq <- function(currDate,
                         formula,
                         modelName = "model1"){

  .d <- `[`

  energy <- data.table::fread(here("data", "energy", paste0("energy_processed", currDate, ".csv")))

  energy <- format.energy.byhour(currDate) |>
    .d(, holwkend := data.table::fcase(
      wkend == 1 | holiday == 1, 1,
      wkend == 0 & holiday == 0, 0
    )) |>
    .d(, summer := data.table::fcase(
      month %in% seq(4,9), 1,
      month %in% c(1,2,3,10,11,12),0)
      ) |>
    .d(, year := lubridate::year(utchour))

  energySplit <- energy |>
    split(by = c("summer", "year"))

  getinds <- function(excl,
                      maxind = length(energySplit)){

    inds <- seq(1,maxind)[-excl]
    return(inds)
  }

  cvdats <- purrr::map(seq(1,length(energySplit)), function(ind)
    rbindlist(energySplit[getinds(ind)]))


  cvRes <- vector(mode = "list", length = length(energySplit))
  for(i in 1:length(energySplit)){

    rqmodel <- quantreg::rq(formula,
                  tau = c(0.025, 0.25, 0.5, 0.75, 0.975),
                  #weights = weights,
                  data = cvdats[[i]])

    qrpred <- predict(rqmodel, energySplit[[i]]) |>
      as.data.table()
    names(qrpred) <- paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975))


    valdata <- energySplit[[i]] |>
      .d(, .(date, utchour, dhr)) |>
      cbind(qrpred)

    cvRes[[i]] <- valdata |>
      data.table::melt(id.vars = c("date", "utchour", "dhr"),
                       measure.vars = paste0("q", c(0.025, 0.25, 0.5, 0.75, 0.975)),
                       variable.name = "quantile", value.name = "prediction") |>
      setnames(old = "dhr", new = "true_value") |>
      .d(,quantile := as.numeric(gsub("q", "", quantile))) |>
      .d(, model := modelName) |>
      scoringutils::score(metrics = "interval_score") |>
      scoringutils::summarise_scores(by = "model") |>
      .d(, fold := i)

  }
  cvRes <- rbindlist(cvRes)

  return(cvRes)
}


