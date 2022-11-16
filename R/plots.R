plot.energy.preds <- function(currDate,
                              enerPred,
                              hours = c(11,15,19),
                              wdays = c(6,7),
                              duration = weeks(2),
                              modelName = "model1"){

  .d <- `[`

  if(is.null(enerPred$model)){
    enerPred[, model := modelName]
  }

  plotData <- data.table::fread(here("data", "energy", paste0("energy_processed", currDate, ".csv"))) |>
    .d(utchour >= (as.Date(currDate, format = "%Y%m%d") - duration)) |>
    #highlight relevant hours in data
    .d(hour %in% hours & wday %in% wdays, relHour := 1)



  enerPlot <- ggplot2::ggplot(plotData, aes(x = utchour, y = dhr)) +
    geom_line() +

    #indicate same hours in realized time series
    geom_point(data = plotData[relHour == 1], aes(x = utchour, y = dhr), shape = 4, size = 3) +

    #points and line for predicted median level
    geom_point(data = enerPred,
               aes(x = target_date, y = q0.5, color = model),
               shape = 18, size = 3) +
    geom_line(data = enerPred,
              aes(x = target_date, y = q0.5, color = model)) +

    #small crosses to indicate quantiles
    lapply(c("q0.25", "q0.75", "q0.025", "q0.975"), function(qlvl) {
      list(
        geom_point(data = enerPred, aes(x = target_date, y = get(qlvl), color = model), shape = 4, alpha = 0.5)
      )}) +

    #prediction invervals
    geom_ribbon(data = enerPred,
                aes(x = target_date, y = q0.5, ymin = q0.25, ymax = q0.75, fill = model),
                alpha = 0.5) +
    geom_ribbon(data = enerPred,
                aes(x = target_date, y = q0.5, ymin = q0.025, ymax = q0.975, fill = model),
                alpha = 0.25) +

    #colors, theme
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal()

  return(enerPlot)

}
