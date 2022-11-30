#' Title
#'
#' @param currDate
#' @param enerPred energy prediction (as returned by energy.shell)
#' @param weekday
#' @param nameSeries
#'
#' @return a data.table corresponding to the challenge's format requirements
#'

plot.energy.preds <- function(currDate,
                              enerPred,
                              hours = c(11,15,19),
                              wdays = c(6,7),
                              duration = weeks(2),
                              modelName = "model1"){

  .d <- `[`

  #reformat Date to read in data
  if(!lubridate::is.Date(currDate)){
    stop("currDate needs to be in Date format")
  }
  currDateForm <- as.character(format(currDate, format = "%Y%m%d"))

  if(is.null(enerPred$target_date)){
    stop("for this function, predictions need to include a column for the target date. Simply do this with the energy.shell function")
  }

  if(is.null(enerPred$model)){
    enerPred[, model := modelName]
  }

  plotData <- data.table::fread(
    here("data", "energy", paste0("energy_processed", currDateForm, ".csv"))) |>
    .d(utchour >= (currDate - duration)) |>
    #highlight relevant hours in data
    .d(hour %in% hours & wday %in% wdays, relHour := 1)



  enerPlot <- ggplot2::ggplot(plotData, ggplot2::aes(x = utchour, y = dhr)) +
    ggplot2::geom_line() +

    #indicate same hours in realized time series
    ggplot2::geom_point(data = plotData[relHour == 1],
                        ggplot2::aes(x = utchour, y = dhr), shape = 4, size = 3) +

    #points and line for predicted median level
    ggplot2::geom_point(data = enerPred,
                        ggplot2::aes(x = target_date, y = q0.5, color = model),
               shape = 18, size = 3) +
    ggplot2::geom_line(data = enerPred,
                       ggplot2::aes(x = target_date, y = q0.5, color = model)) +

    #small crosses to indicate quantiles
    lapply(c("q0.25", "q0.75", "q0.025", "q0.975"), function(qlvl) {
      list(
        ggplot2::geom_point(data = enerPred,
                            ggplot2::aes(x = target_date, y = get(qlvl), color = model),
                            shape = 4, alpha = 0.5)
      )}) +

    #prediction invervals
    ggplot2::geom_ribbon(data = enerPred,
                         ggplot2::aes(x = target_date, y = q0.5,
                                      ymin = q0.25, ymax = q0.75, fill = model),
                alpha = 0.5) +
    ggplot2::geom_ribbon(data = enerPred,
                ggplot2::aes(x = target_date, y = q0.5,
                             ymin = q0.025, ymax = q0.975, fill = model),
                alpha = 0.25) +

    #colors, theme
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::theme_minimal()

  return(enerPlot)

}
