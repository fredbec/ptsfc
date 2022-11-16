library(RJSONIO)
library(data.table)
library(here)

#This is just a short script to get public holiday data from online


#only pulls those days which are public holidays THROUGHOUT Germany
#another option would be to get data for each state and weight the holiday
#according to the number of participating states

#also: bridge days^^?

.d <- `[`

years <- seq(2015,2022)

holidays <- purrr::map(years, function(yr) fromJSON(paste0(
  "https://feiertage-api.de/api/?jahr=", yr, "&nur_land=NATIONAL"
)) |>
  unlist() |>
  unname()) |>
  unlist() |>
  data.table() |>
  setnames(old = "V1", new = "date") |>
  .d(date != "") |>
  .d(, holiday := 1) |<
  .d()

data.table::fwrite(holidays, here("data", "holidays.csv"))
