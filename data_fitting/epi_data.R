
suppressPackageStartupMessages({
    require(data.table)
    require(jsonlite)
})

.args <- if (interactive()) c(
    "sindh_datastudio.json",
    "epi_data.rds"
) else commandArgs(trailingOnly = TRUE)

sindh_raw <- fromJSON(.args[1])$default$dataResponse$dataSubset
case_inc <- 2
death_inc <- 5 #' only has initial dates
case_cum <- 1 #' for comparison
death_cum <- 4 #' for extension

extract <- function(index, dtraw, measure, cumulative = FALSE) {
    ds <- dtraw[[index]]$dataset$tableDataset
    n <- ds$size
    dat <- ds$column[[1]]
    res <- data.table(
      measure = measure,
      date   = as.Date(sindh_get(1, TRUE, n, dat), "%Y%m%d"),
      value  = sindh_get(2, FALSE, n, dat)
    )
    return(if (cumulative) {
        res[,.(date=date[-1], ddate = diff(date), value = diff(value), measure = measure[-1])][ddate == 1, .(measure, date, value)]
    } else res)
}

sindh_get = function(i, string, n, data) {
    indices = 0:(n - 1);
    present_indices = setdiff(indices, data$nullIndex[[i]]) + 1;
    present_values =  { if (string) data$stringColumn$values[[i]] else data$doubleColumn$values[[i]] };
    values = rep(NA, n);
    values[present_indices] = present_values;
    values
}

sindh <- dcast(rbind(
    extract(case_inc, sindh_raw, "cases"),
    extract(death_inc, sindh_raw, "deaths")[1:2],
    extract(death_cum, sindh_raw, "deaths", cumulative = TRUE)
), date ~ measure)[, location := "Sindh" ][, provenance := "reported" ]

#' want to fill in pre-April / late April data
#' determine pre-incidence deaths:
predeath <- extract(death_cum, sindh_raw, "deaths")[1][sindh, on=.(date), value - deaths, nomatch = 0 ]
precases <- extract(case_cum, sindh_raw, "cases")[1][sindh, on=.(date), value - cases, nomatch = 0 ]

#' need the all-Pakistan data
jhurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
casesurl <- sprintf("%s/time_series_covid19_confirmed_global.csv", jhurl)
deathsurl <- sprintf("%s/time_series_covid19_deaths_global.csv", jhurl)

fetch <- function(url, vn) melt(fread(url)[
  `Country/Region` == "Pakistan"
][, -c(1,3,4) ], id.vars = "Country/Region", variable.name = "date", value.name = vn)

#' fetch ECDC data; requires network connection
cases.dt <- fetch(casesurl, "cases")
deaths.dt <- fetch(deathsurl, "deaths")

res <- cases.dt[deaths.dt, on=.(`Country/Region`, date)]
res[, date := as.Date(date, format = "%m/%d/%y") ]

#' select the columns of interest; order by key columns
pakistan <- res[order(date),
             .(date, cases = c(cases[1], diff(cases)), deaths = c(deaths[1], diff(deaths))),
             keyby=.(
               location = `Country/Region`
             )
]

ref <- pakistan[sindh, on=.(date)][
  date < "2020-07-01", .(dp = sum(i.deaths)/sum(deaths), cp = sum(i.cases)/sum(cases))
]

res <- sindh[
  pakistan, on=.(date),
  .(
    location = "Sindh", date = date,
    cases = fifelse(is.na(cases), round(ref$cp*i.cases), cases),
    deaths = fifelse(is.na(deaths), round(ref$dp*i.deaths), deaths),
    provenance = fifelse(is.na(cases), "imputed", "reported")
  )
]

testing <- fread("testing.csv", col.names = c("date","tests"))

res[testing, on=.(date), testpos := cases / tests ]

#' @examples 
#' ggplot(res) + aes(date, color = provenance) +
#'  geom_line(aes(y=cases)) +
#'  geom_line(aes(y=deaths)) +
#'  scale_y_log10()
#'  
#' ggplot(res) + aes(date, color = provenance) +
#'  geom_line(aes(y=testpos)) +
#'  scale_y_continuous(trans = "logit")

saveRDS(res, tail(.args, 1))
