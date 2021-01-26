
suppressPackageStartupMessages({
    require(data.table)
    require(jsonlite)
})

.args <- if (interactive()) c(
    "sindh.json",
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
), date ~ measure)[, location := "Sindh" ]

#' @examples 
#' require(ggplot2)
#' ggplot(sindh) +
#'   geom_point(aes(date, cases, colour = "cases"), alpha = 0.5) +
#'   geom_point(aes(date, deaths, colour = "deaths"), alpha = 0.5) +
#'   geom_line(aes(date, frollmean(cases,7), colour = "cases")) +
#'   geom_line(aes(date, frollmean(deaths,7), colour = "deaths")) +
#'   theme_minimal() +
#'   scale_y_log10() +
#'   scale_x_date(date_breaks = "months", date_labels = "%b")

saveRDS(sindh, tail(.args, 1))
