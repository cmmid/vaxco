suppressPackageStartupMessages({
    require(data.table)
    require(wpp2019)
})

.args <- if (interactive()) c(
    "mob_data.rds"
) else commandArgs(trailingOnly = TRUE)

# MOBILITY DATA
# NOTE: These are large and publicly available files, so they have been left off the repo.
# For Google Community Mobility reports, download the "Global CSV" from https://www.google.com/covid19/mobility/
# For OxCGRT, download latest data CSV from https://github.com/OxCGRT/covid-policy-tracker/tree/master/data

mob.dt <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")[
    country_region_code == "PK" & sub_region_1 == "Sindh"
]

nonschool <- melt(
    mob.dt, measure.vars = 9:14
)[, variable := gsub("_percent_change_from_baseline","", variable )][,
    .(date = as.Date(date), context = variable, change = value / 100)
]

ox.dt <- melt(fread(
    "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_school_closing.csv"
)[country_code == "PAK", .SD, .SDcols = -c(1,3)], id.vars = "country_code")
ox.dt[, date := as.Date(variable, "%d%b%Y")]

school <- ox.dt[!is.na(value),
    .(date, context = "school", change = -value/3)
]

both.dt <- rbind(nonschool, school)[, location := "Sindh" ]

saveRDS(both.dt, tail(.args, 1))

#' @examples 
#' require(ggplot2)
#' ggplot(both.dt) + aes(date, change, color = context) +
#'   geom_line() +
#'   scale_x_date(NULL, date_breaks = "months", date_labels = "%b") +
#'   theme_minimal()