
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
    require(ggplot2)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/epi_quantile_ext.rds",
    "%s/config_ext.sqlite",
    "%s/test.png"
), .debug) else commandArgs(trailingOnly = TRUE)

epi.dt <- readRDS(.args[1])
setkeyv(epi.dt,c("id","age","qtile","anni_year"))

agg.dt <- epi.dt[, .(
    age="all", cases = sum(cases), deaths = sum(death_o),
    del.cases = sum(cases.del), del.deaths = sum(death_o.del)
), by=setdiff(key(epi.dt),"age")]

readDBtable <- function(
    fl, tbl = "scenario",
    drv = RSQLite::SQLite(), flags = SQLITE_RO
) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

scn <- readDBtable(.args[2])[, .(
    id, strategy, vax_eff, nat_imm_dur_days, vax_imm_dur_days,
    start_timing, vax_delay, doses_per_day, strategy_str, from_age
)]

full.dt <- agg.dt[scn, on=.(id)]

base.scn <- scn.focus[nat_imm_dur_days == 365 & doses_per_day == 4000]

base.pl <- agg.dt[base.scn, on=.(id)][!is.na(nat_imm_dur_days)]

scale_x_anni_year <- function(
    name = "Years Since Initial Vaccination",
    breaks = 1:10,
    ...
) scale_x_continuous(name, breaks, ...)

p <- ggplot(
    base.pl[qtile == "md"][strategy != "none" & (strategy_str == 365 & doses_per_day == 8000)]) +
    aes(
        anni_year, del.deaths,
        color = vax_eff, linetype = factor(from_age), group = interaction(vax_eff, from_age)
    ) +
    facet_nested(
        start_timing + vax_imm_dur_days ~ doses_per_day,
        labeller = labeller(
            doses_per_day = function(dpd) sprintf(
                c("Initial Doses Per Day\n= %s", rep("\n%s", length(dpd)-1)),
                dpd
            ),
            vax_imm_dur_days = function(dur) sprintf(c(
                "Vaccine Immunity\nDuration =%s",
                rep("\n%s", length(dur)-1)
            ), round(as.numeric(dur)/365, digits = 1)),
            start_timing = function(st) as.Date(as.integer(st), origin = "1970-01-01")
        )
    ) +
    geom_line() +
    # geom_line(
    #     aes(color = "no vaccine", linetype = "no vaccine"),
    #     data = base.pl[
    #         qtile == "md" & strategy == "none", .(
    #             deaths, anni_year, start_timing,
    #             vax_imm_dur_days = Inf, doses_per_day = 8000
    #         )
    #     ]
    # ) +
    # geom_line(
    #     aes(color = "no vaccine", linetype = "no vaccine"),
    #     data = base.pl[
    #         qtile == "md" & strategy == "none", .(
    #             deaths, anni_year, start_timing,
    #             vax_imm_dur_days = 365, doses_per_day = 8000
    #         )
    #     ]
    # ) +
    # geom_line(
    #     aes(color = "no vaccine", linetype = "no vaccine"),
    #     data = base.pl[
    #         qtile == "md" & strategy == "none", .(
    #             deaths, anni_year, start_timing,
    #             vax_imm_dur_days = 912, doses_per_day = 8000
    #         )
    #     ]
    # ) +
    # geom_line(
    #     aes(color = "no vaccine", linetype = "no vaccine"),
    #     data = base.pl[
    #         qtile == "md" & strategy == "none", .(
    #             deaths, anni_year, start_timing,
    #             vax_imm_dur_days = 1825, doses_per_day = 8000
    #         )
    #     ]
    # ) +
    scale_x_anni_year() +
    scale_y_continuous(labels = scales::label_number_si()) +
    scale_color_continuous(guide = "legend", breaks = base.pl[qtile == "md"][strategy != "none", sort(unique(vax_eff))]) +
    coord_cartesian(expand = FALSE) +
    theme_minimal() +
    theme(
        legend.position = "bottom"
    )
