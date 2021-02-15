
suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(ggh4x)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/epi_quantile.rds",
    "%s/config.sqlite",
    "%s/figures/baseline.png"
), .debug)

epi.dt <- readRDS(.args[1])

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

#' only looking at the earlier start
full.dt <- agg.dt[
    scn, on=.(id)
][strategy == "none" & start_timing == min(start_timing)]

fig.dt <- melt(
    full.dt, id.vars = c("nat_imm_dur_days", "qtile", "anni_year"),
    measure.vars = c("cases","deaths")
)

fig.dt[, ndur := factor(fifelse(
    is.infinite(nat_imm_dur_days), "Life-long",
    sprintf("%0.2g year%s", nat_imm_dur_days/365, fifelse(nat_imm_dur_days/365==1,"","s"))
), levels = c(sprintf("%0.2g year%s", c(1,2.5,5),c("","s","s")), "Life-long"), ordered = TRUE)]

refdate <- as.Date("2021-04-01")
fig.dt[, anni_date := refdate + anni_year*365 ]

fig.wide <- dcast(fig.dt, ndur + variable + anni_date ~ qtile, value.var = "value")



fig.p <- ggplot(fig.wide) + aes(anni_date) +
    facet_grid(
        variable ~ ndur, scales = "free_y", labeller = labeller(
            ndur = function(nd) sprintf(c("Natural Immunity\n%s", rep("\n%s", length(nd)-1)), nd),
            variable = c(cases = "Annual Cases", deaths = "Annual Deaths")
        ), switch = "y"
    ) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.3) +
    geom_ribbon(aes(ymin=lo50, ymax=hi50), alpha = 0.3) +
    geom_line(aes(y = md)) +
    scale_y_continuous(NULL, labels = scales::label_number_si()) +
    scale_x_date(
        "Calendar Year",
        breaks = refdate + (1:10)*365,
        date_labels = "'%y"
    ) +
    theme_minimal() +
    theme(
        legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.direction = "horizontal", strip.placement = "outside",
        panel.grid.minor = element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=0.5)
    )

ggsave(tail(.args, 1), fig.p, width = 8, height = 5, dpi = 600)
