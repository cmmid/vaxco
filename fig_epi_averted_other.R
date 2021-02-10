
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
    require(ggplot2)
    require(ggh4x)
})

.debug <- c("~/Dropbox/Covid-WHO-vax/outputs", "4000")
.args <- if (interactive()) sprintf(c(
    "%s/epi_quantile.rds",
    "%s/config.sqlite",
    "%s/figures/other_averted_%s.png"
), .debug[1], .debug[2])

dosetar <- as.integer(gsub("^.+_(\\d+)\\.(png|pdf)$","\\1",tail(.args, 1)))

epi.dt <- readRDS(.args[1])
setkeyv(epi.dt,c("id","age","qtile","anni_year"))

agg.dt <- epi.dt[, .(
    age="all",
    non_icu_i = sum(non_icu_severe_i+non_icu_critical_i),
    icu_i = sum(icu_critical_i),
    non_icu_p = sum(non_icu_severe_p+non_icu_critical_p),
    icu_p = sum(icu_critical_p),
    del.non_icu_i = sum(non_icu_severe_i.del+non_icu_critical_i.del),
    del.icu_i = sum(icu_critical_i.del),
    del.non_icu_p = sum(non_icu_severe_p.del+non_icu_critical_p.del),
    del.icu_p = sum(icu_critical_p.del)
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
full.dt$strategy_str <- as.numeric(full.dt$strategy_str)
full.dt[strategy_str == 0, strategy_str := Inf]

full.dt[
    order(anni_year),
    c("c_icu_i.av","c_nonicu_i.av","c_icu_p.av","c_nonicu_p.av") :=
    .(cumsum(del.icu_i), cumsum(del.non_icu_i), cumsum(del.icu_p), cumsum(del.non_icu_p)), by=.(id, qtile)
]

fig2.dt <- dcast(melt(full.dt[
    qtile != "mn" &
    strategy != "none" & start_timing == 18718 &
    doses_per_day == dosetar & vax_delay == 30 &
    #vax_eff == 0.7 & nat_imm_dur_days == round(2.5*365) &
    strategy_str == 365
], id.vars = c("id", "qtile", "anni_year", "vax_imm_dur_days", "from_age", "vax_eff", "nat_imm_dur_days"),
measure.vars = c("c_icu_i.av","c_nonicu_i.av","c_icu_p.av","c_nonicu_p.av")), id + anni_year + vax_imm_dur_days + from_age + variable + vax_eff + nat_imm_dur_days ~ qtile)

fig2.dt[, vdur := factor(fifelse(
    is.infinite(vax_imm_dur_days), "Life-long",
    sprintf("%0.2g yr%s", vax_imm_dur_days/365, fifelse(vax_imm_dur_days/365==1,"","s"))
), levels = c(sprintf("%0.2g yr%s", c(1,2.5,5), c("","s","s")), "Life-long"), ordered = TRUE)]

fig2.dt[, ndur := factor(fifelse(
    is.infinite(nat_imm_dur_days), "Life-long",
    sprintf("%0.2g yr%s", nat_imm_dur_days/365, fifelse(nat_imm_dur_days/365==1,"","s"))
), levels = c(sprintf("%0.2g yr%s", c(1,2.5,5), c("","s","s")), "Life-long"), ordered = TRUE)]

#fig2.dt[, measure := factor(variable, levels = c("ccases.av","cdeaths.av"), ordered = TRUE)]

refdate <- as.Date("2020-04-01")

fig2.p <- function(dt) ggplot(dt) + aes(
    refdate + anni_year*365,
    color = as.integer(vdur),
    fill = as.integer(vdur),
    group = vdur
) + facet_nested(
    variable + ndur ~ vax_eff + from_age, scales = "free_y",
    labeller = labeller(
        variable = c(
            c_icu_i.av = "ICU Admissions", c_nonicu_i.av = "Ward Admissions",
            c_icu_p.av = "ICU Person-Days", c_nonicu_p.av = "Ward Person-Days"
        ),
        from_age = c(`4`="15+",`14`="65+"),
        vax_eff = function(l) sprintf(c("Vax. Eff = %g%%",rep("%g%%", length(l)-1)),as.numeric(l)*100)
    ), switch = "y"
) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95, color = NULL), alpha = 0.2, show.legend = FALSE) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, color = NULL), alpha = 0.35, show.legend = FALSE) +
    geom_line(aes(y=md)) +
    scale_color_continuous(
        "Vaccine Protection Expected Duration",
        labels = levels(fig2.dt$vdur),
        guide = "legend", aesthetics = c("color","fill")
    ) +
    scale_x_date(
        "Calendar Year",
        breaks = refdate + (1:10)*365,
        date_labels = "'%y"
    ) +
    scale_y_continuous(NULL, labels = scales::label_number_si()) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_minimal() +
    theme(
        text = element_text(size = 8), 
        legend.position = "top",
        strip.placement = "outside",
        panel.grid.minor = element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=0.25)
    )

fig2.icu <- fig2.p(fig2.dt[variable %like% "_icu"])
fig2.nonicu <- fig2.p(fig2.dt[variable %like% "_nonicu"])

ggsave(tail(.args, 1), fig2.icu, width = 8, height = 8, units = "in", dpi = 600)

ggsave(gsub("\\.","_non.",tail(.args, 1)), fig2.nonicu, width = 8, height = 8, units = "in", dpi = 600)