
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
    require(ggplot2)
    require(ggh4x)
})

.debug <- c("~/Dropbox/Covid-WHO-vax", "4000")
.args <- if (interactive()) sprintf(c(
    "%s/outputs/epi_quantile.rds",
    #"~/Downloads/from_dstl_orig/epi_quantile.rds",
    "%s/inputs/config.rds",
    "%s/figures/mech_averted_%s.png"
), .debug[1], .debug[2])

dosetar <- as.integer(gsub("^.+_(\\d+)\\.(png|pdf)$","\\1",tail(.args, 1)))

scn <- readRDS(.args[2])[, .(
    id, strategy, vax_eff, nat_imm_dur_days, vax_imm_dur_days,
    start_timing, vax_delay, doses_per_day, strategy_str, from_age,
    vax_mech, eff_mech, increasing
)][
    start_timing == 18718 &
    doses_per_day == dosetar & vax_delay == 30 &
    vax_eff == 0.7 & nat_imm_dur_days == round(2.5*365) &
    vax_imm_dur_days == round(2.5*365) &
    strategy_str == 365 &
    increasing == TRUE
]

scn <- scn[strategy != "none"]

epi.dt <- readRDS(.args[1])[id %in% scn$id]
setkeyv(epi.dt,c("id","age","qtile","anni_year"))

agg.dt <- epi.dt[, .(
    age="all", cases = sum(cases), deaths = sum(death_o),
    del.cases = sum(cases.del), del.deaths = sum(death_o.del)
), by=setdiff(key(epi.dt),"age")]

full.dt <- agg.dt[scn, on=.(id)]
full.dt$strategy_str <- as.numeric(full.dt$strategy_str)
full.dt[strategy_str == 0, strategy_str := Inf]

full.dt[order(anni_year), c("cdeaths.av","ccases.av") := .(cumsum(del.deaths),cumsum(del.cases)), by=.(id, qtile)]

fig2.dt <- dcast(melt(
    full.dt[qtile != "mn"],
    id.vars = c("id", "qtile", "anni_year", "vax_mech", "eff_mech", "from_age"),
    measure.vars = c("ccases.av","cdeaths.av")
), id + anni_year + vax_mech + eff_mech + from_age + variable ~ qtile)

#fig2.dt[, measure := factor(variable, levels = c("ccases.av","cdeaths.av"), ordered = TRUE)]

refdate <- as.Date("2020-04-01")

fig2.p <- ggplot(fig2.dt) + aes(
    refdate + anni_year*365,
    color = vax_mech,
    fill = vax_mech,
    linetype = eff_mech,
    group = interaction(vax_mech, eff_mech)
) + facet_grid(
    variable ~ from_age, scales = "free_y",
    labeller = labeller(
        variable = c(cdeaths.av = "Deaths", ccases.av = "Cases"),
        from_age = c(`4`="15+",`14`="65+")
    ), switch = "y"
) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95, color = NULL), alpha = 0.2, show.legend = FALSE) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50, color = NULL), alpha = 0.35, show.legend = FALSE) +
    geom_line(aes(y=md)) +
    scale_color_discrete(
        "Vaccine Against...",
        labels = c(infection = "Infection", disease="Disease"),
        guide = "legend", aesthetics = c("color","fill")
    ) +
    scale_linetype_discrete(
        "Efficacy Mechanism",
        labels = c(allornothing = "All-or-Nothing", leaky="Leaky")
    ) +
    scale_x_date(
        "Calendar Year",
        breaks = refdate + (1:10)*365,
        date_labels = "'%y"
    ) +
    scale_y_continuous("Cumulative Incidence Averted", labels = scales::label_number_si()) +
    theme_minimal() +
    theme(
        text = element_text(size = 12), 
        legend.position = "top",
        strip.placement = "outside",
        panel.grid.minor = element_blank(),
        panel.border=element_rect(colour = "black", fill=NA, size=0.5)
    )

ggsave(tail(.args, 1), fig2.p, width = 10, height = 6, units = "in", dpi = 600)

# explorer <- function(
#     pdt = full.dt,
#     filt = expression(
#         qtile == "md" & strategy != "none" & start_timing == 18718 &
#         doses_per_day == 8000 & vax_delay == 30 & vax_eff = 0.7 & nat_imm_dur_days == 5*365
#     )
# ) ggplot(pdt[eval(filt)]) + aes(
#     anni_year, cdeaths.av,
#     color = vax_eff,
#     linetype = factor(from_age),
#     size = factor(doses_per_day),
#     alpha = factor(vax_delay),
#     group = interaction(doses_per_day, from_age, vax_delay, vax_eff)
# ) + facet_nested(
#     start_timing + nat_imm_dur_days ~
#         strategy_str + vax_imm_dur_days,
#     labeller = labeller(
#         start_timing = function(st) sprintf(
#             c("Start Date = %s", rep("%s", length(st)-1)),
#             format(as.Date(as.integer(st), origin = "1970-01-01"), "%Y-%b-%d")
#         ),
#         nat_imm_dur_days = function(d) sprintf(
#             c("Nat. Imm. Dur\n=%s",rep("\n%s", length(d)-1)),
#             as.character(round(as.numeric(d)/365, 1))
#         ),
#         vax_imm_dur_days = function(d) sprintf(
#             c("Vax. Imm. Dur\n=%s",rep("\n%s", length(d)-1)),
#             as.character(round(as.numeric(d)/365, 1))
#         ),
#         strategy_str = function(d) sprintf(
#             c("Years of Vaccination = %s",rep("%s", length(d)-1)),
#             as.character(round(as.numeric(d)/365, 1))
#         )
#     )
# ) + geom_line() +
#     scale_color_continuous(
#         "Efficacy",
#         guide = "legend", breaks = seq(0.3, 0.9, by=0.2)
#     ) +
#     scale_size_discrete(
#         "Initial Doses Per Day",
#         range = c(1,3), guide = "legend"
#     ) +
#     scale_alpha_discrete(
#         "Doses for\nEfficacy",
#         labels = c("30"="Two", "0"="One"),
#         range = c(0.5, 1), guide = "legend"
#     ) +
#     scale_linetype_discrete(
#         "Q1 vax. age",
#        labels = c("15+", "65+")
#     ) +
#     scale_x_continuous("Years Since Initial Vaccination", breaks = 1:10) +
#     scale_y_continuous("Cumulative Deaths Averted", labels = scales::label_number_si()) +
#     coord_cartesian(expand = FALSE) +
#     theme_minimal() +
#     theme(
#         legend.position = "bottom"
#     )
# 
# overview <- explorer()
# ggsave(sprintf("%s/figures/epi_overview.png", .debug), overview, width = 12, height = 5, units = "in", dpi = 150)
# 
