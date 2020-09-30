suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/scenarios.rds", "%s/outputs/quantiles.rds",
    "%s/outputs/baseline.rds",
    "%s/figures/incremental.png"
), .debug) else commandArgs(trailingOnly = TRUE)

scn.dt <- readRDS(.args[1])
qs.dt <- readRDS(.args[2])
bs.dt <- readRDS(.args[3])
tarfile <- tail(.args, 1)

bq.dt <- bs.dt[,{
  qs <- quantile(inc, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(qs) <- sprintf("inc.%s", c("lo95","lo50","md","hi50","hi95"))
  as.list(qs)
}, by=.(scenarioId, simday, outcome, age)]
bq.dt[order(simday), anniversary := round((simday - simday[1])/356), by = scenarioId ]

plot.dt <- qs.dt[scn.dt, on=.(scenarioId == id)]

plot.dt[order(simday), anniversary := round((simday - simday[1])/356), by = scenarioId ]

p.del <- function(meas = "cases", lbl = "Cases") ggplot(plot.dt[
    age == "all" & outcome == meas & start_date == "2021-01-01" & (anniversary != 0)
]) +
    facet_nested(tar_age ~ campaign_dur_days) +
    aes(
        anniversary, color = vax_eff,
        linetype = factor(vax_imm_dur_yrs),
        group = interaction(vax_eff, vax_imm_dur_yrs)
    ) +
    geom_line(aes(y=inc.md)) +
    geom_line(
        aes(y=inc.md, color = 0, linetype = NULL, group = NULL),
        data = bq.dt[age == "all" & outcome == meas & (anniversary != 0) & scenarioId == 2]
    ) +
    theme_minimal() +
    scale_color_continuous(
        "Vaccine Efficacy",
        breaks = c(0, seq(10,90,by=20)/100),
        guide = "legend"
    ) +
    scale_linetype_manual(
        "Vaccine Expected\nDurability (yrs)",
        values = c(`2.5`="dotted", `5`="solid")
    ) + scale_x_continuous(
        "Years since start of 1-time campaign",
        breaks = 0:10
    ) + scale_y_continuous(
        sprintf("Annual %s Incidence", lbl), labels = scales::label_number_si()
    )

outp <- (p.del() / p.del("death_o", "Deaths")) + plot_layout(guides = "collect")

ggsave(tarfile, outp, width = 6, height = 5, units = "in")