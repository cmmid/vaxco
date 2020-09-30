suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/scenarios.rds", "%s/outputs/quantiles.rds",
    "%s/figures/overview.png"
), .debug) else commandArgs(trailingOnly = TRUE)

scn.dt <- readRDS(.args[1])
qs.dt <- readRDS(.args[2])
tarfile <- tail(.args, 1)

plot.dt <- qs.dt[scn.dt, on=.(scenarioId == id)]

plot.dt[order(simday), anniversary := round((simday - simday[1])/356), by = scenarioId ]

p.del <- ggplot(plot.dt[outcome == "cases" & start_date == "2021-01-01"]) +
    facet_nested(tar_age + campaign_dur_days ~ vax_eff) +
    aes(
        anniversary, color = age,
        linetype = factor(vax_imm_dur_yrs),
        group = interaction(age, vax_imm_dur_yrs)
    ) +
    geom_line(aes(y=cum.averted.md)) +
    theme_minimal() +
    scale_color_continuous("Age Category",guide = "legend") +
    scale_linetype_manual(
        "Vaccine Expected\nDurability (yrs)",
        values = c(`2.5`="dotted", `5`="solid")
    ) + scale_x_continuous(
        "Years since initial vaccination",
        breaks = 0:10
    ) + scale_y_continuous(
        "Cumulative cases averted (median)"
    )

p.eff <- ggplot(plot.dt[outcome == "cases" & start_date == "2021-01-01"]) +
    facet_nested(tar_age + campaign_dur_days ~ vax_eff) +
    aes(
        anniversary, color = age,
        linetype = factor(vax_imm_dur_yrs),
        group = interaction(age, vax_imm_dur_yrs)
    ) +
    geom_line(aes(y=cum.eff.md)) +
    theme_minimal() +
    scale_color_continuous("Age Category",guide = "legend") +
    scale_linetype_manual(
        "Vaccine Expected\nDurability (yrs)",
        values = c(`2.5`="dotted", `5`="solid")
    ) + scale_x_continuous(
        "Years since initial vaccination",
        breaks = 0:10
    ) + scale_y_continuous(
        "Cumulative Effectiveness (cases, median)"
    )

