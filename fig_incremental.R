suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/scenarios.rds", "%s/outputs/low/quantiles.rds",
    "%s/outputs/low/baseline.rds",
    "%s/figures/low/incremental.png"
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

plot.dt[, campaign := factor(fifelse(
  campaign_dur_days == 0, "Ongoing",
  sprintf("%i-day programme", campaign_dur_days)
), levels = c(sprintf("%i-day programme", c(90, 365)), "Ongoing"), ordered = TRUE)]

filt <- expression(age == "all" & outcome == meas & (anniversary != 0))

p.del <- function(meas = "cases", lbl = "Cases", showX = TRUE, high = "#56B1F7") ggplot(plot.dt[
    eval(filt) & start_date == "2021-01-01" & (vax_eff %in% c(0.3, 0.7))
]) +
    facet_nested(
      tar_age ~ campaign,
      labeller = labeller(
        tar_age = function(v) sprintf("vaccinate %s", v)
      )) +
    aes(
        anniversary, color = vax_eff,
        linetype = factor(vax_imm_dur_yrs),
        group = interaction(vax_eff, vax_imm_dur_yrs)
    ) +
    geom_line(aes(y=inc.md)) +
    geom_line(
        aes(y=inc.md, color = 0, linetype = NULL, group = NULL),
        data = bq.dt[eval(filt) & scenarioId == 1]
    ) +
    theme_minimal() +
    scale_color_continuous(
        "Vaccine Efficacy",
        breaks = c(0, c(30,70)/100),
        labels = c("No Vaccine", "30%", "70%"),
        guide = "legend",
        high = high
    ) +
    scale_linetype_manual(
        "Vaccine Expected\nDuration (yrs)",
        values = c(`2.5`="dotted", `5`="solid")
    ) + scale_x_continuous(
        if (showX) "Years since programme start" else NULL,
        breaks = 0:10, guide = if (showX) "axis" else "none"
    ) + scale_y_continuous(
        sprintf("Annual %s Incidence", lbl), labels = scales::label_number_si()
    ) + theme(
      strip.text.x = if (!showX) element_text() else element_blank()
    ) +
    coord_cartesian(ylim = c(0, NA))

outp <- (
  p.del(showX=FALSE) / p.del("death_o", "Deaths", high = "#f76956")
) + plot_layout(guides = "collect")

ggsave(tarfile, outp, width = 7.5, height = 5, units = "in")
