suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/scenarios_high.rds",
    "%s/outputs/econ_summary_inc.rds",
    "%s/figures/econ_summary.png"
), .debug) else commandArgs(trailingOnly = TRUE)

scn.dt <- readRDS(.args[1])
econ.dt <- readRDS(.args[2])[scn.dt[id %in% unique(scenarioId)], on = .(scenarioId == id)]

tarfile <- tail(.args, 1)

econ.dt[, campaign := factor(fifelse(
  campaign_dur_days == 0, "Ongoing",
  sprintf("%i-day programme", campaign_dur_days)
), levels = c(sprintf("%i-day programme", c(90, 365)), "Ongoing"), ordered = TRUE)]

p.del <- function(meas = "cost_total_disc", lbl = "Discounted Cost ($)", showX = TRUE, high = "#56B1F7") ggplot(econ.dt[
  outcome == meas &  (vax_eff %in% c(0.3, 0.7))
]) +
    facet_nested(
      tar_age + vac_price ~ campaign,
      labeller = labeller(
        tar_age = function(v) sprintf("vaccinate %s", v)
      )) +
    aes(
        anni_year, color = vax_eff,
        linetype = factor(vax_imm_dur_yrs),
        group = interaction(vax_eff, vax_imm_dur_yrs)
    ) +
    geom_line(aes(y=inc_val.md)) +
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
        "Years since programme start",
        breaks = 0:10, guide = if (showX) "axis" else "none"
    ) + scale_y_continuous(
        sprintf("Annual %s", lbl), labels = scales::label_number_si()
    )

outp <- p.del()

ggsave(tarfile, outp, width = 7.5, height = 5, units = "in")
