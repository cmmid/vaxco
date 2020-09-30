suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(ggh4x)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/scenarios.rds", "%s/outputs/low/validation.rds",
    "%s/figures/low/validation.png"
), .debug) else commandArgs(trailingOnly = TRUE)

scn.dt <- rbind(
  readRDS(.args[1]),
  data.table(id=1, start_date=as.Date(c("2021-01-01"))),
  fill = TRUE
)

qs.dt <- readRDS(.args[2])
tarfile <- tail(.args, 1)

plot.dt <- qs.dt[scn.dt, on=.(scenarioId == id)]

plot.dt[order(simday), week := simday/7, by = scenarioId ]

p.del <- function(meas = "cases", lbl = "Cases") {}
  
lbl <- "Cases"

mainp <- plot.dt
basep <- plot.dt[
  age == "all" & is.na(tar_age) & (start_date == "2021-01-01"),
  .(week, inc.md, outcome, vax_eff = 0, tar_age="15+")
]
basep <- rbind(
  basep[, tar_age := "65+" ], copy(basep)[, tar_age := "15+" ]
)

p <- ggplot(plot.dt[age == "all" & !is.na(tar_age) & start_date == "2021-01-01"]) +
  facet_nested(
    outcome + tar_age ~ campaign_dur_days + vax_imm_dur_yrs,
    scales = "free_y"
  ) +
  aes(week, color = vax_eff, group = vax_eff) +
  geom_line(aes(y=inc.md)) +
  geom_line(aes(y=inc.md), basep) +
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
      "Weeks since 1 JAN 2020"
  ) + scale_y_continuous(
      "Incidence", labels = scales::label_number_si()
  )

ggsave(tarfile, p, width = 6, height = 5, units = "in")