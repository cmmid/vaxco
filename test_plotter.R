
res <- rbindlist(lapply(list.files("test", full.names = T), function(p) readRDS(p)[, id := as.integer(gsub("^.+/(\\d+)\\.rds$","\\1",p))]))
scn <- readRDS("~/Dropbox/Covid-WHO-vax/inputs/config.rds")[id %in% res$id]
plot.dt <- res[scn, on=.(id)][, .(sampleId, group, t, outcome, value, id, vax_mech, eff_mech, from_age)]
base.dt <- plot.dt[is.na(vax_mech), .SD, .SDcols = -c("vax_mech", "eff_mech", "from_age", "id")]
# dcp <- copy(base.dt[outcome == "death_o"])
# dcp[, outcome := "death_o.1"][, value := 0.0 ]
# base.dt <- rbind(base.dt, dcp)

res.dt <- plot.dt[!is.na(vax_mech)][base.dt, on=.(sampleId, group, t, outcome)][, .(
    value = sum(value), i.value = sum(i.value), averted = sum(i.value-value)
), by=c(setdiff(names(plot.dt), c("value","group")))]
res.dt[order(t), c("cv","cv.i","ca") := .(cumsum(value), cumsum(i.value), cumsum(averted)), by=.(sampleId, id, outcome)]

require(ggplot2)

ggplot(res.dt[t > 800 & !(outcome %in% c("E","Sv","Ev"))]) +
    aes(t, ca, color = vax_mech, linetype = eff_mech, group = interaction(vax_mech, eff_mech, sampleId, outcome, from_age)) + 
    facet_grid(outcome ~ from_age, scales = "free_y") +
    geom_line(alpha = 0.2) +
    theme_minimal() +
    scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_linetype_discrete(guide = guide_legend(override.aes = list(alpha = 1))) +
    scale_y_log10() + coord_cartesian(expand = F)
