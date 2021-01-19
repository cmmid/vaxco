suppressPackageStartupMessages({
    require(data.table)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/sim_model.rds", "sindh_data.csv",
    "%s/figures/model_fit.png"
), .debug)

dt <- readRDS(.args[1])
dt[is.na(ind), epi := "model"]
dt[is.na(ind), ind := "serology"]

cat("Model total indicators...\n")

cat(dt[t <= 258 & epi == "true",
    .(tot_inc = sum(value)),
   by = .(run, scenario, ind)
][,{
    qs <- quantile(tot_inc, probs = c(0.5, 0.025, 0.975))
    names(qs) <- c("md","lo","hi")
    as.list(qs)
}, by = .(scenario, ind)][, sprintf("%s: %s %0.2g (%0.2g, %0.2g)\n", scenario, ind, md, lo, hi)])

cat("Model ascertainment...\n")

cat(dt[t <= 258 & ind %in% c("cases","deaths"),
  .(tot_asc = sum(value[epi == "reported"])/sum(value[epi == "true"])),
  by = .(run, scenario, ind)
][,{
    qs <- quantile(tot_asc, probs = c(0.5, 0.025, 0.975))
    names(qs) <- c("md","lo","hi")
    as.list(qs)
}, by = .(scenario, ind)][, sprintf("%s: %s %0.2g%% (%0.2g-%0.2g%%)\n", scenario, ind, md*100, lo*100, hi*100)])
