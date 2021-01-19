suppressPackageStartupMessages({
    require(data.table)
    require(qs)
    require(ggplot2)
    require(lubridate)
    require(patchwork)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "fitd_combined.qs", "sindh_data.csv",
    "../covidm-vaxco",
    "%s/sim_model.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

# set up covidm
cm_path = tail(.args, 2)[1];
cm_force_rebuild = F;
cm_build_verbose = T;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

comb.fits <- qread(.args[1])
samples <- 250

all.dyn <- rbindlist(mapply(function(ft, nm) {
    ft$par$deterministic <- FALSE
    ft$par$time1 <- as.Date("2021-12-31")
    
    dyn <- rbindlist(
        cm_backend_sample_fit_test(cm_translate_parameters(ft$par), ft$post, samples, seed = 0)
    )[, .(true_deaths = sum(death_o), true_cases = sum(cases), totR = sum(R),
          rep_deaths = obs0[group == 2], rep_cases = obs0[group == 3]), by = .(run, t) ]
    
    melt(dyn[order(run, t)], id.vars = c("run","t"), variable.name = "outcome")[, scenario := nm ]
}, ft = comb.fits, nm = names(comb.fits), SIMPLIFY = FALSE))

#' TODO could get fancy with tstrsplit?
all.dyn[outcome %like% "^true",   epi := "true"];
all.dyn[outcome %like% "^rep",    epi := "reported"];
all.dyn[outcome %like% "cases$",  ind := "cases"];
all.dyn[outcome %like% "deaths$", ind := "deaths"];

all.dyn[, date := ymd("2020-01-01") + t ];

saveRDS(all.dyn, tail(.args, 1))
