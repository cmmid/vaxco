library(data.table)
library(ggplot2)
library(lubridate)
library(qs)

# Scenario parameters
# TODO what this does not cover yet: any vaccines being disbursed (how many doses to which age groups, when)
p = list(
    n_samples = 50,         # number of samples from posterior
    rng_seed = 0,           # RNG seed; 0 means default for RNG
    vax_eff = rep(0.8, 16), # probability of vaccine giving protection until waning (by age group)
    dur_nat = rep(180000, 16), # duration in dats of natural immunity (by age group)
    dur_vax = rep(180000, 16)  # duration in days of vaccine immunity (by age group)
)

# load covidm
cm_path = "~/Dropbox/nCoV/covidm/";
cm_force_rebuild = F;
cm_build_verbose = T;
cm_version = 2;
source(paste0(cm_path, "/R/covidm.R"))

# load fitted model for Sindh
fitS = qread("fit_sindh.qs")
cm_source_backend(user_defined = fitS$user_defined) # This will recompile covidm with certain components required to run 
                                                    # for this setting and model setup -- this step may need to be done 
                                                    # prior to batch execution of runs to avoid multiple threads trying to 
                                                    # recompile covidm at once.

# set parameters for this set of scenario runs
fitS$par$pop[[1]]$ev = p$vax_eff
fitS$par$pop[[1]]$wn = 365 / p$dur_nat
fitS$par$pop[[1]]$wv = 365 / p$dur_vax
fitS$par$time1 = 730

# sample from posterior to generate runs
run = cm_backend_sample_fit_test(cm_translate_parameters(fitS$par), fitS$post, p$n_samples, seed = p$rng_seed);
run = rbindlist(run)

# NOTE
# at this point, run contains the results of the model run and that's all that is needed for further analysis.
# the rest of the file is just for plotting and comparison

# load epi & mobility data
epi = fread("epi_data.csv")
mob = fread("mob_data.csv")

# plot reported deaths (model) against reported deaths (data)
# for stupid reasons, the model fitting process that was set up to fit to Sindh stores total simulated reported deaths in column obs0, group 2.
# "actual" total number of modelled deaths are stored in column death_o as would be expected (does not account for underascertainment of deaths)
ggplot() + 
    geom_line(data = run[group == 2, .(d = obs0), by = .(run, t)], aes(x = t, y = d, colour = run, group = run)) + # model
    geom_point(data = epi[location == "Sindh"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = rollmean(deaths, 7, fill = NA))) + # reported
    theme(legend.position = "none") + labs(x = "Time (days)", y = "Deaths")

# note modelled cases do not fit well with reported cases, even with a constant underascertainment rate
# possibly because ascertainment changed over time.
ggplot(run[, .(d = sum(cases) * 0.1), by = .(run, t)]) + 
    geom_point(aes(x = t, y = d, colour = run)) +
    geom_line(data = epi[location == "Sindh"], aes(x = as.numeric(ymd(date) - ymd("2020-01-01")), y = cases))