
suppressPackageStartupMessages({
    require(data.table)
    require(qs)
    require(RSQLite)
})

.args <- if (interactive()) c(
    "fit_sindh.qs", "epi_data.csv", "mob_data.csv",
    "results.sqlite", "5", "../covidm"
) else commandArgs(trailingOnly = TRUE)

# load fitted model for Sindh
fitS = qread(.args[1])

# load epi & mobility data
epi = fread(.args[2])
mob = fread(.args[3])

# TODO fish out scenario
scndb <- .args[4]
scnid <- as.integer(.args[5])

drv <- RSQLite::SQLite()
conn <- dbConnect(drv, scndb)
scen.dt <- data.table(dbReadTable(conn, "scenario"))[id == scnid]
#' TODO pull from pars table
n_samples <- 10
dbDisconnect(conn)

# Scenario parameters
# TODO what this does not cover yet: any vaccines being disbursed
# (how many doses to which age groups, when)
p = list(
    n_samples = n_samples,
    # number of samples from posterior
    rng_seed = 0,
    # RNG seed; 0 means default for RNG
    vax_eff = rep(scen.dt$vaxEff, 16),
    # probability of vaccine giving protection until waning (by age group)
    dur_nat = scen.dt$immuneDurDays,
    # duration in dats of natural immunity (by age group)
    dur_vax = scen.dt$immuneDurDays
    # duration in days of vaccine immunity (by age group)
)


cm_path <- tail(.args, 1)

# load covidm
cm_force_rebuild = F;
cm_build_verbose = T;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

cm_source_backend(user_defined = fitS$user_defined)
# This will recompile covidm with certain components required to run 
# for this setting and model setup -- this step may need to be done 
# prior to batch execution of runs to avoid multiple threads trying to 
# recompile covidm at once.

# set parameters for this set of scenario runs
fitS$par$pop[[1]]$ev = p$vax_eff
fitS$par$pop[[1]]$wn = ifelse(p$dur_nat == 0, 0, 365/p$dur_nat)
fitS$par$pop[[1]]$wv = ifelse(p$dur_vax == 0, 0, 365/p$dur_vax)
fitS$par$time1 = 730

# sample from posterior to generate runs
run = cm_backend_sample_fit_test(
    cm_translate_parameters(fitS$par),
    fitS$post, p$n_samples, seed = p$rng_seed
);
run = rbindlist(run)

# TODO