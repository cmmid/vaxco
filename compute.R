
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

# Scenario parameters
# TODO what this does not cover yet: any vaccines being disbursed
# (how many doses to which age groups, when)
drv <- RSQLite::SQLite()
conn <- dbConnect(drv, scndb)
scen.dt <- as.list(data.table(dbReadTable(conn, "scenario"))[id == scnid])
scen.dt$n_samples <- dbGetQuery(conn, "SELECT max(particleId) FROM sample;")[,1]
#' TODO pull from pars table?
#' also, need to set selections from covidm sampling
scen.dt$rng_seed <- 0
dbDisconnect(conn)

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

date_vax <- as.Date(scen.dt$start_timing, origin = "1970-01-01")
t_vax <- as.numeric(date_vax - as.Date(fitS$par$date0))
#' first 3 years + vax anniversaries
record_times <- unique(c(1:(365*3), seq(t_vax+365, by=365, length.out = 10)))

mk_waning <- function(baseline_dur_days, ages = 16, age_dur_mods = rep(1, ages) ) {
    rep(
        ifelse(baseline_dur_days == 0, 0, 365/baseline_dur_days),
        ages
    ) / age_dur_mods
}

fitS$par$time1 = t_vax + 10*365 # 10 years of anniversaries
fitS$par$pop[[1]]$wn = mk_waning(scen.dt$nat_imm_dur_days)

if (scen.dt$strategy == "campaign") {
    # set parameters for this set of scenario runs
    fitS$par$pop[[1]]$ev = scen.dt$vax_eff
    fitS$par$pop[[1]]$wv = mk_waning(scen.dt$vax_imm_dur_days)
    
    doses_per_day <- rep(0, 16)
    tar_ages <- scen.dt$from_age:scen.dt$to_age
    doses_per_day[tar_ages] <- round(scen.dt$doses_per_day/length(tar_ages))
    del <- scen.dt$doses_per_day - sum(doses_per_day)
    if (del) {
        del_tar <- scen.dt$from_age:(scen.dt$from_age+del-1)
        doses_per_day[del_tar] <- doses_per_day[del_tar] + 1
    }
    
    ### NEW BIT IS HERE
    fitS$par$schedule[[2]] = list(   # schedule[[2]] because schedule[[1]] is already occupied
        parameter = 'v',             # impact parameter 'v' (vaccines administered per day for each age group)
        pops = 0,                    # 0th population
        mode = 'assign',             # assign values to v
        times =     c(t_vax,           t_vax+scen.dt$strategy_str),    # do changes on vax day, vax day + 90
        values = list(doses_per_day, rep(0, 16))    # 16000 vaccines a day for 180 days, then 32000 a day for 180 days, then no more vaccines
    )
    
}

# sample from posterior to generate runs
runs = cm_backend_sample_fit_test(
    cm_translate_parameters(fitS$par),
    fitS$post, scen.dt$n_samples, seed = scen.dt$rng_seed
)

all = rbindlist(lapply(runs, function (ru) {
    ru[order(t),
       { ret <- lapply(
           .SD[,.SD,.SDcols=-c("S","E","Ip","Is","Ia","R","t","population")],
           cumsum
       ); c(list(t=t), ret) },
       keyby=.(particleId = run, group)
    ][ t %in% record_times ]
}))

long.dt <- melt(all, id.vars = c("particleId","group","t"))
long.dt[, name := sprintf("%s_age%i_t%i", variable, group, t) ]

write.dt <- long.dt[,.(id = scnid, sampleId = particleId, name, value_numeric = value)]

conn <- dbConnect(drv, scndb)
dbWriteTable(conn, "metrics", write.dt, append = TRUE)
dbDisconnect(conn)
