
suppressPackageStartupMessages({
    require(data.table)
    require(qs)
    require(RSQLite)
})

.debug <- "1537"
.args <- if (interactive()) sprintf(c(
    "fitd_combined.qs", "epi_data.csv", "mob_data.csv",
    "inputs/config.sqlite", "%s", "../covidm-vaxco", "outputs/%s.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

# load epi & mobility data
epi = fread(.args[2])
mob = fread(.args[3])

# TODO fish out scenario
scndb <- .args[4]

scnid <- as.integer(tail(.args, 3)[1])
cm_path <- tail(.args, 2)[1]
outfile <- tail(.args, 1)

# Scenario parameters
# TODO what this does not cover yet: any vaccines being disbursed
# (how many doses to which age groups, when)
drv <- RSQLite::SQLite()
conn <- dbConnect(drv, scndb)
scen.dt <- as.list(data.table(dbReadTable(conn, "scenario"))[id == scnid])
scen.dt$n_samples <- dbGetQuery(conn, "SELECT max(particleId) FROM parameter;")[,1]
#' @example 
#' scen.dt$n_samples <- 5
#' TODO pull from pars table?
#' also, need to set selections from covidm sampling
dbDisconnect(conn)

natwaning_key <- sprintf("%.1f", scen.dt$nat_imm_dur_days/365)

# load fitted model for Sindh, match to scenario waning assumption
fitS = qread(.args[1])[[natwaning_key]]

# load covidm
cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

date_vax <- as.Date(scen.dt$start_timing, origin = "1970-01-01")
t_vax <- as.numeric(date_vax - as.Date(fitS$par$date0))
#' first 3 years + vax anniversaries
# validation_times <- seq(7, 365*3, by=7)
anni_times <- seq(t_vax, by=365, length.out = scen.dt$horizon+1) #' horizon years + 1
record_times <- anni_times #unique(c(validation_times, anni_times))

mk_waning <- function(baseline_dur_days, ages = 16, age_dur_mods = rep(1, ages) ) {
    rep(
        ifelse(baseline_dur_days == 0, 0, 1/baseline_dur_days),
        ages
    ) / age_dur_mods
}

fitS$par$time1 = t_vax + max(record_times) # 10 years of anniversaries
# TODO need fitting w/ the different immunity waning?
fitS$par$pop[[1]]$wn = mk_waning(scen.dt$nat_imm_dur_days)

if (scen.dt$strategy == "campaign") {
    # set parameters for this set of scenario runs
    fitS$par$pop[[1]]$ev = rep(scen.dt$vax_eff, 16) #' TODO mods by age?
    fitS$par$pop[[1]]$wv = mk_waning(scen.dt$vax_imm_dur_days)
    
    doses_per_day <- rep(0, 16)
    tar_ages <- scen.dt$from_age:scen.dt$to_age
    vp <- fitS$par$pop[[1]]$size[tar_ages]; vp <- vp/sum(vp)
    #' TODO potentially make demographic sensitive?
    doses_per_day[tar_ages] <- floor(vp*scen.dt$doses_per_day)
    del <- scen.dt$doses_per_day - sum(doses_per_day)
    if (del) {
        del_tar <- scen.dt$from_age:(scen.dt$from_age+del-1)
        doses_per_day[del_tar] <- doses_per_day[del_tar] + 1
    }
    
    doses_per_day_later <- rep(0, 16)
    tar_ages <- 4:scen.dt$to_age
    vp <- fitS$par$pop[[1]]$size[tar_ages]; vp <- vp/sum(vp)
    #' TODO potentially make demographic sensitive?
    doses_per_day_later[tar_ages] <- floor(vp*scen.dt$doses_per_day)
    del <- scen.dt$doses_per_day - sum(doses_per_day_later)
    if (del) {
      del_tar <- 4:(4+del-1)
      doses_per_day_later[del_tar] <- doses_per_day_later[del_tar] + 1
    }
    
    t_end <- ifelse(scen.dt$strategy_str == 0, fitS$par$time1, t_vax+scen.dt$strategy_str)
    
    covaxIncreaseMult <- c(4, 6, 8)
    covaxIncreaseDays <- t_vax + seq(91, by=91, length.out = length(covaxIncreaseMult))
    
    dpd <- lapply(
      1:(length(covaxIncreaseMult)+2),
      function (i) c(1, covaxIncreaseMult, 0)[i]*(if (i==1) doses_per_day else doses_per_day_later)
    )
    
    ### NEW BIT IS HERE
    fitS$par$schedule[[2]] = list(   # schedule[[2]] because schedule[[1]] is already occupied
        parameter = 'v',             # impact parameter 'v' (vaccines administered per day for each age group)
        pops = 0,                    # 0th population
        mode = 'assign',             # assign values to v
        times =     c(t_vax, covaxIncreaseDays, t_end) + scen.dt$vax_delay,    # do changes on vax day, vax day + 90
        values = dpd
        # however many doses a day for strategy_str days, then stop
    )
    
}

#' TODO check coding?

keepoutcomes <- c(
  "cases", "death_o",
  "non_icu_severe_i", "non_icu_critical_i", "icu_critical_i",
  "non_icu_severe_p", "non_icu_critical_p", "icu_critical_p"
)

# sample from posterior to generate runs
all_runs = rbindlist(lapply(1:scen.dt$n_samples, function (n) {
  runs = cm_backend_sample_fit_test(
    cm_translate_parameters(fitS$par),
    fitS$post, 1, seed = n
  )[[1]][order(t), {
    ret <- lapply(
      .SD[,.SD,.SDcols=keepoutcomes],
      cumsum
    )
    c(list(t=t, sampleId = n), ret)
  }, keyby=.(age = group)
  ][ t %in% record_times ]
  #[ t < record_times[2] ]
  #
}))

# this version out-of-memories; possible to reduce mem by reducing times out?
# runs = cm_backend_sample_fit_test(
#     cm_translate_parameters(fitS$par),
#     fitS$post, 1, seed = scen.dt$rng_seed
# )

#' @examples 
#' require(ggplot2)
#' plot.dt <- melt(runs[[5]], id.vars = c("run","t","population","group"))
#' p <- ggplot(plot.dt) + aes(t, value, color=group, group=group) +
#'   geom_line(data=function(dt) dt[variable == "cases"]) + theme_minimal() +
#'   coord_cartesian(xlim = c(0, 356)) + geom_vline(xintercept = t_vax)

long.dt <- melt.data.table(
  all_runs, id.vars = c("sampleId","age","t"), variable.name = "outcome"
)

long.dt[, anni_year := (t %/% 365) - 1 ]
long.dt$t <- NULL

saveRDS(long.dt, tail(.args, 1))