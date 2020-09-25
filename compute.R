
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
cm_build_verbose = F;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

date_vax <- as.Date(scen.dt$start_timing, origin = "1970-01-01")
t_vax <- as.numeric(date_vax - as.Date(fitS$par$date0))
#' first 3 years + vax anniversaries
validation_times <- 1:(365*3)
anni_times <- seq(t_vax+365, by=365, length.out = 10)
record_times <- unique(c(validation_times, anni_times))

mk_waning <- function(baseline_dur_days, ages = 16, age_dur_mods = rep(1, ages) ) {
    rep(
        ifelse(baseline_dur_days == 0, 0, 1/baseline_dur_days),
        ages
    ) / age_dur_mods
}

fitS$par$time1 = t_vax + 10*365 # 10 years of anniversaries
fitS$par$pop[[1]]$wn = mk_waning(scen.dt$nat_imm_dur_days)

if (scen.dt$strategy == "campaign") {
    # set parameters for this set of scenario runs
    fitS$par$pop[[1]]$ev = rep(scen.dt$vax_eff, 16) #' TODO mods by age?
    fitS$par$pop[[1]]$wv = mk_waning(scen.dt$vax_imm_dur_days)
    
    doses_per_day <- rep(0, 16)
    tar_ages <- scen.dt$from_age:scen.dt$to_age
    #' TODO potentially make demographic sensitive?
    doses_per_day[tar_ages] <- floor(scen.dt$doses_per_day/length(tar_ages))
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
        values = list(doses_per_day, rep(0, 16))
        # however many doses a day for strategy_str days, then stop
    )
    
}

#' TODO check coding?

# sample from posterior to generate runs
runs = cm_backend_sample_fit_test(
    cm_translate_parameters(fitS$par),
    fitS$post, scen.dt$n_samples, seed = scen.dt$rng_seed
)

#' @examples 
#' require(ggplot2)
#' plot.dt <- melt(runs[[5]], id.vars = c("run","t","population","group"))
#' p <- ggplot(plot.dt) + aes(t, value, color=group, group=group) +
#'   geom_line(data=function(dt) dt[variable == "cases"]) + theme_minimal() +
#'   coord_cartesian(xlim = c(0, 356)) + geom_vline(xintercept = t_vax)

all = rbindlist(lapply(runs, function (ru) {
    ru[order(t), {
      ret <- lapply(
        .SD[,.SD,.SDcols=-c("S","E","Ip","Is","Ia","R","t","population")],
        cumsum
      )
      c(list(t=t), ret)
    }, keyby=.(sampleId = run, age = group)
    ][ t %in% record_times ]
}))

long.dt <- melt.data.table(
  all, id.vars = c("sampleId","age","t"), variable.name = "outcome"
)

write.dt <- long.dt[,.(
  scenaroId = scnid,
  sampleId,
  age,
  simday = t,
  outcome,
  value_numeric = value
)]

max_retries <- 200
retries <- 1:max_retries

for (retry in retries) {
  
  skip_to_next <- FALSE
  
  tryCatch ({
    
    conn <- dbConnect(drv, scndb)
    dbWriteTable(conn, "metrics", write.dt[simday %in% anni_times], append = TRUE)
    dbWriteTable(conn, "other", write.dt[simday %in% validation_times], append = TRUE)
    dbDisconnect(conn)
    
  }, error = function(err) {
    
    skip_to_next <<- TRUE
    
  })
  
  if (!skip_to_next) break

}

conn <- dbConnect(drv, scndb)
dbWriteTable(conn, "metrics", write.dt, append = TRUE)
dbDisconnect(conn)
