
suppressPackageStartupMessages({
    require(data.table)
    require(RSQLite)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/epi_quantile",
    "covid_other_costs.csv",
    "covid_vac_costs_per_dose.csv",
    "daly_scenarios.csv",
    "%s/config",
    "%s/econ_quantile_ext.rds"
),.debug) else commandArgs(trailingOnly = TRUE)

readDBtable <- function(fl, tbl = "metrics", drv = RSQLite::SQLite(), flags = SQLITE_RO) {
    conn <- dbConnect(drv, fl, flags = flags)
    res <- data.table(dbReadTable(conn, tbl))
    dbDisconnect(conn)
    res
}

dbs <- list.files(dirname(.args[5]), basename(.args[5]), full.names = TRUE)

epi_scn <- rbindlist(lapply(
  dbs, function(db) readDBtable(db, tbl = "scenario")[, .(id, vax_delay, strategy_str, doses_per_day)])
)

qs <- list.files(dirname(.args[1]), basename(.args[1]), full.names = TRUE)

epi_qs.dt <- rbindlist(lapply(qs, readRDS))[epi_scn, on=.(id)][!is.na(qtile)]

#' vector of doses 
doses_yr_1 <- sum(c(1,4,6,8))*365/4 # year 1
doses_routine <- 8*365
doses_per_anniversary <- c(doses_yr_1, rep(doses_routine, 9)) 

# year > 1 


# if vax_delay == 30, that's two dose per course, so doses = doses * 2

# by perspective
othercosts <- dcast(fread(.args[2]), perspective ~ name, value.var = "cost")

vac_cost.dt <- fread(.args[3])[scenario == "campaign"]
dalys.dt <- fread(.args[4])
dalys.dt[, age := age_cat ]
dalys.dt$age_cat <- NULL

#' econ scenarios: 3x perspective, 3x vac costs

econscns.dt <- data.table(expand.grid(
    perspective = othercosts[, unique(perspective)],
    vac_price = vac_cost.dt[, unique(vac_price)],
    daly_scenario = dalys.dt[, unique(daly_scenario)],
    disc.costs = dalys.dt[, max(disc_rate)],
    disc.dalys = dalys.dt[, unique(disc_rate)]
))[, econ_id := 1:.N ]

econ_digestor <- function(epi.dt, dalys.dt, econ_pars){
    
    # divide one-off annual/daily costs across age categories
    age_cats <- epi.dt[, max(age)]
    econ_pars[["cost_hs_day_erm"]] <- econ_pars[["cost_hs_day_erm"]] / age_cats
    econ_pars[["cost_hs_one_erm"]] <- econ_pars[["cost_hs_one_erm"]] / age_cats
    econ_pars[["cost_hs_day_comms"]] <- econ_pars[["cost_hs_day_comms"]] / age_cats
    
    add_costs <- function(dt) dt[, costs := fifelse(view == "incremental",-1,1)*with(econ_pars,
        # one-off / daily health system response costs
        365 * (cost_hs_day_erm + cost_hs_day_comms) + 
        cost_hs_one_erm +
        # 10 % of symptomatic cases tested, 7 contacts per tested case
        # TODO: how do these assumptions scale with prevalence
        cases * 0.1 * 7 * (cost_hs_per_traced + cost_hs_per_quarantined) +
        # testing costs, 11.31 tests per hospitalised case
        (non_icu_severe_i + non_icu_critical_i) * 11.31 * cost_hs_per_test +
        # testing of 10% of non-hospitalised cases
        (cases - (non_icu_severe_i + non_icu_critical_i)) * 
        0.1 * 11.31 * cost_hs_per_test +
        # daily cost of treatment on general ward
        (non_icu_severe_p + non_icu_critical_p) * cost_hs_day_treat_general +
        # daily cost of critical care
        icu_critical_p * cost_hs_day_treat_critical +
        # one-off cost of treating 10% of non-hospitalised cases at home
        (cases - (non_icu_severe_i + non_icu_critical_i)) * 
        0.1 * cost_hs_treat_home +
        # cost of death to the health system
        death_o * cost_hs_per_death +
        # household: cost of death
        death_o * (cost_hh_death_funeral + cost_hh_death_income) +
        # household: medical + non-med costs while in general ward
        (non_icu_severe_p + non_icu_critical_p) *
        (cost_hh_treat_general_med_per_day + cost_hh_treat_general_non_med_per_day) +
        # household: medical + non-med costs while in icu
        icu_critical_p * (cost_hh_treat_critical_med_per_day + cost_hh_treat_critical_non_med_per_day) +
        # household: medical + non-med costs for non-hospitalised cases
        # TODO: update with prevalent cases or alternative assumption about
        # treatment duration at home
        (cases - (non_icu_severe_i + non_icu_critical_i)) * 
        7 * (cost_hh_treat_home_med_per_day + cost_hh_treat_home_non_med_per_day) +
        # household: individual and caregiver lost income
        cases * (cost_hh_individual_income_per_case + cost_hh_caregiver_income_per_case)
    )][,
      costs := with(econ_pars, costs + fifelse(
          is.na(vax_delay),
          0,
          doses_per_day*doses_per_anniversary[anni_year]/age_cats *
              ((strategy_str == 0) | (anni_year <= (strategy_str/365))) *
              cost_vac_dose * fifelse(vax_delay == 0, 1, 2)
      ))
    ][,
      costs := with(econ_pars, (1/(1 + disc.costs)^(anni_year - 1)) * costs)
    ]
    
    add_costs(epi.dt) # gives absolute costs
    
    add_dalys <- function(dt) dt[
        dalys.dt,
        dalys := with(econ_pars,
                      death_o * dalys_death + # dalys per death
                          cases * dalys_case + # dalys per case
                          # dalys per hospitalised case in general ward
                          non_icu_severe_i * dalys_hospital +
                          # dalys per icu admissions that survive
                          (icu_critical_i - death_o) * dalys_icu
        ),
        on=.(age)
    ][, #' TODO revisit approach?
      dalys := with(econ_pars, (1/(1 + disc.dalys)^(anni_year-1)) * dalys)
    ]
    
    add_dalys(epi.dt)
    
    agg.dt <- epi.dt[,
        .(costs = sum(costs), dalys = sum(dalys)),
        by=.(qtile, id, anni_year, view)
    ]
    
    agg.dt[
        order(anni_year),
        c("ccosts", "cdalys") := .(cumsum(costs), cumsum(dalys)),
        by=.(qtile, id, view)
    ]
    
    agg.dt[, icer := NA_real_ ]
    agg.dt[view == "incremental", icer := ccosts / cdalys ]
    
    return(agg.dt)
}

incr.dt <- epi_qs.dt[!is.na(cases.del), .SD, .SDcols = c("id","age","qtile","anni_year", grep("\\.del", names(epi_qs.dt), value = TRUE), "vax_delay","strategy_str","doses_per_day")]
names(incr.dt) <- grep("\\.del", names(epi_qs.dt), invert = TRUE, value = TRUE)

both.dt <- rbind(
    incr.dt[, view := "incremental"],
    epi_qs.dt[,.SD,.SDcols = -grep("\\.del", names(epi_qs.dt))][, view := "raw" ]
)

ret.dt <- rbindlist(lapply(1:nrow(econscns.dt), function(i) {
    es <- as.list(econscns.dt[i])
    econ_pars <- c(
        as.list(othercosts[perspective == es$perspective]),
        as.list(vac_cost.dt[vac_price == es$vac_price, .(cost_vac_dose)]),
        es[c("disc.costs","disc.dalys")]
    )
    res <- econ_digestor(both.dt, dalys.dt[daly_scenario == es$daly_scenario & disc_rate == es$disc.dalys], econ_pars)
    res[, econ_id := es$econ_id ]
}))

saveRDS(ret.dt, tail(.args, 1))