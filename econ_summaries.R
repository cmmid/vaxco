suppressPackageStartupMessages({
    require(RSQLite)
    require(data.table)
    require(readr)
})

.debug <- "~/Dropbox/Covid-WHO-vax"
.args <- if (interactive()) sprintf(c(
    "%s/inputs/daly_scenarios.csv",
    "%s/inputs/covid_vac_cost_inputs.csv",
    "%s/inputs/covid_other_cost_inputs.csv",
    "%s/inputs/config_high.sqlite",
    "%s/outputs/metrics_",
    "%s/outputs/dalys.rds",
    "%s/outputs/costs.rds"
), .debug) else commandArgs(trailingOnly = TRUE)

#' load DALY data, extract no. of age categories
dalys.dt <- fread(.args[1])
age_cats <- dalys.dt[, max(age_cat)]

#' load vaccine cost data; n.b., costs are total, need cost by age category
vac_costs.dt <- fread(.args[2])
#' so divide all the totals cols by no. of age categories
tot_cols <- grep("^tot", names(vac_costs.dt), value = T)
vac_costs.dt[, c(tot_cols) := .SD/age_cats, .SDcols = tot_cols ]

#' get other cost data
other_costs <- as.list(
    dcast(fread(.args[3])[, .(short_desc, cost)],. ~ short_desc, value.var = "cost")
)

#' read in scenario data
drv <- RSQLite::SQLite()
conn <- dbConnect(drv, dbname=.args[4], flags = SQLITE_RO)
epi_scen.dt  <- data.table(dbReadTable(conn,"scenario"))
dbDisconnect(conn)
#' select epi scenarios starting 01-Jan-21 = 18628
scen_ids <- epi_scen.dt[start_timing==18628,id]
ref_scen_id <- epi_scen.dt[start_timing==18628 & strategy == "none", id]

tar_outcomes <- c(
    'cases', 'death_o', 'non_icu_severe_i', 'non_icu_severe_p',
    'non_icu_critical_i', 'non_icu_critical_p', 'icu_critical_i', 'icu_critical_p'
)
outcome_set <- paste(sprintf("'%s'", tar_outcomes), collapse = ", ")
slct_stmt <- sprintf("SELECT * FROM metrics WHERE outcome IN (%s);", outcome_set)

epi_outcomes.dt <- rbindlist(lapply(scen_ids, function(scn, path, drv, slct_stmt) {
    conn <- dbConnect(drv, dbname=sprintf("%s%02d.sqlite", path, scn), flags = SQLITE_RO)
    res <- data.table(dbGetQuery(conn, slct_stmt))
    dbDisconnect(conn)
    res
}, path = .args[5], drv = drv, slct_stmt))

epi_outcomes.dt[, anni_year := as.integer((simday-min(simday))/365) ]
epi_outcomes.dt[
    order(anni_year),
    inc := c(value_numeric[1], diff(value_numeric)),
    by = .(scenarioId, sampleId, age, outcome)
]

#' convert to wide format
outcomes.dt <- dcast(
    epi_outcomes.dt[anni_year != 0], 
    scenarioId + sampleId + age + anni_year ~ outcome,
    value.var = "inc"
)

#' add non-vac costs
#' TODO magic numbers:
#'  - traces_per_case - also, shouldn't this be per detected case?
#'  - fraction_home_care - need to exclude various hosp outcomes before applying?
with(other_costs, outcomes.dt[,
    c("cost_ERM", "cost_comms", "cost_trace", "cost_test", "cost_treat", "cost_death") := list(
        0 * cost_day_erm + 0 * cost_one_erm, # to update
        0 * cost_day_comms,   # to update
        cases * 10 * cost_per_traced + 0 * cost_per_quarantined,
        # 10 traces per case; to update quarantine; CABP note: why no testing cost w/ tracing?
        (non_icu_severe_i + non_icu_critical_i) * cost_per_test,
        (non_icu_severe_p + non_icu_critical_p) * cost_day_treat_general +
            icu_critical_p * cost_day_treat_critical +
            cases * 0.1 * cost_treat_home, # 10% cases treated at home; CABP note: 10% should exclude various forms of hosp cases?
        death_o * cost_per_death
)])

#' discount rate
#' TODO: extract from input table
disc.rate.cost  <- 0.03
disc.rate.daly  <- 0 # must match values available in daly_scenarios.csv currently 0 / 0.03

# load epi scenario definitions

# grab strategy_str by scenario
outcomes.dt[epi_scen.dt[id %in% unique(scenarioId)], on=.(scenarioId = id), strategy_str := strategy_str ]
outcomes.dt[,
    disc_factor.cost := 1/(1 + disc.rate.cost)^(anni_year - 1)
]
outcomes.dt[,
    disc_factor.daly := 1/(1 + disc.rate.daly)^(anni_year - 1)
]

costs.dt <- rbindlist(lapply(vac_costs.dt[,unique(vac_price)], function(vp) {
    vp.dt <- vac_costs.dt[vac_price == vp, .(vac_price, strategy_str, tot_vac_costs)]
    res <- rbind(
        outcomes.dt[is.na(strategy_str)],
        outcomes.dt[vp.dt, on=.(strategy_str = strategy_str)],
        fill = TRUE
    )
    #' no vax costs for non-vax strats
    res[is.na(tot_vac_costs), tot_vac_costs := 0]
    res[is.na(vac_price), vac_price := vp ]
    res[(anni_year != 1) & (strategy_str != 0), tot_vac_costs := 0]
    cost_cols <- grep("^cost_", names(res), value = TRUE)
    res[, nonvac_costs := rowSums(.SD), .SDcols = cost_cols ]
    res[, total_costs := tot_vac_costs + nonvac_costs ]
    res
}))

costs.dt[, disc.total_costs := total_costs * disc_factor.cost ]

costs.mlt <- melt(
    costs.dt,
    id.vars = c("scenarioId","sampleId","age","anni_year","vac_price"),
    measure.vars = grep("(^cost|costs$)", names(costs.dt), value = TRUE)
)

lys.dt <- rbindlist(lapply(dalys.dt[, unique(daly_scenario)], function(ds) {
    d.dt <- dalys.dt[
        (disc.rate == disc.rate.daly) & (daly_scenario == ds),
        .(age_cat,daly_scenario,dalys_per_death = dalys)
    ]
    outcomes.dt[d.dt, on=.(age=age_cat)]
}))
lys.dt[, dalys := death_o * dalys_per_death ]
lys.dt[, disc.dalys := dalys * disc_factor.daly ]

lys.mlt <- melt(
    lys.dt,
    id.vars = c("scenarioId","sampleId","age","anni_year","daly_scenario"),
    measure.vars = c("dalys","disc.dalys")
)

accumulate.dt <- function(dt, tvar=expression(anni_year)) {
    dt[order(eval(tvar)), cvalue := cumsum(value), by=setdiff(names(dt),c(tvar, "value","cvalue"))]
}

accumulate.dt(lys.mlt)
accumulate.dt(costs.mlt)

allage.dt <- function(dt) {
    dt[, .(
        value = sum(value), cvalue = sum(value), age = "all"
    ), by=setdiff(names(dt), c("age","value","cvalue"))]
}

sum.costs.dt <- allage.dt(costs.mlt)
sum.lys.dt <- allage.dt(lys.mlt)

baseline.costs <- sum.costs.dt[scenarioId == ref_scen_id]
averted.costs.dt <- sum.costs.dt[scenarioId != ref_scen_id][
    baseline.costs, on=.(sampleId, anni_year, vac_price, variable, age)
][, .(
    scenarioId, sampleId, anni_year, vac_price, variable, age,
    averted = i.value - value, caverted = i.cvalue - cvalue
)]

baseline.lys <- sum.lys.dt[scenarioId == ref_scen_id]
averted.lys.dt <- sum.lys.dt[scenarioId != ref_scen_id][
    baseline.lys, on=.(sampleId, anni_year, daly_scenario, variable, age)
][, .(
    scenarioId, sampleId, anni_year, daly_scenario, variable, age,
    averted = i.value - value, caverted = i.cvalue - cvalue
)]

# cost_total_disc/dalys.disc
icer.dt <- averted.costs.dt[variable == "disc.total_costs"][
    averted.lys.dt[variable == "disc.dalys"], on = .(scenarioId, sampleId, anni_year, age),
    allow.cartesian = TRUE
]
icer.dt[, icer := -caverted / i.caverted ]

qtile <- function(
    v, ps = c(lo95=0.025, lo50=0.25, md=0.5, hi50=0.75, hi95=0.975),
    withMean = c("mn", NA),
    fmt = "value.%s"
) {
    qs <- quantile(v, probs = ps)
    names(qs) <- sprintf(fmt, names(ps))
    if (!is.na(withMean[1])) {
        mn <- mean(v)
        names(mn) <- sprintf(fmt, withMean[1])
        qs <- c(qs, mn)
    }
    as.list(qs)
}


q.costs.dt <- sum.costs.dt[,{
    qv <- qtile(value)
    qc <- qtile(cvalue, fmt = "cvalue.%s")
    c(qv, qc)
},by=setdiff(names(sum.costs.dt),c("sampleId","value","cvalue"))]
qa.costs.dt <- averted.costs.dt[,{
    qv <- qtile(averted, fmt = "averted.%s")
    qc <- qtile(caverted, fmt = "caverted.%s")
    c(qv, qc)
},by=setdiff(names(averted.costs.dt),c("sampleId","averted","caverted"))]

q.lys.dt <- sum.lys.dt[,{
    qv <- qtile(value)
    qc <- qtile(cvalue, fmt = "cvalue.%s")
    c(qv, qc)
},by=setdiff(names(sum.lys.dt),c("sampleId","value","cvalue"))]
qa.lys.dt <- averted.lys.dt[,{
    qv <- qtile(averted, fmt = "averted.%s")
    qc <- qtile(caverted, fmt = "caverted.%s")
    c(qv, qc)
},by=setdiff(names(averted.lys.dt),c("sampleId","averted","caverted"))]

q.icer.dt <- icer.dt[,{
    qtile(icer)
},by=.(scenarioId, anni_year, vac_price, daly_scenario)]

saveRDS(q.costs.dt, tail(.args, 1))
saveRDS(qa.costs.dt, gsub("\\.rds","_averted.rds", tail(.args, 1)))

saveRDS(q.lys.dt, tail(.args, 3)[1])
saveRDS(qa.lys.dt, gsub("\\.rds","_averted.rds", tail(.args, 2)[1]))

saveRDS(q.icer.dt, tail(.args, 3)[2])
