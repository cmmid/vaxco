update_output = F # only writes out updated rds files if true

require(RSQLite)
require(data.table)
require(readr)

# paths
path.in  <- "~/Dropbox/Covid-WHO-vax/inputs/"
path.out <- "~/Dropbox/Covid-WHO-vax/outputs/"

# discount rate
disc.rate.cost  <- 0.03
disc.rate.daly  <- 0 # must match values available in daly_scenarios.csv currently 0 / 0.03

# load DALY data
dalys.dt <- data.table(fread(paste0(path.in,"daly_scenarios.csv")))

# load cost inputs
vac_costs.dt <- data.table(fread(paste0(path.in,"covid_vac_cost_inputs.csv")))

vac_costs.dt <- vac_costs.dt[,tot_vac_costs := tot_vac_costs/16] # divide across 16 age bands
vac_costs.dt <- vac_costs.dt[,total_vac_and_supplies_inc_buffer 
                             := total_vac_and_supplies_inc_buffer/16] # divide across 16 age bands
vac_costs.dt <- vac_costs.dt[,total_inc_coldchain 
                             := total_inc_coldchain/16] # divide across 16 age bands
vac_costs.dt <- vac_costs.dt[,total_campaign_operational 
                             := total_campaign_operational/16] # divide across 16 age bands

other_costs.dt <- data.table(fread(paste0(path.in,"covid_other_cost_inputs.csv")))
other_costs <- dcast(other_costs.dt[,c("short_desc","cost")],. ~ short_desc, value.var = "cost")

# load epi scenario definitions
drv <- RSQLite::SQLite()
conn <- dbConnect(drv, dbname=paste0(path.in,"config_high.sqlite"))

epi_pars.dt  <- data.table(dbReadTable(conn,"parameter"))
epi_scen.dt  <- data.table(dbReadTable(conn,"scenario"))

dbDisconnect(conn)

# select epi scenarios starting 01-Jan-21 = 18628
scen_ids <- epi_scen.dt[start_timing==18628,id]

# scen_ids <- c(2,13,122)

#load selected epi scenarios
epi_outcomes.dt = NULL
for (s in scen_ids){
    conn <- dbConnect(drv,
                          dbname=sprintf("%smetrics_%02d.sqlite",path.out,s)
    )
    epi_outcomes.dt <- rbind(
        data.table(dbGetQuery(
            conn,
            "SELECT *
            FROM metrics 
            WHERE outcome IN ('cases', 
                            'death_o', 
                            'non_icu_severe_i', 
                            'non_icu_severe_p',
                            'non_icu_critical_i', 
                            'non_icu_critical_p', 
                            'icu_critical_i', 
                            'icu_critical_p')"
            )
        ), 
        epi_outcomes.dt
    )
    dbDisconnect(conn)
}

# create dt with annual instead of cumulative outcomes 
outcomes.dt <- epi_outcomes.dt[order(simday), 
                               value_numeric := c(value_numeric[1], diff(value_numeric)), 
                               by=.(scenarioId, sampleId, age, outcome)
                               ]

# convert to wide format
outcomes.dt <- dcast(outcomes.dt, 
                     scenarioId + sampleId + age + simday ~ outcome, 
                     value.var = "value_numeric"
)

# add var for anniversary year 
outcomes.dt[,anni_year := rank(simday) - 1,  by=.(scenarioId, sampleId, age)]

# create cost varitables and set to zero
outcomes.dt[, 
            c("cost_ERM",
              "cost_comms",
              "cost_trace",
              "cost_test",
              "cost_treat",
              "cost_death") 
            :=
                list(0,0,0,0,0,0)
            ]

# add non-vac costs
outcomes.dt[anni_year > 0,
            c("cost_ERM",
              "cost_comms",
              "cost_trace",
              "cost_test",
              "cost_treat",
              "cost_death") 
            :=
                list(0 * other_costs$cost_day_erm +    # to update
                         0 * other_costs$cost_one_erm, # to update
                     0 * other_costs$cost_day_comms,   # to update
                     cases * 10 *  other_costs$cost_per_traced + # 10 traces per case
                         0 * other_costs$cost_per_quarantined,   # to update
                     (non_icu_severe_i + non_icu_critical_i) * other_costs$cost_per_test,
                     (non_icu_severe_p + non_icu_critical_p) * other_costs$cost_day_treat_general +
                         icu_critical_p * other_costs$cost_day_treat_critical +
                         cases * 0.1 * other_costs$cost_treat_home, # 10% cases treated at home
                     death_o * other_costs$cost_per_death
                )
            ]

# vac costs for different price scenarios

vac_price <- data.table(vac_price=c("low","med","high")) # vac price scenarios 
outcomes.dt <- outcomes.dt[,vac_price[],by=names(outcomes.dt)] # combine epi and vac price scenarios

outcomes.dt <- merge ( # get vac stragey for epi scenarios
    outcomes.dt, 
    epi_scen.dt[,c("id","strategy_str")],
    by.x = c("scenarioId"),
    by.y = c("id"),
    all.x = TRUE
)

outcomes.dt <- merge( # join total vac cost based on vac price and campaign duration (strategy_str)
    outcomes.dt, 
    vac_costs.dt[,c("vac_price","strategy_str","tot_vac_costs")],
    by.x = c("vac_price","strategy_str"),
    by.y = c("vac_price","strategy_str"),
    all.x = TRUE
)

outcomes.dt[is.na(tot_vac_costs), tot_vac_costs := 0] # non-vaccine scenarios
outcomes.dt[anni_year != 1 & strategy_str != 0, tot_vac_costs := 0] # 0 if not year 1, except ongoing

# total costs
outcomes.dt[,
            cost_total := (cost_ERM + cost_comms + cost_trace + cost_test + cost_treat + cost_death 
            + tot_vac_costs)
            ]

# discount total costs
outcomes.dt[anni_year > 1, disc_factor.cost := 1/(1 + disc.rate.cost)^(anni_year - 1)]
outcomes.dt[anni_year <=1, disc_factor.cost := 1]

outcomes.dt[, cost_total_disc := cost_total * disc_factor.cost]

# DALYS
daly_scen <- data.table(daly_scen=c("high","low")) 
outcomes.dt <- outcomes.dt[,daly_scen[],by=names(outcomes.dt)] # combine daly scenarios

# join to dalys.dt to get discounted age-specific dalys

outcomes.dt <- merge(
    outcomes.dt, 
    dalys.dt[disc.rate == disc.rate.daly,c("age_cat","daly_scenario","dalys")], # match disc.rate
    by.x = c("age","daly_scen"),
    by.y = c("age_cat","daly_scenario"),
    all.x = TRUE
)

# further discount lifetime dalys by year of death
outcomes.dt[anni_year > 1, disc_factor.daly := 1/(1 + disc.rate.daly)^(anni_year - 1)]
outcomes.dt[anni_year <=1, disc_factor.daly := 1]

outcomes.dt[anni_year > 0, dalys.disc := death_o * dalys * disc_factor.daly]
outcomes.dt[anni_year == 0, dalys.disc := 0] # set zero in year 0

# melt
outcomes.dt <- melt(outcomes.dt, 
                    measure.vars = c("cases",
                                     "death_o",
                                     "icu_critical_i",
                                     "icu_critical_p",
                                     "non_icu_critical_i",
                                     "non_icu_critical_p",
                                     "non_icu_severe_i",
                                     "non_icu_severe_p",
                                     "cost_ERM",
                                     "cost_comms",
                                     "cost_trace",
                                     "cost_test",
                                     "cost_treat",
                                     "cost_death",
                                     "tot_vac_costs",
                                     "cost_total",
                                     "disc_factor.cost",
                                     "disc_factor.daly",
                                     "cost_total_disc",
                                     "dalys",
                                     "dalys.disc"
                    ), 
                    variable.name = "outcome", 
                    value.name = "val"
)

# drop year 0
outcomes.dt <- outcomes.dt[anni_year > 0,]

# add cumulative values
outcomes.dt <- outcomes.dt[,
                           cum_val := cumsum(val),
                           by = .(scenarioId, sampleId, age, vac_price, daly_scen, outcome)
                           ]

# collapse age structure
outcomes.dt <- outcomes.dt[,
                           .(val = sum(val), cum_val = sum(cum_val)),
                           by=.(scenarioId, sampleId, vac_price, daly_scen, anni_year, simday, outcome)
                           ]

if (update_output){write_rds(outcomes.dt, paste0(path.out,"outcomes_tot.rds"))}

# summarise totals
summary_tot.dt <- outcomes.dt[,
                              .(
                                  val.mn = mean(val),
                                  val.md = median(val),
                                  val.lo95 = quantile(val,0.025,na.rm=T),
                                  val.hi95 = quantile(val,0.975,na.rm=T),
                                  cum_val.mn = mean(cum_val),
                                  cum_val.md = median(cum_val),
                                  cum_val.lo95 = quantile(cum_val,0.025,na.rm=T),
                                  cum_val.hi95 = quantile(cum_val,0.975,na.rm=T)
                              ),
                              by = .(scenarioId, vac_price, daly_scen, anni_year, simday, outcome)
                              ]

if (update_output){write_rds(summary_tot.dt, paste0(path.out,"econ_summary_tot.rds"))}

# calculate increment from baseline
outcomes.dt <- outcomes.dt[order(scenarioId),
                           inc_val := val - val[1],
                           by=.(sampleId, vac_price, daly_scen, anni_year, simday, outcome)
                           ]

outcomes.dt <- outcomes.dt[order(scenarioId),
                           cum_inc_val := cum_val - cum_val[1],
                           by=.(sampleId, vac_price, daly_scen, anni_year, simday, outcome)
                           ]

outcomes.dt <- outcomes.dt[scenarioId != 2]   # drop reference scenario from inc results
outcomes.dt <- outcomes.dt[,val := NULL]      # drop val
outcomes.dt <- outcomes.dt[,cum_val := NULL]  # drop cum_val

if (update_output){write_rds(outcomes.dt, paste0(path.out,"outcomes_inc.rds"))}

# summarise incremental results
summary_inc.dt <- outcomes.dt[,
                              .(
                                  inc_val.mn = mean(inc_val),
                                  inc_val.md = median(inc_val),
                                  inc_val.lo95 = quantile(inc_val,0.025,na.rm=T),
                                  inc_val.hi95 = quantile(inc_val,0.975,na.rm=T),
                                  cum_inc_val.mn = mean(cum_inc_val),
                                  cum_inc_val.md = median(cum_inc_val),
                                  cum_inc_val.lo95 = quantile(cum_inc_val,0.025,na.rm=T),
                                  cum_inc_val.hi95 = quantile(cum_inc_val,0.975,na.rm=T)
                              ),
                              by = .(scenarioId, vac_price, daly_scen, anni_year, simday, outcome)
                              ]

if (update_output){write_rds(summary_inc.dt, paste0(path.out,"econ_summary_inc.rds"))}

# CALCULATE ICERS USING CUMULATIVE RESULTS FOR ANNI_YEAR 10

# keep only cumulative results for final anniversary
outcomes.dt <- outcomes.dt[anni_year==10,]
outcomes.dt <- outcomes.dt[,inc_val := NULL]

outcomes.dt <- outcomes.dt[outcome %in% c("cost_total_disc","dalys.disc"),]

# re-cast
outcomes.dt <- dcast(outcomes.dt,
                     scenarioId + sampleId + vac_price + daly_scen + anni_year + simday ~ outcome,
                     value.var = "cum_inc_val"
)

outcomes.dt <- outcomes.dt[, dalys.disc := -1 * dalys.disc] # express as dalys averted
outcomes.dt <- outcomes.dt[, icer :=  cost_total_disc/dalys.disc]

# re-melt
outcomes.dt <- melt(outcomes.dt, 
                    measure.vars = c("cost_total_disc",
                                     "dalys.disc",
                                     "icer"
                    ), 
                    variable.name = "outcome", 
                    value.name = "cum_inc_val"
)

if (update_output){write_rds(outcomes.dt, paste0(path.out,"outcomes_icer_inc.rds"))}

#summarise incremental results
summary_icer_inc.dt <- outcomes.dt[,
                              .(
                                  cum_inc_val.mn = mean(cum_inc_val),
                                  cum_inc_val.md = median(cum_inc_val),
                                  cum_inc_val.lo95 = quantile(cum_inc_val,0.025,na.rm=T),
                                  cum_inc_val.hi95 = quantile(cum_inc_val,0.975,na.rm=T)
                              ),
                              by = .(scenarioId, vac_price, daly_scen, anni_year, simday, outcome)
                              ]

if (update_output){write_rds(summary_icer_inc.dt, paste0(path.out,"econ_summary_icer_inc.rds"))}

