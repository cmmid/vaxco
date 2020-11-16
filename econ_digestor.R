
# ------------------------------------------------------------------------------
# FUNCTION: econ_digestor 
# Calculates incremental costs, dalys averted, and ICERS for a given
# vaccine intervention scenario
#
# Inputs:
#
# epi_scen.dt - epi runs for vaccine scenario (annual totals)
# epi_base.dt - epi runs for no-vaccine comparator (annual totals)
# dalys.dt - dt of dalys per age cat - selected to match disc.rate.daly
# econ_pars - list with: 
#             cost_vac_annual - annual cost of vaccination program
#             year_vac_final - the last year to which cost_vac_annual is applied
#             other health system / household costs, 
#             disc.rate.cost, 
#             disc.rate.daly
#
# ------------------------------------------------------------------------------

econ_digestor <- function(epi_scen.dt, epi_base.dt, dalys.dt, econ_pars){
    
    # divide one-off annual/daily costs across age categories
    age_cats <- epi_scen.dt[, max(age)] 
    econ_pars[["cost_vac_annual"]] <- econ_pars[["cost_vac_annual"]] / age_cats
    econ_pars[["cost_hs_day_erm"]] <- econ_pars[["cost_hs_day_erm"]] / age_cats
    econ_pars[["cost_hs_one_erm"]] <- econ_pars[["cost_hs_one_erm"]] / age_cats
    econ_pars[["cost_hs_day_comms"]] <- econ_pars[["cost_hs_day_comms"]] / age_cats

    # define annual anniversaries
    epi_scen.dt[, anni_year := as.integer((simday-min(simday))/365) ]
    epi_base.dt[, anni_year := as.integer((simday-min(simday))/365) ]
    
    # calculate annual totals from cumulative totals
    epi_scen.dt[
        order(anni_year),
        inc := c(value_numeric[1], diff(value_numeric)),
        by = .(scenarioId, sampleId, age, outcome)
    ]
    
    epi_base.dt[
        order(anni_year),
        inc := c(value_numeric[1], diff(value_numeric)),
        by = .(scenarioId, sampleId, age, outcome)
    ]
    
    # long to wide format, year zero
    epi_scen.dt <- dcast(
        epi_scen.dt[anni_year != 0], 
        scenarioId + sampleId + age + anni_year ~ outcome,
        value.var = "inc"
    )
    
    epi_base.dt <- dcast(
        epi_base.dt[anni_year != 0], 
        scenarioId + sampleId + age + anni_year ~ outcome,
        value.var = "inc"
    )
    
    # add non-vaccine costs
    
    add_costs <- function(scen.dt, econ_pars){
    with(econ_pars, scen.dt[,
        costs := (1/(1 + disc.rate.cost)^(anni_year - 1)) * (
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
                (cost_hh_treat_general_med_per_day + 
                     cost_hh_treat_general_non_med_per_day) +
            # household: medical + non-med costs while in icu
            icu_critical_p * (cost_hh_treat_critical_med_per_day
                + cost_hh_treat_critical_non_med_per_day) +
            # household: medical + non-med costs for non-hospitalised cases
            # TODO: update with prevalent cases or alternative assumption about
            # treatment duration at home
            (cases - (non_icu_severe_i + non_icu_critical_i)) * 
                3.5 * (cost_hh_treat_home_med_per_day + 
                     cost_hh_treat_home_non_med_per_day) +
            # household: individual and caregiver lost income
            # TODO: update with prevalent cases or alternative assumption about
            # days of lost income per case
            cases * 3.5 * (cost_hh_individual_income_per_day +
                               cost_hh_caregiver_income_per_day)
        )])
        return(scen.dt)
    }
    
    add_costs(epi_scen.dt,econ_pars) # add costs to intervention scenario
    add_costs(epi_base.dt,econ_pars) # add costs to base case
    
    # add annual vaccine costs for relevant years for vaccine scenarios
    
    with(econ_pars,epi_scen.dt[,
        costs := ifelse(
            anni_year > year_vac_final,
            costs,
            costs +
            ((1/(1 + disc.rate.cost)^(anni_year-1)) * death_o * cost_vac_annual)
        )
    ])
    
    # add dalys using age-specific values from dalys.dt
    
    with(econ_pars,epi_scen.dt[,
        dalys := (1/(1 + disc.rate.daly)^(anni_year-1)) *
            death_o * daly.dt[age_cat == age_cat, dalys]
    ])
    
    with(econ_pars,epi_scen.dt[,
        dalys := (1/(1 + disc.rate.daly)^(anni_year-1)) *
            death_o * daly.dt[age_cat == age_cat, dalys]
    ])

    # CALCULATION OF OUTPUTS HERE


    
}

