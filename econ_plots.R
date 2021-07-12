
figext <- "tiff"

require(data.table)
require(ggplot2)

# time-horizon (years)
#TODO still needs manual tinkering if t-horizon is not 10

t_horizon <- 10

# paths
path.in  <- "~/Dropbox/Covid-WHO-vax/inputs/"
path.out <- "~/Dropbox/Covid-WHO-vax/outputs/"
# path.fig <- "~/Dropbox/Covid-WHO-vax/figures/"
path.fig <- "~/Dropbox/Covid-WHO-vax/figures/revised_after_review/"

# load epi scenario info
epi_scen.dt <-  as.data.table(readRDS(paste0(path.in,"config.rds")))

# load econ scenario info

othercosts <- dcast(fread("covid_other_costs.csv"), perspective ~ name, value.var = "cost")
vac_cost.dt <- fread("covid_vac_costs_per_dose.csv")[scenario == "campaign"]
dalys.dt <- fread("daly_scenarios.csv")

econ_scen.dt <- data.table(expand.grid(
    perspective = othercosts[, unique(perspective)],
    vac_price = vac_cost.dt[, unique(vac_price)],
    daly_scenario = dalys.dt[, unique(daly_scenario)],
    disc.costs = dalys.dt[, max(disc_rate)],
    disc.dalys = dalys.dt[, unique(disc_rate)]
))[, econ_id := 1:.N ]

# econ_scen.dt <- data.table(readRDS(paste0(path.out,"econ_scns.rds")))

# load results

epi.dt <- data.table(readRDS(paste0(path.out,"epi_quantile.rds")))
econ.dt <- data.table(readRDS(paste0(path.out,"econ_quantile.rds")))

epi.dt <- epi.dt[qtile %in% c("lo95","md","hi95")]
econ.dt <- econ.dt[qtile %in% c("lo95","md","hi95") & # drop unused qtiles
                       view == "incremental" # only need incremental values
                   ]
# qtiles long to wide format
econ.dt <- dcast(
    econ.dt, 
    id + econ_id + view + anni_year ~ qtile,
    value.var = c("costs","dalys","ccosts","cdalys","icer")
)
epi.dt <- dcast(
    epi.dt, 
    id + age + anni_year ~ qtile,
    value.var = c("cases","death_o","cases.del","death_o.del","cases.cdel","death_o.cdel","cases.cval","death_o.cval")
)

# join scenario details
econ.dt <- econ_scen.dt[econ.dt, on = "econ_id"]
econ.dt <- epi_scen.dt[econ.dt, on = "id"]
econ.dt <- econ.dt[start_timing==18718] # just April 1st campaign start


# find scenario ids for epi and econ base cases

base_id <- epi_scen.dt[#setId==0 &
                           strategy=="campaign" &
                           vax_mech=="infection" &
                           eff_mech=="allornothing" &
                           vax_eff==0.7 &
                           nat_imm_dur_days==912 &
                           vax_imm_dur_days==912 &
                           start_timing=="2021-04-01" &
                           vax_delay==30 & # 2-dose regimen
                           repeat_period==0 &
                           repeat_number==0 &
                           seasonality=="none" &
                           doses_per_day==4000 &
                           increasing==TRUE &
                           strategy_str==365 & # 1 year campaign
                           from_age==14 &
                           to_age==16 &
                           R0=="fitted" &
                           contact_matrix=="prem et al" &
                           npis=="google mobility" &
                           susceptibility=="nat med fit" &
                           clin_frac=="nat med fit" &
                           subclin_inf==0.5 &
                           horizon==10 &
                           birthdeath=="yes" &
                           hosp_model=="current" &
                           icu_model=="current" &
                           death_model=="current"
                       ,id]

base_econ_id <- econ_scen.dt[perspective=="health_system" &
                                 vac_price==3 &
                                 daly_scenario=="high" &
                                 disc.costs==0.03 &
                                 disc.dalys==0.03 # changed base case to 3%
                             ,econ_id]


# parameter list for vaccination base case 

base.list <- list(scen_name="Vaccine base case",
                  #setId=0,
                  strategy="campaign",
                  vax_mech="infection",
                  eff_mech="allornothing",
                  vax_eff=0.7,
                  nat_imm_dur_days=912,
                  vax_imm_dur_days=912,
                  start_timing=as.Date("2021-04-01"),
                  vax_delay=30,
                  repeat_period=0,
                  repeat_number=0,
                  seasonality="none",
                  doses_per_day=4000,
                  increasing=TRUE,
                  strategy_str=365,
                  from_age=14,
                  to_age=16,
                  R0="fitted",
                  contact_matrix="prem et al",
                  npis="google mobility",
                  susceptibility="nat med fit",
                  clin_frac="nat med fit",
                  subclin_inf=0.5,
                  horizon=10,
                  birthdeath="yes",
                  hosp_model="current",
                  icu_model="current",
                  death_model="current",
                  perspective="health_system",
                  vac_price=3,
                  daly_scenario="high",
                  disc.costs=0.03,
                  disc.dalys=0.03 # changed base case to 3%
)

# build data table for plots 

epi.base.list <- head(base.list,-5) # drop econ parameters
epi.base.list <- tail(epi.base.list,-1) # drop econ parameters

vars <- c("strategy_str","nat_imm_dur_days") # epi vars for faceting plots
epi.sens.list <- list()
for (v in vars){
    epi.base.list[[v]] <- NULL
    epi.sens.list[[v]]<- unique(epi_scen.dt[!is.na(get(v)),get(v)])
}

plots.dt <- rbind(
    data.table(do.call(expand.grid, c(epi.base.list,epi.sens.list))),
    fill = TRUE
)

n <- names(plots.dt)
plots.dt <- econ.dt[plots.dt,on=n]

# fudge to get 10 year campaign in order - need to update
plots.dt[strategy_str==0,strategy_str:=3650]
plots.dt[order(strategy_str)]

refdate <- as.Date("2021-04-01")
plots.dt[, anni_date := anni_year*365 + refdate ]
# plot of incremental costs

plt.costs <- function(meas = "costs_md", 
                      lbl = "Annual Incremental Cost ($)",
                      nat_imm_dur_days.lbl = c('365'="Natural immunity\n1 year",'912'="2.5 years", '1825'="5 years", 'Inf'="Life-long" ),
                      strategy_str.lbl = c('3650'="10 year campaign",'365'="1 year campaign", '1825'="5 year campaign"),
                      t_horizon = 10,
                      showX = TRUE, 
                      #high = "#56B1F7"
                      high = "#FFA7A8",
                      mid = "#BA585C",
                      low = "#000000") ggplot(plots.dt[
        daly_scenario == "high" & 
        disc.dalys == 0.03 &
        perspective %in% c("health_system","societal")
]) +
    facet_grid(
        strategy_str ~ nat_imm_dur_days,
        labeller = labeller(
            nat_imm_dur_days = nat_imm_dur_days.lbl,
            strategy_str = strategy_str.lbl
        )
    ) +
    aes(
        anni_date, color = vac_price,
        linetype = factor(perspective),
        group = interaction(vac_price, perspective)
    ) +
    geom_line(aes(y=get(meas))) +
    theme_minimal() +
    theme(legend.position="top") +
    theme(
        panel.border=element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.minor = element_blank()
    ) +
    scale_color_gradient2(
        "Vaccine price",
        breaks = c(3,6,10),
        labels = c("$3", "$6", "$10"),
        guide = "legend",
        #limits = c(3,10),
        midpoint = 6,
        low = low,
        mid = mid,
        high = high
    ) +
    scale_linetype_discrete(
        name = "Cost perspective",
        labels = c("Health system", "Societal")
    ) +
    scale_x_date(
        "Calendar Year",
        breaks = refdate + (0:10)*365,
        date_labels = "'%y"
    ) +
    scale_y_continuous(
        sprintf("%s", lbl), labels = scales::label_number_si()
    )

plot.costs <- plt.costs(t_horizon=t_horizon)
plot.cum.costs <- plt.costs(meas="ccosts_md", lbl="Cumulative Incremental Cost ($)",t_horizon=t_horizon)

tarfile <- sprintf("%sfig2_cost_plot_%s_time-horizon.%s",path.fig,t_horizon,figext)
ggsave(tarfile, plot.costs, width = 7.5, height = 6, units = "in")

# plot of incremental dalys

plt.dalys <- function(meas = "dalys_md", 
                      lbl = "Annual DALYs Averted",
                      nat_imm_dur_days.lbl = c('365'="Natural immunity\n1 year",'912'="2.5 years", '1825'="5 years", 'Inf'="Life-long" ),
                      strategy_str.lbl = c('3650'="10 year campaign",'365'="1 year campaign", '1825'="5 year campaign"),
                      showX = TRUE, 
                      t_horizon=10,
                      #high = "#56B1F7"
                      high = "#78B978") ggplot(plots.dt[
        vac_price == 3 &
        perspective == "health_system"
]) +
    facet_grid(
        strategy_str ~ nat_imm_dur_days,
        labeller = labeller(
            nat_imm_dur_days = nat_imm_dur_days.lbl,
            strategy_str = strategy_str.lbl
        )
    ) +
    aes(
        anni_date, color = disc.dalys,
        linetype = factor(daly_scenario),
        group = interaction(disc.dalys, daly_scenario)
    ) +
    geom_line(aes(y=get(meas))) +
    theme_minimal() +
    theme(legend.position="top") +
    theme(
        panel.border=element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.minor = element_blank()
    ) +
    scale_color_continuous(
        "Discount rate",
        labels = c("0%", "3%"),
        breaks = c(0.00,0.03),
        guide = "legend",
        high = high
    ) +
    scale_linetype_discrete(
        name = "Comorbidities",
        labels = c("The same as the\ngeneral population", 
                   "Higher than the \ngeneral population")
    ) +
    scale_x_date(
        "Calendar Year",
        breaks = refdate + (0:10)*365,
        date_labels = "'%y"
    ) +
    scale_y_continuous(
        sprintf("%s", lbl), labels = scales::label_number_si()
    )

plot.dalys <- plt.dalys(t_horizon=t_horizon)
plot.cum.dalys <- plt.dalys(meas="cdalys_md", lbl="Cumulative DALYs Averted",t_horizon=t_horizon)

tarfile <- sprintf("%sfig3_daly_plot_%s_time-horizon.%s",path.fig,t_horizon,figext)
ggsave(tarfile, plot.cum.dalys, width = 7.5, height = 6, units = "in")

# plot of ICERS

plt.icers1 <- function(meas = "icer_md", 
                      lbl = "ICER ($)",
                      nat_imm_dur_days.lbl = c('365'="Natural immunity\n1 year",'912'="2.5 years", '1825'="5 years", 'Inf'="Life-long" ),
                      strategy_str.lbl = c('3650'="10 year campaign",'365'="1 year campaign", '1825'="5 year campaign"),
                      showX = TRUE, 
                      t_horizon=10,
                      #high = "#56B1F7"
                      high = "#6656F7") ggplot(plots.dt[
                          daly_scenario == "high" & 
                          disc.dalys == 0.03 &
                          perspective %in% c("health_system","societal") #&
                              #nat_imm_dur_days !="Inf"
                      ]) +
    facet_grid(
        strategy_str ~ nat_imm_dur_days,
        labeller = labeller(
            nat_imm_dur_days = nat_imm_dur_days.lbl,
            strategy_str = strategy_str.lbl
        )
    ) +
    aes(
        anni_year, color = vac_price,
        linetype = factor(perspective),
        group = interaction(vac_price, perspective)
    ) +
    geom_line(aes(y=get(meas))) +
    theme_minimal() +
    theme(legend.position="top") +
    theme(
        panel.border=element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.minor = element_blank()
    ) +
    scale_color_continuous(
        "Vaccine price",
        breaks = c(3,6,10),
        labels = c("$3", "$6", "$10"),
        guide = "legend",
        high = high
    ) +
    scale_linetype_discrete(
        name = "Cost perspective",
        labels = c("Health system", "Societal")
    ) +
    scale_x_continuous(
        "Time-horizon (years)",
        breaks = 0:10
    ) +
    scale_y_continuous(
        sprintf("%s", lbl), labels = scales::label_number_si()
    ) + 
    coord_cartesian(ylim = c(-1000,5000),)

plot.icers1 <- plt.icers1(t_horizon=t_horizon)

tarfile <- sprintf("%ssup_fig_icer_plot1_%s_time-horizon.%s",path.fig,t_horizon,figext)
ggsave(tarfile, plot.icers1, width = 7.5, height = 6, units = "in")


plt.icers2 <- function(meas = "icer_md", 
                       lbl = "ICER ($)",
                       nat_imm_dur_days.lbl = c('365'="Natural immunity\n1 year",'912'="2.5 years", '1825'="5 years", 'Inf'="Life-long" ),
                       strategy_str.lbl = c('3650'="10 year campaign",'365'="1 year campaign", '1825'="5 year campaign"),
                       showX = TRUE, 
                       t_horizon=10,
                       #high = "#56B1F7"
                       high = "#6656F7") ggplot(plots.dt[
                           vac_price == 3 &
                               perspective == "health_system" #&
                               #nat_imm_dur_days !="Inf"
                       ]) +
    facet_grid(
        strategy_str ~ nat_imm_dur_days,
        labeller = labeller(
            nat_imm_dur_days = nat_imm_dur_days.lbl,
            strategy_str = strategy_str.lbl
        )
    ) +
    aes(
        anni_year, color = disc.dalys,
        linetype = factor(daly_scenario),
        group = interaction(disc.dalys, daly_scenario)
    ) +
    geom_line(aes(y=get(meas))) +
    theme_minimal() +
    theme(legend.position="top") +
    theme(
        panel.border=element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.minor = element_blank()
    ) +
    scale_color_continuous(
        "DALY discount rate",
        labels = c("0%", "3%"),
        breaks = c(0.00,0.03),
        guide = "legend",
        high = high
    ) +
    scale_linetype_discrete(
        name = "Comorbidities",
        labels = c("The same as the\ngeneral population", 
                   "Higher than the \ngeneral population")
    ) +
    scale_x_continuous(
        "Time-horizon (years)",
        breaks = 0:10
    ) +
    scale_y_continuous(
        sprintf("%s", lbl), labels = scales::label_number_si()
    ) + 
    coord_cartesian(ylim = c(-1000,5000),)

plot.icers2 <- plt.icers2(t_horizon=t_horizon)

tarfile <- sprintf("%ssup_fig_icer_plot2_%s_time-horizon.%s",path.fig,t_horizon,figext)
ggsave(tarfile, plot.icers2, width = 7.5, height = 6, units = "in")

# Scenarios table

# Generate scenario analysis list. MUST be a better way to do this...
# update to 3% qaly discounting as the base case
scen.list <- list()

to.add <- base.list
to.add$scen_name = "Vaccine base case"
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Target 15+ from outset"
to.add$from_age = 4
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "5 year campaign"
to.add$strategy_str = 1825
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "10 year campaign"
to.add$strategy_str = 0
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Slow roll out: 4K courses per day (no scale-up) for 10 years"
to.add$strategy_str = 0
to.add$doses_per_day = 4000
to.add$increasing = FALSE
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Fast roll out: 184K courses per day (no scale-up) for 6 months"
to.add$from_age = 4
to.add$strategy_str = 180
to.add$doses_per_day = 184000
to.add$increasing = FALSE
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "1 year vaccine & natural immunity waning"
to.add$nat_imm_dur_days = 365
to.add$vax_imm_dur_days = 365
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "5 year vaccine & 2.5 year natural immunity waning"
to.add$vax_imm_dur_days = 1825
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "1 dose regimen (twice rate of people vaccinated)"
to.add$vax_delay = 0
to.add$doses_per_day = 8000
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "30% vaccine efficacy"
to.add$vax_eff = 0.3
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "90% vaccine efficacy"
to.add$vax_eff = 0.9
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Vaccine protects against disease not infection"
to.add$vax_mech = "disease"
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Vaccine protection is leaky"
to.add$eff_mech = "leaky"
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "$10 price per dose"
to.add$vac_price = 10
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "DALYs discounted at 0%" # 0% now scenario not base case
to.add$disc.dalys = 0.00
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "DALYs based on higher comorbidities"
to.add$daly_scenario = "low"
scen.list <- rbindlist(list(scen.list, to.add))

to.add <- base.list
to.add$scen_name = "Societal perspective"
to.add$perspective = "societal"
scen.list <- rbindlist(list(scen.list, to.add))

n <- names(scen.list)[-1] # name of fields to join on
scen.dt <- as.data.table(scen.list)
scen.dt <- econ.dt[scen.dt,on=n][anni_year==t_horizon]
scen.dt[,scen_id:=.I]

# summary of 10 year costs and dalys averted
scen.tab <- scen.dt[,.("Scenario No."=scen_id,"Description"=scen_name,
                       "Difference in Cost ($ millions)" = sprintf(
                           "%.1f (%.1f, %.1f)",
                           ccosts_md/10^6,ccosts_lo95/10^6,ccosts_hi95/10^6
                           ),
                       "DALYs Averted (thousands)" = sprintf(
                           "%.1f (%.1f, %.1f)",
                           cdalys_md/10^3,cdalys_lo95/10^3,cdalys_hi95/10^3
                       ),
                       "$ per DALY Averted" = sprintf(
                           "%.1f (%.1f, %.1f)",
                           icer_md,icer_lo95,icer_hi95
                       ),
                       id,econ_id)]



# add in cases and deaths averted
ids <- unique(scen.tab[,id])
scen.tab <- scen.tab[
    epi.dt[age == "all"][id %in% ids & anni_year == t_horizon,
           .("Cases Averted (millions)" = sprintf(
                 "%.1f (%.1f, %.1f)",
                 cases.cdel_md/10^6, cases.cdel_lo95/10^6, cases.cdel_hi95/10^6
             ),
             "Deaths Averted (thousands)" = sprintf(
                 "%.1f (%.1f, %.1f)",
                 death_o.cdel_md/10^3,death_o.cdel_lo95/10^3,death_o.cdel_hi95/10^3
             )
            ), 
           by=id
    ],
    on="id"
]
scen.tab[,id:=NULL]
scen.tab[,econ_id:=NULL]

write.csv(scen.tab[order(`Scenario No.`)],
          sprintf("%sscenario_table_%s_time-horizon.csv",path.fig,t_horizon)
          )


# 
# plt.icers <- function(meas = "icer_md", 
#                       lbl = "ICER ($)",
#                       showX = TRUE, 
#                       #high = "#56B1F7"
#                       high = "#FFA7A8",
#                       mid = "#BA585C",
#                       low = "#000000") ggplot(scen.dt) +
#     aes(
#         anni_year, color=scen_name
#     ) +
#     geom_line(aes(y=get(meas))) +
#     theme_minimal() +
#     theme(legend.position="top") +
#     theme(
#         panel.border=element_rect(colour = "black", fill=NA, size=0.5),
#         panel.grid.minor = element_blank()
#     ) +
#     scale_y_continuous(
#         sprintf("%s", lbl), labels = scales::label_number_si()
#     )


