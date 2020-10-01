
require(RSQLite)
require(data.table)
require(ggplot2)

# paths
path.in  <- "~/Dropbox/Covid-WHO-vax/inputs/"
path.out <- "~/Dropbox/Covid-WHO-vax/outputs/"
path.fig <- "~/Dropbox/Covid-WHO-vax/figures/"

#load epi scenario info
drv <- RSQLite::SQLite()
conn <- dbConnect(drv, dbname=paste0(path.in,"config_high.sqlite"))

epi_pars.dt  <- data.table(dbReadTable(conn,"parameter"))
epi_scen.dt  <- data.table(dbReadTable(conn,"scenario"))

dbDisconnect(conn)

# load results

summary_icers.dt <- data.table(readRDS(paste0(path.out,"econ_summary_icer_inc.rds")))
outcomes_icers.dt <- data.table(readRDS(paste0(path.out,"outcomes_icer_inc.rds")))

summary_inc.dt <- data.table(readRDS(paste0(path.out,"econ_summary_inc.rds")))
outcomes_inc.dt <- data.table(readRDS(paste0(path.out,"outcomes_inc.rds")))



# icer plots

# icers
plot.dt <- summary_icers.dt[outcome %in% c("icer") & anni_year == 10,]
plot.dt <- merge(plot.dt, epi_scen.dt, by.x="scenarioId", by.y="id", all.x=T, all.y=F)
plot.dt <- plot.dt[vax_eff %in% c(0.3,0.7),]

plot.dt[, campaign := factor(fifelse(
    strategy_str == 0, "Ongoing",
    sprintf("%i-day", strategy_str)
), levels = c(sprintf("%i-day", c(90, 365)), "Ongoing"), ordered = TRUE)]

plot.dt[, tar_age := factor(fifelse(from_age==4,"Vaccinate 15+","Vaccinate 65+"), 
                            levels = c("Vaccinate 15+","Vaccinate 65+"), ordered = TRUE)]

plot.dt[, dur_imm := factor(fifelse(vax_imm_dur_days==912,"Vac. Dur. 2.5Y",
                                    "Vac. Dur. 5Y"), 
                            levels = c("Vac. Dur. 2.5Y","Vac. Dur. 5Y"), 
                            ordered = TRUE)]

plot.dt[, vac_price := factor(vac_price,level=c("low","med","high"))]

plot_icer_1 <- ggplot(data = plot.dt, aes(x = vac_price, y = cum_inc_val.md, group = vac_price)) + 
    geom_bar(position="dodge",stat="identity",aes(fill=vac_price)) +
    scale_fill_brewer(palette = "Dark2", name="Vaccine Price") +
    xlab("Vaccine Price") + ylab("ICER ($ per DALY Averted") + 
    facet_grid (dur_imm + vax_eff ~ campaign + tar_age,   
                labeller = labeller(
        vax_eff = function(v) sprintf("Vac. Eff.= %s", v)
    )) + theme_minimal()

ggsave(paste0(path.fig,"plot_icer_1.png"), plot_icer_1, width = 10, height = 5, units = "in")


# CE plane
plot.dt <- outcomes_icers.dt[outcome %in% c("cost_total_disc","dalys.disc") & anni_year == 10,]
plot.dt <- dcast(plot.dt,
                 scenarioId + sampleId + vac_price + daly_scen ~ outcome,
                 value.var = "cum_inc_val"
)
plot.dt <- merge(plot.dt, epi_scen.dt, by.x="scenarioId", by.y="id", all.x=T, all.y=F)
plot.dt <- plot.dt[vax_eff %in% c(0.3,0.7),]

plot.dt[, campaign := factor(fifelse(
    strategy_str == 0, "Ongoing",
    sprintf("%i-day", strategy_str)
), levels = c(sprintf("%i-day", c(90, 365)), "Ongoing"), ordered = TRUE)]

plot.dt[, tar_age := factor(fifelse(from_age==4,"Vaccinate 15+","Vaccinate 65+"), 
                            levels = c("Vaccinate 15+","Vaccinate 65+"), ordered = TRUE)]

plot.dt[, dur_imm := factor(fifelse(vax_imm_dur_days==912,"Vac. Dur. 2.5Y",
                                    "Vac. Dur. 5Y"), 
                            levels = c("Vac. Dur. 2.5Y","Vac. Dur. 5Y"), 
                            ordered = TRUE)]

plot.dt[, vac_price := factor(vac_price,levels=c("low","med","high"),
                              labels=c("V.P.=low","V.P.=med","V.P.=high"))]

plot.dt[, daly_scen := factor(daly_scen,levels=c("low","high"),labels=c("DALYs=low","DALYs=high"))]

plot_icer_2 <- ggplot(data = plot.dt, aes(x = dalys.disc, y = cost_total_disc, group = sampleId)) +
    geom_point(size= 0.9, alpha = 0.8, aes(color = campaign, shape = tar_age)) +
    scale_color_brewer(palette="Dark2", name="Campaign Duration") +
    scale_shape(name="Target Age") +
    xlab("Incremental DALYs") + ylab("Incremental Costs ($)") +
    facet_grid (vax_eff + dur_imm ~ vac_price + daly_scen, 
                labeller = labeller(
        vax_eff = function(v) sprintf("Vac. Eff.= %s", v)
    )) + 
    scale_x_continuous(labels = scales::label_number_si()) +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45))

ggsave(paste0(path.fig,"plot_icer_2.png"), plot_icer_2, width = 10, height = 5, units = "in")



# cost plots

# discounted annual incremental cost: cost_total_disc
plot.dt <- summary_inc.dt[outcome %in% c("cost_total_disc")]
plot.dt <- merge(plot.dt, epi_scen.dt, by.x="scenarioId", by.y="id", all.x=T, all.y=F)
plot.dt <- plot.dt[vax_eff %in% c(0.3,0.7),]

plot.dt[, campaign := factor(fifelse(
    strategy_str == 0, "Ongoing",
    sprintf("%i-day", strategy_str)
), levels = c(sprintf("%i-day", c(90, 365)), "Ongoing"), ordered = TRUE)]

plot.dt[, tar_age := factor(fifelse(from_age==4,"Vaccinate 15+","Vaccinate 65+"), 
                            levels = c("Vaccinate 15+","Vaccinate 65+"), ordered = TRUE)]

plot.dt[, dur_imm := factor(fifelse(vax_imm_dur_days==912,"Vac. Dur. 2.5Y",
                                    "Vac. Dur. 5Y"), 
                            levels = c("Vac. Dur. 2.5Y","Vac. Dur. 5Y"), 
                            ordered = TRUE)]

plot.dt[, vac_price := factor(vac_price,level=c("low","med","high"))]

plot_cost_1 <- ggplot(data = plot.dt, aes(x = anni_year, y = inc_val.md, group = vac_price)) + 
    geom_bar(position="dodge",stat="identity",aes(fill=vac_price)) +
    scale_fill_brewer(palette = "Dark2", name="Vaccine Price") +
    xlab("Years since programme start") + ylab("Discounted Annual Incremental Costs ($)") + 
    facet_grid (dur_imm + vax_eff ~ campaign + tar_age,
                labeller = labeller(
        vax_eff = function(v) sprintf("Vac. Eff.= %s", v)
    )) + 
    scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme_minimal()

ggsave(paste0(path.fig,"plot_cost_1.png"), plot_cost_1, width = 10, height = 5, units = "in")



# daly plots

# discounted annual incremental dalys: cost_total_disc
plot.dt <- summary_inc.dt[outcome %in% c("dalys.disc")]
plot.dt <- merge(plot.dt, epi_scen.dt, by.x="scenarioId", by.y="id", all.x=T, all.y=F)
plot.dt <- plot.dt[vax_eff %in% c(0.3,0.7),]

plot.dt[, campaign := factor(fifelse(
    strategy_str == 0, "Ongoing",
    sprintf("%i-day", strategy_str)
), levels = c(sprintf("%i-day", c(90, 365)), "Ongoing"), ordered = TRUE)]

plot.dt[, tar_age := factor(fifelse(from_age==4,"Vaccinate 15+","Vaccinate 65+"), 
                            levels = c("Vaccinate 15+","Vaccinate 65+"), ordered = TRUE)]

plot.dt[, dur_imm := factor(fifelse(vax_imm_dur_days==912,"Vac. Dur. 2.5Y",
                                    "Vac. Dur. 5Y"), 
                            levels = c("Vac. Dur. 2.5Y","Vac. Dur. 5Y"), 
                            ordered = TRUE)]

plot.dt[, daly_scen := factor(daly_scen,level=c("low","high"))]

plot_daly_1 <- ggplot(data = plot.dt, aes(x = anni_year, y = -1*inc_val.md, group = daly_scen)) + 
    geom_bar(position="dodge",stat="identity",aes(fill=daly_scen)) +
    scale_fill_brewer(palette = "Dark2", name="DALY Scenario") +
    xlab("Years since programme start") + ylab("Annual DALYs Averted") + 
    facet_grid (dur_imm + vax_eff ~ campaign + tar_age, 
                labeller = labeller(
        vax_eff = function(v) sprintf("Vac. Eff.= %s", v)
    )) +
    scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme_minimal()

ggsave(paste0(path.fig,"plot_daly_1.png"), plot_daly_1, width = 10, height = 5, units = "in")

