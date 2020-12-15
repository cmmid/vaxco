library(data.table)
library(ggplot2)
library(lubridate)
library(stringr)
library(cowplot)

# set up covidm
cm_path = "~/Dropbox/nCoV/covidm/";
cm_force_rebuild = F;
cm_build_verbose = T;
cm_version = 2;
source(paste0(cm_path, "/R/covidm.R"))

load_fit = function(fit_filename, scenario_name)
{
    fit = qread(fit_filename);
    
    cm_source_backend(user_defined = fit$user_defined);
    dyn = cm_backend_sample_fit_test(cm_translate_parameters(fit$par), fit$post, 250, seed = 0);
    dyn = rbindlist(dyn);
    
    dyn = dyn[, .(scenario = scenario_name, true_deaths = sum(death_o), true_cases = sum(cases), 
        rep_deaths = obs0[2], rep_cases = obs0[3]), by = .(run, t)];
    dyn = melt(dyn, id.vars = 1:3);
    
    dyn = dyn[, as.list(quantile(value, c(0.025, 0.5, 0.975))), by = .(t, scenario, variable)];
    
    return (dyn)
}

dyn = rbind(
    load_fit("fit_sindh.qs", "No waning"),
    load_fit("fit_sindh_waning_1.0.qs", "1 year protection"),
    load_fit("fit_sindh_waning_2.5.qs", "2.5 year protection"),
    load_fit("fit_sindh_waning_5.0.qs", "5 year protection")
);
dyn[, scenario := factor(scenario, unique(scenario))];
dyn[, date := ymd("2020-01-01") + t];
dyn[variable %like% "^true",   epi := "true"];
dyn[variable %like% "^rep",    epi := "reported"];
dyn[variable %like% "cases$",  ind := "cases"];
dyn[variable %like% "deaths$", ind := "deaths"];

data = fread("sindh_data.csv");
data = melt(data, id.vars = 1:2, variable.name = "ind");

ccol = "lightslateblue";
dcol = "coral";

th = theme_cowplot(font_size = 10) + theme(strip.background = element_blank(), strip.placement = "outside")

plot_cases = ggplot(dyn[ind == "cases" & epi == "reported"]) +
    geom_ribbon(aes(date, ymin = `2.5%`, ymax = `97.5%`, group = variable), fill = ccol) +
    geom_line(aes(date, y = `50%`, group = variable), size = 0.25) +
    geom_point(aes(date, value), data = data[ind == "cases"], size = 0.1) +
    facet_wrap(~scenario, ncol = 1, scales = "free_x", strip.position = "left") +
    scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
    labs(x = NULL, y = NULL, title = "Cases") +
    th

plot_deaths = ggplot(dyn[ind == "deaths" & epi == "reported"]) +
    geom_ribbon(aes(date, ymin = `2.5%`, ymax = `97.5%`, group = variable), fill = dcol) +
    geom_line(aes(date, y = `50%`, group = variable), size = 0.25) +
    geom_point(aes(date, value), data = data[ind == "deaths"], size = 0.1) +
    facet_wrap(~scenario, ncol = 1, scales = "free_x") +
    scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
    labs(x = NULL, y = NULL, title = "Deaths") +
    th + theme(strip.text = element_blank())

dyn[, epi := factor(epi, c("true", "reported"))]

plot_compare = ggplot() +
    geom_line(data = dyn, aes(date, y = `2.5%`,  group = variable, colour = ind, linetype = epi), size = 0.25) +
    geom_line(data = dyn, aes(date, y = `97.5%`, group = variable, colour = ind, linetype = epi), size = 0.25) +
    facet_wrap(~scenario, ncol = 1, scales = "free_x") +
    scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
    scale_y_log10(limits = c(1, NA), breaks = c(1, 100, 10000), labels = c(1, 100, 10000)) +
    scale_linetype_manual(values = c("true" = "solid", "reported" = "dotted")) +
    scale_colour_manual(values = c("cases" = ccol, "deaths" = dcol), guide = FALSE) +
    labs(x = NULL, y = NULL, title = "Reporting", colour = NULL, linetype = NULL) +
    th + theme(legend.position = c(0.65, 1.0), legend.key.height = unit(0.2, "pt"), strip.text = element_blank())

cowplot::plot_grid(plot_cases, plot_deaths, plot_compare, nrow = 1, rel_widths = c(1, 1, 1))
