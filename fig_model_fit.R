suppressPackageStartupMessages({
    require(data.table)
    require(qs)
    require(ggplot2)
    require(lubridate)
    require(patchwork)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "fit_combined.qs", "sindh_data.csv",
    "../covidm-vaxco", "%s/figures/model_fit.png"
), .debug)

# set up covidm
cm_path = tail(.args, 2)[1];
cm_force_rebuild = F;
cm_build_verbose = T;
cm_force_shared = T;
cm_version = 2;
source(file.path(cm_path, "R", "covidm.R"))

comb.fits <- qread(.args[1])

all.dyn <- rbindlist(mapply(function(ft, nm) {
    dyn <- rbindlist(
        cm_backend_sample_fit_test(cm_translate_parameters(ft$par), ft$post, 250, seed = 0)
    )[, .(true_deaths = sum(death_o), true_cases = sum(cases), 
          rep_deaths = obs0[2], rep_cases = obs0[3]), by = .(run, t) ]
    
    melt(dyn[order(t),
        .(t,
            ct_deaths = cumsum(true_deaths), ct_cases = cumsum(true_cases),
            tr_deaths = cumsum(rep_deaths), tr_cases = cumsum(rep_cases)
        ),
        keyby = run
    ], id.vars = c("run","t"), variable.name = "outcome")[, scenario := nm ]
}, ft = comb.fits, nm = names(comb.fits), SIMPLIFY = FALSE))

qtile <- function(
    v, ps = c(lo95=0.025, lo50=0.25, md=0.5, hi50=0.75, hi95=0.975),
    withMean = c("mn", NA),
    fmt = "%s",
    na.rm = TRUE
) {
    qs <- quantile(v, probs = ps, na.rm = na.rm)
    names(qs) <- sprintf(fmt, names(ps))
    if (!is.na(withMean[1])) {
        mn <- mean(v)
        names(mn) <- sprintf(fmt, withMean[1])
        qs <- c(qs, mn)
    }
    as.list(qs)
}

qs.dt <- melt(
    all.dyn[, qtile(value), by=.(scenario, outcome, t)],
    id.vars = c("scenario","outcome","t"),
    variable.name = "quantile"
)

qs.dt[order(t), dvalue := c(value[1], diff(value)), by=.(scenario, outcome, quantile)]
scnlvls <- names(comb.fits)
qs.dt[, scenario := factor(scenario, levels = c(scnlvls[-1],scnlvls[1]), ordered = TRUE)]

#' TODO could get fancy with tstrsplit?
qs.dt[outcome %like% "^ct",   epi := "true"];
qs.dt[outcome %like% "^tr",    epi := "reported"];
qs.dt[outcome %like% "cases$",  ind := "cases"];
qs.dt[outcome %like% "deaths$", ind := "deaths"];

qs.dt[, date := ymd("2020-01-01") + t ];

# dyn = rbind(
#     load_fit("fit_sindh.qs", "No waning"),
#     load_fit("fit_sindh_waning_1.0.qs", "1 year protection"),
#     load_fit("fit_sindh_waning_2.5.qs", "2.5 year protection"),
#     load_fit("fit_sindh_waning_5.0.qs", "5 year protection")
# );

qs.wd <- dcast(qs.dt, scenario + epi + ind + date ~ quantile, value.var = "dvalue")

obs.dt = melt(fread(.args[2]), id.vars = 1:2, variable.name = "ind");
#' assert: no missing dates
obs.dt[order(date), rolling := frollmean(value, 7), by = ind ]

plotter <- function(
    dt, filt = expression(1:.N), obs = obs.dt,
    locol = "dodgerblue", hicol = "firebrick",
    ylim = c(100, 10000)
) ggplot(dt[eval(filt)]) +
    aes(date, alpha = scenario) +
    geom_point(
        aes(y = value, color = "obs"),
        data = obs.dt[eval(filt)], alpha = 0.1,
        show.legend = FALSE
    ) +
    geom_line(aes(y = hi95, color = "hi", linetype = "95"), size = 0.25) +
    geom_line(aes(y = hi50, color = "hi", linetype = "50"), size = 0.25) +
    geom_line(aes(y = lo95, color = "lo", linetype = "95"), size = 0.25) +
    geom_line(aes(y = lo50, color = "lo", linetype = "50"), size = 0.25) +
    geom_line(aes(y = md, color = "md", linetype = "0")) +
    geom_line(aes(y = rolling, alpha = "observed", color = "obs", linetype = "obs"), data = obs.dt[eval(filt)]) +
    facet_grid(ind ~ ., scales = "free_y", switch = "y") +
    scale_y_log10() +
    scale_linetype_manual(
        name = "Simulation\nQuantile",
        breaks = c("0","50","95"),
        labels = c("0"="median","50"="50% IQR","95"="95% IQR"),
        values = c(obs="solid", `0`="solid", `50`="dashed", `95`="dotted"),
        guide = guide_legend(
            override.aes = list(size = c(1, 0.25, 0.25))
        )
    ) +
    scale_color_manual(
        "Measures",
        labels = c(lo="Lower Qs", hi = "Upper Qs", md = "Median", obs = "Reported\n(7 day mean)"),
        values = c(lo=locol, hi = hicol, md = "grey45", obs = "black")
    ) +
    scale_alpha_manual(
        "Assumed Natural Waning",
        values = c(observed = 1, `1.0` = 1, `2.5`=0.7, `5.0`=0.5, `Inf`=0.3),
        guide = "none"
    ) +
    scale_x_date(
        name = NULL,
        date_breaks = "months", date_label = "%b",
    ) + theme_minimal() + theme(
        strip.placement = "outside", axis.title.y = element_blank()
    ) +
    coord_cartesian(ylim = ylim, xlim = c(ymd("2020-05-01"), NA))

fit.p <- (plotter(
    qs.wd[epi == "reported" & scenario == "1.0"], filt = expression(ind == "cases")
) / plotter(
    qs.wd[epi == "reported" & scenario == "1.0"], filt = expression(ind == "deaths"), ylim = c(1,100)
)) + plot_layout(nrow = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(tail(.args, 1), fit.p, width = 9, height = 6, dpi = 300)

# ccol = "lightslateblue";
# dcol = "coral";
# 
# th = theme_cowplot(font_size = 10) + theme(strip.background = element_blank(), strip.placement = "outside", plot.title = element_text(size = 10))
# 
# plot_cases = ggplot(dyn[ind == "cases" & epi == "reported"]) +
#     geom_ribbon(aes(date, ymin = `2.5%`, ymax = `97.5%`, group = variable), fill = ccol) +
#     geom_line(aes(date, y = `50%`, group = variable), size = 0.25) +
#     geom_point(aes(date, value), data = data[ind == "cases"], size = 0.1) +
#     facet_wrap(~scenario, ncol = 1, scales = "free_x", strip.position = "left") +
#     scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
#     labs(x = NULL, y = NULL, title = "Cases") +
#     th
# 
# plot_deaths = ggplot(dyn[ind == "deaths" & epi == "reported"]) +
#     geom_ribbon(aes(date, ymin = `2.5%`, ymax = `97.5%`, group = variable), fill = dcol) +
#     geom_line(aes(date, y = `50%`, group = variable), size = 0.25) +
#     geom_point(aes(date, value), data = data[ind == "deaths"], size = 0.1) +
#     facet_wrap(~scenario, ncol = 1, scales = "free_x") +
#     scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
#     labs(x = NULL, y = NULL, title = "Deaths") +
#     th + theme(strip.text = element_blank())

# dyn[, epi := factor(epi, c("true", "reported"))]
# 
# plot_compare = ggplot() +
#     geom_line(data = dyn, aes(date, y = `2.5%`,  group = variable, colour = ind, linetype = epi), size = 0.25) +
#     geom_line(data = dyn, aes(date, y = `97.5%`, group = variable, colour = ind, linetype = epi), size = 0.25) +
#     facet_wrap(~scenario, ncol = 1, scales = "free_x") +
#     scale_x_date(date_breaks = "1 month", date_label = "%b", limits = c(ymd("2020-03-01"), NA)) +
#     scale_y_log10(limits = c(1, NA), breaks = c(1, 100, 10000), labels = c(1, 100, 10000)) +
#     scale_linetype_manual(values = c("true" = "solid", "reported" = "dotted")) +
#     scale_colour_manual(values = c("cases" = ccol, "deaths" = dcol), guide = FALSE) +
#     labs(x = NULL, y = NULL, title = "Reporting", colour = NULL, linetype = NULL) +
#     th + theme(legend.position = c(0.65, 1.0), legend.key.height = unit(0.2, "pt"), strip.text = element_blank())

# cowplot::plot_grid(plot_cases, plot_deaths, plot_compare, nrow = 1, rel_widths = c(1, 1, 1))
