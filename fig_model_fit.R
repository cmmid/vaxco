suppressPackageStartupMessages({
    require(data.table)
    require(ggplot2)
    require(patchwork)
    require(qs)
})

.debug <- "~/Dropbox/Covid-WHO-vax/outputs"
.args <- if (interactive()) sprintf(c(
    "%s/sim_model.rds",
    "sindh_data.csv",
    "fitd_combined.qs",
    "%s/figures/model_fit.png"
), .debug)

all.dyn <- readRDS(.args[1])

obs.dt = melt(fread(.args[2]), id.vars = 1:2, variable.name = "ind");
#' assert: no missing dates
obs.dt[order(date), rolling := frollmean(value, 7), by = ind ]

comb.fits <- qread(.args[3])

mdlcols <- c("black", scales::hue_pal()(4))
scns <- c("reported", "Inf","5.0","2.5","1.0")

inc.p <- function(
    dt, ylab, tarind = dt[,unique(ind)], mdlcol = "dodgerblue"
) ggplot(dt) +
    aes(date, value, group = run) +
    geom_line(aes(color="model"), alpha = 0.02) +
    geom_point(aes(color="reported", group = NULL), obs.dt[ind == tarind], alpha = 0.2) +
    geom_line(aes(y=rolling, color="reported", group = NULL), obs.dt[ind == tarind]) +
    scale_x_date(
        name = NULL, date_breaks = "months", date_labels = "%b"
    ) + scale_y_log10(name = ylab) +
    scale_color_manual(name = NULL, values = c(model = mdlcol, reported="black")) +
    coord_cartesian(expand = FALSE, clip = "off")

pcases <- inc.p(all.dyn[
    scenario == scns[5] & epi == "reported" & ind == "cases" & between(date, "2020-04-15", "2020-09-15")
], "New Cases", mdlcol = mdlcols[5])

pdeaths <- inc.p(all.dyn[
    scenario == scns[5] & epi == "reported" & ind == "deaths" & between(date, "2020-04-15", "2020-09-15")
], "New Deaths", mdlcol = mdlcols[5])

pext <- function(dt, ylab, tarind = dt[,unique(ind)]) ggplot(dt) +
    aes(date, value, group = interaction(run, scenario), color = scenario) +
    geom_line(alpha = 0.02) +
    geom_point(aes(color="reported", group = NULL), obs.dt[ind == tarind], alpha = 0.2) +
    geom_line(aes(y=rolling, color="reported", group = NULL), obs.dt[ind == tarind]) +
    scale_x_date(
        name = NULL, date_breaks = "months", date_labels = "%b"
    ) + scale_y_log10(name = ylab) +
    scale_color_manual(
        name = NULL,
        breaks = scns,
        values = mdlcols
    ) +
    coord_cartesian(expand = FALSE, ylim = c(1,NA))

pext.cases <- pext(all.dyn[
  epi == "reported" & ind == "cases" & date > "2020-04-15"
], "New Cases")

pext.deaths <- pext(all.dyn[
    epi == "reported" & ind == "deaths" & date > "2020-04-15"
], "New Deaths")

pop <- sum(comb.fits[[1]]$par$pop[[1]]$size)
sero <- fread("sindh_sero.csv")
sero[, value := positive/total ]

bino <- function(ci, pos, tot) as.data.table(t(mapply(
    function(x, n, p=x/n) binom.test(x, n, p, conf.level = ci)$conf.int,
    x = pos, n = tot
)))

sero[, c("lo95","hi95") := bino(.95, positive, total) ]
sero[, mid := start + (end - start)/2 ]

sero.p <- function(dt, mdlcol = "dodgerblue") ggplot(dt) +
    aes(date, value/pop, group = run) +
    geom_line(aes(color="model"), alpha = 0.02) +
    geom_linerange(
        aes(
            y = value, x = NULL,
            xmin = start, xmax = end,
            color = "reported", group = NULL,
            alpha = assessment
        ), data = sero
    ) +
    geom_linerange(
        aes(
            x = mid, y = NULL,
            ymin = lo95, ymax = hi95,
            color = "reported", group = NULL,
            alpha = assessment
        ), data = sero
    ) + scale_color_manual(guide = "none", values = c(model=mdlcol, reported="black")) +
    scale_alpha_manual(NULL, values = c(good = 0.9, bad = 0.2), guide = "none") +
    scale_y_continuous("Seropositivity", breaks = c(0:3)/4) +
    scale_x_date(
        name = NULL, date_breaks = "months", date_labels = "%b"
    ) + 
    coord_cartesian(ylim = c(0, 0.75), expand = FALSE, clip = "off")

sero.ext <- function(dt) ggplot(dt) +
    aes(date, value/pop, group = interaction(run,scenario), color = scenario) +
    geom_line(alpha = 0.02) +
    geom_linerange(
        aes(
            y = value, x = NULL,
            xmin = start, xmax = end,
            color = "reported", group = NULL,
            alpha = assessment
        ), data = sero
    ) +
    geom_linerange(
        aes(
            x = mid, y = NULL,
            ymin = lo95, ymax = hi95,
            color = "reported", group = NULL,
            alpha = assessment
        ), data = sero
    ) +
    scale_alpha_manual(NULL, values = c(good = 0.9, bad = 0.2), guide = "none") +
    scale_y_continuous("Seropositivity", breaks = c(0:3)/4) +
    scale_x_date(
        name = NULL, date_breaks = "months", date_labels = "%b"
    ) +
    scale_color_manual(
        name = NULL,
        breaks = scns,
        values = mdlcols
    ) +
    coord_cartesian(ylim = c(0, 0.75), expand = FALSE)

psero <- sero.p(all.dyn[
    scenario == scns[5] & outcome == "totR" & between(date, "2020-04-15", "2020-09-15")
], mdlcols[5])

pext.sero <- sero.ext(all.dyn[
    outcome == "totR" & date > "2020-04-15"
])

res <- ((guide_area() / pcases / pdeaths / psero) +
    plot_layout(guides = "collect", heights = c(.1, 1, 1, 1)) +
    plot_annotation(tag_levels = "A")) &
    theme_minimal() & theme(
        legend.direction = "horizontal",
        panel.border=element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid.minor = element_blank()
    )

res.ext <- ((guide_area() / pext.cases / pext.deaths / pext.sero) +
    plot_layout(guides = "collect", heights = c(.1, 1, 1, 1)) +
    plot_annotation(tag_levels = "A")) &
    theme_minimal() &
    theme(
        legend.direction = "horizontal",
        panel.border=element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid.minor = element_blank()
    )

ggsave(tail(.args, 1), res, height = 6, width = 5, dpi = 600)

ggsave(gsub("\\.","_ext.",tail(.args, 1)), res.ext, height = 6, width = 6, dpi = 600)
