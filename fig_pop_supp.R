suppressPackageStartupMessages({
    require(qs)
    require(data.table)
    require(ggplot2)
    require(patchwork)
})

.args <- if (interactive()) sprintf(c(
    "fitd_combined.qs", "~/Dropbox/Covid-WHO-vax/outputs/figures/SI_pop.png"
)) else commandArgs(trailingOnly = TRUE)

pop <- qread(.args[1])[[1]]$par$pop[[1]]

sizes.dt <- data.table(age = factor(pop$group_names, levels = pop$group_names, ordered = TRUE), pop = pop$size)

size.p <- ggplot(sizes.dt) +
    aes(x = age, y = pop / 1000) +
    geom_col(fill = "#cc3366") +
    coord_flip(expand = FALSE, clip = "off") +
    labs(x = NULL, y = "Population (thousands)") +
    theme_minimal()

cm_data <- rbindlist(mapply(function(m, n) {
    data <- data.table(reshape2::melt(m))
    names(data) <- c("From", "To", "Contacts")
    data[, element := n ]
    data
}, m=pop$matrices, n=names(pop$matrices), SIMPLIFY = FALSE))
cm_data[log10(Contacts) < -4, Contacts := NA_real_]

m.p <- ggplot(cm_data) +
    facet_wrap(~element, nrow = 2, ncol = 2) +
    geom_raster(aes(x = To, y = From, fill = Contacts)) +
    scale_fill_viridis_c(
        trans = "log", breaks = c(0.01, 0.1, 1, 10), guide = "legend", na.value = "grey"
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(fill = "white", colour = "white")
    )

playout <- "
AA
BB
BB
"

res <- size.p + m.p + plot_layout(design = playout, guides = "collect") & theme(text = element_text(size = 8))

ggsave(tail(.args, 1), res, height = 6, width = 4, dpi = 600)
