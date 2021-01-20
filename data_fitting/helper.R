#
# S-SHAPED ASCERTAINMENT CURVE AND RELATED FUNCTIONS
#
asc = function(x, y0, y1, s0, s1)
{
    xx = s0 + x * (s1 - s0)
    h0 = exp(s0) / (1 + exp(s0))
    h1 = exp(s1) / (1 + exp(s1))
    h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0)
    y0 + (y1 - y0) * h
}

asc_lo = function(x, y0, y_lo, s0, s1)
{
    y1 = odds(y0, exp(y_lo))
    asc(x, y0, y1, s0, s1)
}

plot_asc = function(y0, y1, s0, s1, y_lo = NULL, x_start = 0, x_span = 1, x_divs = 100, colour = NA)
{
    # Create storage for quantiles
    data = data.table(x = seq(0, 1, by = 1 / (x_divs)));
    
    # Create y1 if y_lo is supplied
    if (!is.null(y_lo)) {
        y1 = odds(y0, exp(y_lo))
    }
    
    # Move along xdivs, finding quantiles
    i = 1;
    for (x_pt in data$x) {
        sample_pts = asc(x_pt, y0, y1, s0, s1)
        data[i, q2.5 :=  quantile(sample_pts, 0.025)];
        data[i, q25 :=   quantile(sample_pts, 0.25)];
        data[i, q50 :=   quantile(sample_pts, 0.5)];
        data[i, q75 :=   quantile(sample_pts, 0.75)];
        data[i, q97.5 := quantile(sample_pts, 0.975)];
        i = i + 1
    }
    
    # represent as ggplot elements
    list(
        geom_ribbon(data = data,     aes(x_start + x_span * x, ymin = q2.5, ymax = q97.5), alpha = 0.3, fill = colour),
        geom_ribbon(data = data, aes(x_start + x_span * x, ymin = q25, ymax = q75), alpha = 0.6, fill = colour),
        geom_line(data = data,   aes(x_start + x_span * x, q50), colour = colour)
    )
}