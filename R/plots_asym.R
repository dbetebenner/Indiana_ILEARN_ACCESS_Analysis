###########################################################################
### R/plots_asym.R — figures for the copula-asymmetry step (09) and the
### Youden explainer. Depends on theme_signal() / save_figure() from
### R/plots.R and the config anchors.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
})

#' Radial-asymmetry surface D_n(u,v) as a diverging heatmap
#'
#' Blue = upper-tail ("signal corner") dominance; red = lower-tail
#' ("noise corner") dominance; grey ~ radial symmetry. A perfectly
#' symmetric copula (t / Frank / Gaussian, the fitted families) would be
#' flat grey everywhere.
plot_copula_asymmetry_surface <- function(surf_dt, year_label = "2021-2025 primary") {
    rng <- max(abs(surf_dt$D), na.rm = TRUE)
    ggplot(surf_dt, aes(x = u, y = v, fill = D)) +
        geom_raster(interpolate = TRUE) +
        geom_contour(aes(z = D), color = "grey30", linewidth = 0.2, bins = 10) +
        scale_fill_gradient2(
            low = "#1f6feb", mid = "grey96", high = "#cf222e", midpoint = 0,
            limits = c(-rng, rng),
            labels = function(x) sprintf("%+.3f", x)
        ) +
        annotate("text", x = 0.15, y = 0.11, label = "noise corner\n(both low)",
            size = 3.1, color = "grey20", lineheight = 0.9) +
        annotate("text", x = 0.85, y = 0.90, label = "signal corner\n(both high)",
            size = 3.1, color = "grey20", lineheight = 0.9) +
        coord_fixed(expand = FALSE) +
        labs(
            title = "The dependence is radially ASYMMETRIC",
            subtitle = sprintf("D(u,v) = empirical copula minus its reflection, %s", year_label),
            x = paste(WIDA_AXIS_LABEL, "rank (u)"),
            y = paste(ILEARN_AXIS_LABEL_ELA, "rank (v)"),
            fill = "D(u,v)",
            caption = "Not flat = not radially symmetric: the both-high corner couples more tightly than the both-low corner."
        ) +
        theme_signal()
}

#' Upper- vs lower-tail dependence functions lambda_U(t), lambda_L(t)
#'
#' Under radial symmetry these coincide. Here the upper (signal-corner)
#' curve sits above the lower (noise-corner) curve all the way into the
#' tail: both-high pairs co-occur more than both-low pairs. The shaded
#' band is the radial asymmetry the symmetric families erase.
plot_tail_dependence <- function(tail_dt, year_label = "2021-2025 primary") {
    w <- dcast(tail_dt, t ~ tail, value.var = "lambda")
    setnames(w, c("t", "lower", "upper"))
    ggplot() +
        geom_ribbon(data = w, aes(x = t, ymin = lower, ymax = upper),
            fill = "#1f6feb", alpha = 0.12) +
        geom_line(data = tail_dt, aes(x = t, y = lambda, color = tail),
            linewidth = 1) +
        scale_color_manual(values = c("lower (noise corner)" = "#8b949e",
            "upper (signal corner)" = "#1f6feb")) +
        scale_x_reverse() +
        annotate("text", x = 0.47, y = Inf, vjust = 1.5, hjust = 0,
            size = 3, color = "grey45", label = "<- toward the tail") +
        labs(
            title = "Upper-tail coupling outruns lower-tail coupling",
            subtitle = sprintf("Tail-dependence functions, %s. Equal curves = radial symmetry.", year_label),
            x = "tail depth t  (smaller t = deeper into the tail)",
            y = expression("tail concordance  " * lambda(t)),
            color = "Tail",
            caption = "lambda_L(t)=P(V<=t|U<=t), lambda_U(t)=P(V>=1-t|U>=1-t). t / Frank / Gaussian force these equal (t) or zero."
        ) +
        theme_signal() +
        theme(legend.position = "bottom")
}

#' Scalar asymmetry indices by year (primary window + 2026 ranks)
plot_copula_asymmetry_stats <- function(stats_long) {
    ggplot(stats_long, aes(x = YEAR, y = value, group = metric)) +
        geom_hline(yintercept = 0, color = "grey75", linewidth = 0.4) +
        geom_line(color = "#1f6feb", linewidth = 0.6) +
        geom_point(color = "#1f6feb", size = 2) +
        facet_wrap(~ metric, scales = "free_y", ncol = 2) +
        labs(
            title = "Exchangeable, but radially asymmetric -- and strengthening",
            subtitle = "By year, pooled over grades 3-8. Zero = the symmetric-copula null. 2026 both scales reset (ranks only).",
            x = "Year", y = NULL,
            caption = "Exchangeability ~ 0 (margins swap cleanly); radial / tail asymmetry is an order of magnitude larger and rises 2021-2025."
        ) +
        theme_signal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Youden explainer: ROC geometry + sensitivity/specificity/J across cuts
#'
#' @param cell data.table for ONE (year, grade, outcome) with columns
#'   cut, sens, spec, youden.
#' @param label Human label for the cell (e.g. "2025, grade 5").
plot_youden_explainer <- function(cell, label = "") {
    suppressPackageStartupMessages(library(patchwork))
    d <- cell[is.finite(sens) & is.finite(spec)]
    setorder(d, cut)
    d[, fpr := 1 - spec]
    opt <- d[which.max(youden)]

    ## Panel A -- ROC curve with the Youden segment (J = vertical gap to chance).
    pA <- ggplot(d, aes(x = fpr, y = sens)) +
        geom_abline(slope = 1, intercept = 0, color = "grey70",
            linetype = "dashed") +
        geom_path(color = "#1f6feb", linewidth = 0.8) +
        geom_point(color = "#1f6feb", size = 1, alpha = 0.5) +
        geom_segment(data = opt, aes(x = fpr, xend = fpr, y = fpr, yend = sens),
            color = "#cf222e", linewidth = 1) +
        geom_point(data = opt, aes(x = fpr, y = sens), color = "#cf222e",
            size = 3) +
        annotate("text", x = opt$fpr + 0.04, y = mean(c(opt$fpr, opt$sens)),
            hjust = 0, size = 3.4, color = "#cf222e",
            label = sprintf("J = %.2f\nat cut %.1f", opt$youden, opt$cut)) +
        annotate("text", x = 0.62, y = 0.52, angle = 45, size = 3,
            color = "grey45", label = "chance (J = 0)") +
        coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
        labs(subtitle = "ROC: J is the height above chance",
            x = "1 - specificity  (false-positive rate)",
            y = "sensitivity  (true-positive rate)") +
        theme_signal()

    ## Panel B -- sens, spec, J vs cut.
    long <- rbindlist(list(
        d[, .(cut, value = sens, metric = "sensitivity")],
        d[, .(cut, value = spec, metric = "specificity")],
        d[, .(cut, value = youden, metric = "Youden J")]
    ))
    pB <- ggplot(long, aes(x = cut, y = value, color = metric)) +
        geom_vline(xintercept = WIDA_EXIT_PROVISIONAL, linetype = "dashed",
            color = "#d29922", linewidth = 0.4) +
        geom_vline(xintercept = WIDA_EXIT_AUTO, linetype = "dashed",
            color = "#cf222e", linewidth = 0.4) +
        geom_vline(xintercept = opt$cut, linetype = "dotted",
            color = "grey30", linewidth = 0.5) +
        geom_line(linewidth = 0.8) +
        scale_color_manual(values = c(sensitivity = "#1f6feb",
            specificity = "#57606a", `Youden J` = "#cf222e")) +
        labs(subtitle = "Each cut is a classifier; J peaks at the optimum",
            x = paste(WIDA_AXIS_LABEL, "cut"), y = NULL, color = NULL) +
        theme_signal() +
        theme(legend.position = "bottom")

    (pA | pB) +
        patchwork::plot_annotation(
            title = "Youden's J: choosing the cut that best separates the groups",
            subtitle = sprintf("Classifier: WIDA >= cut predicts ILEARN proficiency.  %s", label),
            theme = theme_signal()
        )
}
