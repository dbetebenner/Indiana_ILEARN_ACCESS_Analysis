###########################################################################
### R/plots.R — ggplot2 figure helpers. One claim per figure, N in the
### caption, WIDA always labeled "Overall Composite" (never "Reading").
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
})

WIDA_AXIS_LABEL <- "WIDA ACCESS Overall Composite"
ILEARN_AXIS_LABEL_ELA <- "ILEARN ELA"

#' Shared minimal theme for the pipeline's figures
theme_signal <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
        theme(
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(size = base_size - 2, color = "grey40"),
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold")
        )
}

#' Save a figure in PDF + PNG under FIGURES_DIR
#'
#' @param plot A ggplot object.
#' @param basename File stem (no extension).
#' @param width,height Inches.
save_figure <- function(plot, basename, width = 9, height = 6) {
    pdf_path <- file.path(FIGURES_DIR, paste0(basename, ".pdf"))
    png_path <- file.path(FIGURES_DIR, paste0(basename, ".png"))
    ggsave(pdf_path, plot, width = width, height = height)
    ggsave(png_path, plot, width = width, height = height, dpi = 150)
    message(sprintf("[plots] wrote %s.{pdf,png}", basename))
    invisible(pdf_path)
}

#' Heatmap of paired N by year x grade
plot_n_heatmap <- function(n_dt) {
    ggplot(n_dt, aes(x = GRADE, y = YEAR, fill = N)) +
        geom_tile(color = "white") +
        geom_text(aes(label = scales::comma(N)), size = 3) +
        scale_fill_viridis_c(option = "mako", labels = scales::comma) +
        labs(
            title = "Paired ILEARN ELA x WIDA ACCESS students",
            subtitle = "Same-year, same-grade matches",
            x = "Grade", y = "Year", fill = "N pairs",
            caption = "Source: Indiana ILEARN + WIDA ACCESS long files (aggregate counts)."
        ) +
        theme_signal()
}

#' Pseudo-observation scatter faceted by grade for one year
plot_pobs_facets <- function(pairs_year, year_label, max_points_per_grade = 2500) {
    n_total <- nrow(pairs_year)
    sampled <- pairs_year[is.finite(U_WIDA) & is.finite(V_ILEARN)]
    sampled <- sampled[, {
        if (.N <= max_points_per_grade) .SD
        else .SD[sample.int(.N, max_points_per_grade)]
    }, by = GRADE]
    ggplot(sampled, aes(x = U_WIDA, y = V_ILEARN)) +
        geom_point(alpha = 0.08, size = 0.4) +
        geom_smooth(method = "loess", se = FALSE, color = "#1f6feb",
            linewidth = 0.6, formula = y ~ x) +
        facet_wrap(~ GRADE, labeller = label_both) +
        coord_fixed() +
        labs(
            title = sprintf("Rank dependence by grade, %s", year_label),
            x = paste(WIDA_AXIS_LABEL, "(within-cell rank)"),
            y = paste(ILEARN_AXIS_LABEL_ELA, "(within-cell rank)"),
            caption = sprintf("Pseudo-observations. N = %s pairs.", scales::comma(n_total))
        ) +
        theme_signal()
}

#' tau(u) curves by grade with policy vertical lines
#'
#' @param curve_dt data.table: GRADE, u_center, tau, n plus U-scale
#'   positions of the 4.3 and 5.0 anchors (u_provisional, u_auto).
plot_local_tau <- function(curve_dt, anchors_dt, year_label) {
    p <- ggplot(curve_dt[is.finite(tau)], aes(x = u_center, y = tau)) +
        geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
        geom_line(color = "#1f6feb", linewidth = 0.7) +
        facet_wrap(~ GRADE, labeller = label_both)
    if (!is.null(anchors_dt) && nrow(anchors_dt) > 0) {
        p <- p +
            geom_vline(data = anchors_dt, aes(xintercept = u_provisional),
                linetype = "dashed", color = "#d29922") +
            geom_vline(data = anchors_dt, aes(xintercept = u_auto),
                linetype = "dashed", color = "#cf222e")
    }
    p +
        labs(
            title = sprintf("Local rank dependence tau(u), %s", year_label),
            subtitle = "Amber = 4.3 provisional; red = 5.0 auto-exit (on WIDA rank scale)",
            x = paste(WIDA_AXIS_LABEL, "percentile (within-cell rank)"),
            y = "Kendall's tau in moving window",
            caption = "Windowed tau of ILEARN ELA vs WIDA rank. Rise location = empirical signal threshold."
        ) +
        theme_signal()
}

#' ILEARN percentile distribution by WIDA level (boxplots), one grade panel
plot_ilearn_by_wida_level <- function(pairs_dt, year_label) {
    d <- pairs_dt[!is.na(WIDA_LEVEL) & is.finite(ILEARN_PCTILE)]
    counts <- d[, .(N = .N), by = .(GRADE, WIDA_LEVEL)]
    ggplot(d, aes(x = WIDA_LEVEL, y = ILEARN_PCTILE)) +
        geom_boxplot(outlier.size = 0.3, fill = "#dbeafe") +
        facet_wrap(~ GRADE, labeller = label_both) +
        labs(
            title = sprintf("ILEARN ELA percentile by WIDA level, %s", year_label),
            x = paste(WIDA_AXIS_LABEL, "achievement level"),
            y = "ILEARN ELA percentile (within grade x year)",
            caption = "Boxplots by WIDA level. Cells below the level-slice N gate are omitted."
        ) +
        theme_signal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#' Exit-criterion agreement (Youden J) across WIDA cuts by grade
plot_exit_youden <- function(agree_dt, year_label) {
    ggplot(agree_dt[is.finite(youden)], aes(x = cut, y = youden)) +
        geom_line(color = "#1f6feb", linewidth = 0.7) +
        geom_vline(xintercept = WIDA_EXIT_PROVISIONAL, linetype = "dashed", color = "#d29922") +
        geom_vline(xintercept = WIDA_EXIT_AUTO, linetype = "dashed", color = "#cf222e") +
        facet_wrap(~ GRADE, labeller = label_both) +
        labs(
            title = sprintf("Agreement of WIDA cut with ILEARN proficiency, %s", year_label),
            subtitle = "Youden's J across candidate Overall cuts; amber 4.3, red 5.0",
            x = paste(WIDA_AXIS_LABEL, "cut"),
            y = "Youden's J (sensitivity + specificity - 1)",
            caption = "Higher J = better separation of ILEARN-proficient from not."
        ) +
        theme_signal()
}

#' Heatmap of inferred exiters by year t x last-WIDA band
plot_exiter_n <- function(n_dt) {
    d <- n_dt[role == "exiter"]
    ggplot(d, aes(x = WIDA_EXIT_BAND, y = YEAR_T, fill = N)) +
        geom_tile(color = "white") +
        geom_text(aes(label = scales::comma(N)), size = 3) +
        scale_fill_viridis_c(option = "mako", labels = scales::comma) +
        labs(
            title = "Inferred exiters: ILEARN + WIDA at t, ILEARN only at t+1",
            subtitle = "Testing-pattern exit, not an official IDOE roster",
            x = paste("Last", WIDA_AXIS_LABEL, "band at t"),
            y = "Year t", fill = "N exiters",
            caption = "Grades 3-7 at t. <4.3 disappearances are not treated as policy exits."
        ) +
        theme_signal()
}

#' ILEARN percentile before vs after inferred exit, primary window
plot_exiter_percentile <- function(sum_dt) {
    d <- sum_dt[window == "primary" & role == "exiter" & !is.na(WIDA_EXIT_BAND)]
    long <- rbindlist(list(
        d[, .(WIDA_EXIT_BAND, time = "t (still tested)",
              pctile = ilearn_pctile_t, n = n)],
        d[, .(WIDA_EXIT_BAND, time = "t+1 (ILEARN only)",
              pctile = ilearn_pctile_t1, n = n)]
    ))
    ggplot(long, aes(x = WIDA_EXIT_BAND, y = pctile, color = time, group = time)) +
        geom_point(aes(size = n), position = position_dodge(width = 0.3)) +
        geom_line(position = position_dodge(width = 0.3), linewidth = 0.6) +
        geom_hline(yintercept = 50, color = "grey60", linewidth = 0.4) +
        scale_size_continuous(range = c(2, 6), labels = scales::comma) +
        labs(
            title = "ILEARN ELA percentile before and after inferred exit",
            subtitle = "Primary window 2021-2025. Horizontal line = 50th percentile of the grade",
            x = paste("Last", WIDA_AXIS_LABEL, "band at t"),
            y = "Median ILEARN ELA percentile",
            color = NULL, size = "N",
            caption = "Testing-pattern exit. Same students at t and t+1."
        ) +
        theme_signal()
}

#' Exit-year ILEARN SGP: exiters vs stayers, faceted by last-WIDA band
plot_exiter_sgp <- function(sgp_dt, never_el_median = NA_real_) {
    d <- sgp_dt[role %in% c("exiter", "stayer") & window == "primary" &
            !is.na(WIDA_EXIT_BAND)]
    p <- ggplot(d, aes(x = role, y = sgp_median, fill = role)) +
        geom_col(width = 0.6) +
        geom_hline(yintercept = 50, color = "grey40", linewidth = 0.4) +
        facet_wrap(~ WIDA_EXIT_BAND) +
        scale_fill_manual(values = c(exiter = "#1f6feb", stayer = "#8b949e")) +
        labs(
            title = "Exit-year ILEARN ELA SGP (growth in the first year without ACCESS)",
            subtitle = "Primary window. Dashed line = never-EL median SGP at t+1; solid = 50",
            x = NULL, y = "Median ILEARN SGP at t+1", fill = NULL,
            caption = "Testing-pattern exit vs still-served stayers in the same last-WIDA band."
        ) +
        theme_signal() +
        theme(legend.position = "bottom")
    if (is.finite(never_el_median)) {
        p <- p + geom_hline(yintercept = never_el_median, linetype = "dashed",
            color = "#cf222e", linewidth = 0.5)
    }
    p
}

#' Lagged P(ILEARN proficient at t+1) vs last WIDA PL
#'
#' Empirical local-window rates plus the copula conditional curve.
plot_lagged_p_prof <- function(scan_dt, copula_dt, never_el_p = NA_real_) {
    scan <- scan_dt[window == "primary" & is.finite(p_local)]
    cop <- copula_dt[window == "primary" & is.finite(p_copula) & is.finite(wida_pl)]
    p <- ggplot() +
        geom_hline(yintercept = LAGGED_P_TARGET, color = "grey40", linewidth = 0.4) +
        geom_vline(xintercept = c(WIDA_EXIT_PROVISIONAL, WIDA_EXIT_AUTO),
            linetype = "dashed", color = c("#8b5cf6", "#1f6feb"), linewidth = 0.5)
    if (is.finite(never_el_p)) {
        p <- p + geom_hline(yintercept = never_el_p, linetype = "dotted",
            color = "#cf222e", linewidth = 0.5)
    }
    if (nrow(scan) > 0) {
        p <- p + geom_point(data = scan, aes(x = cut, y = p_local, size = n_local),
            alpha = 0.35, color = "#374151")
    }
    if (nrow(cop) > 0) {
        p <- p + geom_line(data = cop, aes(x = wida_pl, y = p_copula, color = GRADE),
            linewidth = 0.7)
    }
    p +
        scale_size_continuous(range = c(1.2, 5), labels = scales::comma) +
        labs(
            title = "P(ILEARN ELA proficient at t+1 | WIDA ACCESS Overall at t)",
            subtitle = "Primary window. Solid = copula conditional; points = local empirical window. Horizontal = 0.50; dotted = never-EL P(proficient).",
            x = paste(WIDA_AXIS_LABEL, "proficiency level at t"),
            y = "P(ILEARN proficient at t+1)",
            color = "ILEARN grade at t+1", size = "N (local)",
            caption = "A 50-50 content-test cut is a contrast, not an exit recommendation. Native English speakers are not all ILEARN-proficient."
        ) +
        theme_signal() +
        theme(legend.position = "bottom")
}

#' Fifty-fifty WIDA cuts by grade (copula vs local empirical)
plot_lagged_fifty_fifty <- function(cut_dt) {
    d <- cut_dt[window == "primary" & method %in% c("copula", "empirical_local")]
    ggplot(d, aes(x = GRADE, y = wida_cut, color = method, group = method)) +
        geom_hline(yintercept = c(WIDA_EXIT_PROVISIONAL, WIDA_EXIT_AUTO),
            linetype = "dashed", color = "grey70", linewidth = 0.4) +
        geom_point(aes(size = n), position = position_dodge(width = 0.25)) +
        geom_line(position = position_dodge(width = 0.25), linewidth = 0.5) +
        scale_size_continuous(range = c(2, 6), labels = scales::comma) +
        labs(
            title = "WIDA Overall cut at which P(ILEARN proficient next year) = 0.50",
            subtitle = "Primary window. Dashed lines = Indiana 4.3 and 5.0 anchors.",
            x = "ILEARN grade at t+1",
            y = paste(WIDA_AXIS_LABEL, "cut at t"),
            color = "Method", size = "N",
            caption = "Documented contrast only. Predicting ILEARN proficiency is not a proxy for English access."
        ) +
        theme_signal() +
        theme(legend.position = "bottom")
}
