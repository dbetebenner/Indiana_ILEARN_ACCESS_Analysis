###########################################################################
### 04_local_dependence.R — the signal-vs-noise core. Three views by grade
### and year: (1) statistics by WIDA level, (2) moving-window tau(u) along
### the WIDA rank axis with a change-point, and (3) a threshold scan over
### candidate Overall cuts. Independence at low WIDA plus a stable rise
### near 4.3 / 5.0 supports the hypothesis; a flat tau(u) refutes it.
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[04] Local / threshold dependence.")

pairs <- readRDS(PAIRS_CACHE_PATH)

### --- (1) Statistics by WIDA level ------------------------------------------
by_level <- pairs[!is.na(WIDA_LEVEL), {
    nn <- .N
    .(
        n = nn,
        kendall = if (nn >= MIN_N_LEVEL_SLICE) kendall_tau(U_WIDA, V_ILEARN) else NA_real_,
        ilearn_pctile_median = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        ilearn_pctile_sd = if (nn >= MIN_N_LEVEL_SLICE) sd(ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        ilearn_floor_share = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PCTILE <= 5, na.rm = TRUE) else NA_real_,
        p_ilearn_proficient = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_
    )
}, keyby = .(YEAR, GRADE, WIDA_LEVEL)]
by_level[, suppress := n < MIN_N_LEVEL_SLICE]
write_output(round_numeric(by_level, 4), "local_by_wida_level.csv")

### --- (2) Moving-window tau(u) + change-point ------------------------------
curve_list <- list()
cp_list <- list()
cells <- unique(pairs[, .(YEAR, YEAR_STRATUM, GRADE)])
for (i in seq_len(nrow(cells))) {
    yr <- cells$YEAR[i]; gr <- cells$GRADE[i]; st <- cells$YEAR_STRATUM[i]
    sub <- pairs[YEAR == yr & GRADE == gr]
    if (nrow(sub) < MIN_N_COPULA_CELL) next
    curve <- local_tau_curve(sub$U_WIDA, sub$V_ILEARN)
    curve[, `:=`(YEAR = yr, GRADE = gr, YEAR_STRATUM = st)]
    curve_list[[length(curve_list) + 1L]] <- curve

    cp <- tau_changepoint(curve)
    pl_break <- u_break_to_pl(cp$u_break, sub)
    cp_list[[length(cp_list) + 1L]] <- data.table(
        YEAR = yr, GRADE = gr, YEAR_STRATUM = st,
        u_break = cp$u_break, wida_pl_at_break = pl_break,
        slope_left = cp$slope_left, slope_right = cp$slope_right,
        method = cp$method
    )
}
tau_curves <- rbindlist(curve_list, fill = TRUE)
changepoints <- rbindlist(cp_list, fill = TRUE)
if (nrow(tau_curves) > 0) write_output(round_numeric(tau_curves, 4), "local_tau_curves.csv")
if (nrow(changepoints) > 0) write_output(round_numeric(changepoints, 4), "local_tau_changepoints.csv")

### --- (3) Threshold scan (below vs at/above each candidate cut) -------------
scan_list <- list()
for (i in seq_len(nrow(cells))) {
    yr <- cells$YEAR[i]; gr <- cells$GRADE[i]; st <- cells$YEAR_STRATUM[i]
    sub <- pairs[YEAR == yr & GRADE == gr]
    if (nrow(sub) < MIN_N_COPULA_CELL) next
    sc <- threshold_scan(sub)
    sc[, `:=`(YEAR = yr, GRADE = gr, YEAR_STRATUM = st)]
    scan_list[[length(scan_list) + 1L]] <- sc
}
threshold_scans <- rbindlist(scan_list, fill = TRUE)
if (nrow(threshold_scans) > 0) {
    write_output(round_numeric(threshold_scans, 4), "local_threshold_scan.csv")
}

### --- Figures ---------------------------------------------------------------
### Map the 4.3 / 5.0 anchors onto the WIDA rank (U) scale per year x grade
### so the vertical lines are comparable to tau(u).
anchor_u <- function(sub) {
    d <- sub[is.finite(U_WIDA) & is.finite(WIDA_PL)]
    if (nrow(d) < MIN_N_LEVEL_SLICE) return(c(NA_real_, NA_real_))
    up <- mean(d$WIDA_PL < WIDA_EXIT_PROVISIONAL, na.rm = TRUE)
    ua <- mean(d$WIDA_PL < WIDA_EXIT_AUTO, na.rm = TRUE)
    c(up, ua)
}
for (yr in c(YEAR_STRATA$primary, YEAR_STRATA$ilearn_2026)) {
    cur <- tau_curves[YEAR == yr]
    if (nrow(cur) == 0) next
    anchors <- pairs[YEAR == yr, {
        au <- anchor_u(.SD)
        .(u_provisional = au[1], u_auto = au[2])
    }, by = GRADE]
    save_figure(plot_local_tau(cur, anchors, yr), sprintf("04_local_tau_%s", yr))

    sub_year <- pairs[YEAR == yr]
    if (nrow(sub_year) >= MIN_N_LEVEL_SLICE) {
        save_figure(plot_ilearn_by_wida_level(sub_year, yr),
            sprintf("04_ilearn_by_level_%s", yr), height = 6.5)
    }
}

message("[04] Local dependence complete.")
