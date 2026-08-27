###########################################################################
### 08_lagged_proficiency.R — WIDA ACCESS Overall at t vs ILEARN ELA
### proficiency at t+1.
###
### A common cut-score recipe: find the ACCESS score at which a student
### has better than a 50-50 chance of being proficient on next year's ELA
### test, reading the joint through a copula. We run that recipe because
### it is part of the conversation. We do **not** recommend it as an exit
### rule: native English speakers are not all ILEARN-proficient, so
### content-test proficiency is not a proxy for English access.
###
### 2025->2026 is excluded from the proficiency inversion (standard-setting
### change; classifier is NA). Ranks for that window stay in the N table.
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[08] Lagged proficiency-copula (WIDA t → ILEARN t+1). Contrast only.")

ilearn <- get_ilearn("ELA")
wida   <- get_wida()
ilearn[, ILEARN_PROFICIENT := ilearn_is_proficient(ILEARN_LEVEL)]
ilearn[, IS_EL := is_current_el(EL_STATUS)]

never_el_p <- ilearn[GRADE %in% ANALYSIS_GRADES & IS_EL %in% FALSE, .(
    n_never_el = .N,
    p_never_el_proficient = mean(ILEARN_PROFICIENT, na.rm = TRUE)
), keyby = .(YEAR, GRADE)]

build_lagged <- function(year_t, year_t1, window) {
    w <- wida[YEAR == year_t & GRADE %in% LAGGED_WIDA_GRADES,
        .(ID, GRADE_T = GRADE, WIDA_SCORE, WIDA_PL, WIDA_LEVEL)]
    w[, GRADE := as.character(as.integer(GRADE_T) + 1L)]
    il <- ilearn[YEAR == year_t1 & GRADE %in% ANALYSIS_GRADES,
        .(ID, GRADE, ILEARN_SCORE, ILEARN_LEVEL, ILEARN_PROFICIENT, ILEARN_SGP)]
    setkey(w, ID, GRADE)
    setkey(il, ID, GRADE)
    dt <- merge(il, w, by = c("ID", "GRADE"))
    dt[, `:=`(YEAR_T = year_t, YEAR_T1 = year_t1, window = window)]
    dt[]
}

lagged <- rbindlist(lapply(seq_len(nrow(EXITER_TRANSITIONS)), function(i) {
    build_lagged(
        EXITER_TRANSITIONS$year_t[i],
        EXITER_TRANSITIONS$year_t1[i],
        EXITER_TRANSITIONS$window[i]
    )
}), fill = TRUE)

lagged[, U_WIDA := pobs_vec(WIDA_SCORE), by = .(YEAR_T1, GRADE)]
lagged[, V_ILEARN := pobs_vec(ILEARN_SCORE), by = .(YEAR_T1, GRADE)]

saveRDS(lagged, LAGGED_CACHE_PATH)
message(sprintf("[08] Cached %s lagged pairs to %s (gitignored).",
    scales::comma(nrow(lagged)), LAGGED_CACHE_PATH))

n_lag <- lagged[, .(N = .N), keyby = .(YEAR_T, YEAR_T1, window, GRADE)]
n_lag[, suppress := N < MIN_N_SUPPRESS]
write_output(n_lag, "lagged_n_year_grade.csv")

### --- Empirical scan: local window and cumulative P(prof | WIDA) ----------
message("[08] Empirical P(proficient at t+1) scan.")

prof_ok <- lagged[window == "primary" & !is.na(ILEARN_PROFICIENT)]

scan_list <- list()
cells <- unique(prof_ok[, .(YEAR_T, YEAR_T1, window, GRADE)])
for (i in seq_len(nrow(cells))) {
    sub <- prof_ok[YEAR_T == cells$YEAR_T[i] & GRADE == cells$GRADE[i]]
    if (nrow(sub) < MIN_N_LEVEL_SLICE) next
    rows <- rbindlist(lapply(WIDA_CUT_GRID, function(c0) {
        local <- sub[is.finite(WIDA_PL) &
            WIDA_PL >= c0 - LAGGED_LOCAL_PL_HALFWIDTH &
            WIDA_PL <  c0 + LAGGED_LOCAL_PL_HALFWIDTH]
        ge <- sub[is.finite(WIDA_PL) & WIDA_PL >= c0]
        data.table(
            cut = c0,
            n_local = nrow(local),
            p_local = if (nrow(local) >= MIN_N_SUPPRESS)
                mean(local$ILEARN_PROFICIENT) else NA_real_,
            n_ge = nrow(ge),
            p_ge = if (nrow(ge) >= MIN_N_LEVEL_SLICE)
                mean(ge$ILEARN_PROFICIENT) else NA_real_
        )
    }))
    rows[, `:=`(
        YEAR_T = cells$YEAR_T[i], YEAR_T1 = cells$YEAR_T1[i],
        window = cells$window[i], GRADE = cells$GRADE[i]
    )]
    scan_list[[i]] <- rows
}
scan <- rbindlist(scan_list, fill = TRUE)
write_output(round_numeric(scan, 4), "lagged_empirical_scan.csv")

first_cross_cut <- function(cuts, p, n, min_n) {
    ok <- is.finite(p) & n >= min_n
    if (!any(ok)) return(NA_real_)
    idx <- which(ok & p >= LAGGED_P_TARGET)
    if (length(idx) == 0L) return(NA_real_)
    cuts[idx[1]]
}

emp_cuts <- scan[, .(
    n = max(n_ge, na.rm = TRUE),
    empirical_local = first_cross_cut(cut, p_local, n_local, MIN_N_SUPPRESS),
    empirical_cumulative = first_cross_cut(cut, p_ge, n_ge, MIN_N_LEVEL_SLICE)
), keyby = .(YEAR_T, YEAR_T1, window, GRADE)]

### --- Copula conditional P(prof | U = u) ----------------------------------
message("[08] Copula conditional P(proficient at t+1 | WIDA at t).")

fit_rows <- list()
curve_rows <- list()
cut_rows <- list()
u_grid <- seq(0.04, 0.96, by = 0.02)

for (i in seq_len(nrow(cells))) {
    yr <- cells$YEAR_T[i]; yr1 <- cells$YEAR_T1[i]
    win <- cells$window[i]; gr <- cells$GRADE[i]
    sub <- prof_ok[YEAR_T == yr & GRADE == gr &
        is.finite(U_WIDA) & is.finite(V_ILEARN)]
    if (nrow(sub) < MIN_N_COPULA_CELL) next

    u <- as.matrix(sub[, .(U_WIDA, V_ILEARN)])
    best <- fit_best_copula_object(u)
    fits <- copy(best$fits)
    fits[, `:=`(YEAR_T = yr, YEAR_T1 = yr1, window = win, GRADE = gr)]
    fit_rows[[length(fit_rows) + 1L]] <- fits

    v_star <- v_star_from_proficient(sub$ILEARN_SCORE, sub$ILEARN_PROFICIENT)
    never <- never_el_p[YEAR == yr1 & GRADE == gr]
    p_never <- if (nrow(never) == 1L) never$p_never_el_proficient[1] else NA_real_

    inv <- copula_u_for_target_p(best$copula, v_star, target = LAGGED_P_TARGET)
    pl_star <- u_star_to_pl(inv$u_star, sub)

    if (!is.null(best$copula)) {
        p_curve <- copula_p_v_ge_given_u(best$copula, u_grid, v_star)
        curve_rows[[length(curve_rows) + 1L]] <- data.table(
            YEAR_T = yr, YEAR_T1 = yr1, window = win, GRADE = gr,
            family = best$family, u = u_grid, p_copula = p_curve,
            wida_pl = vapply(u_grid, function(uu) u_star_to_pl(uu, sub), numeric(1))
        )
    }

    p_43 <- mean(sub[WIDA_PL >= WIDA_EXIT_PROVISIONAL, ILEARN_PROFICIENT], na.rm = TRUE)
    p_50 <- mean(sub[WIDA_PL >= WIDA_EXIT_AUTO, ILEARN_PROFICIENT], na.rm = TRUE)
    p_43_loc <- mean(sub[
        WIDA_PL >= WIDA_EXIT_PROVISIONAL - LAGGED_LOCAL_PL_HALFWIDTH &
            WIDA_PL < WIDA_EXIT_PROVISIONAL + LAGGED_LOCAL_PL_HALFWIDTH,
        ILEARN_PROFICIENT], na.rm = TRUE)
    p_50_loc <- mean(sub[
        WIDA_PL >= WIDA_EXIT_AUTO - LAGGED_LOCAL_PL_HALFWIDTH &
            WIDA_PL < WIDA_EXIT_AUTO + LAGGED_LOCAL_PL_HALFWIDTH,
        ILEARN_PROFICIENT], na.rm = TRUE)

    cut_rows[[length(cut_rows) + 1L]] <- data.table(
        YEAR_T = yr, YEAR_T1 = yr1, window = win, GRADE = gr,
        n = nrow(sub),
        family = best$family,
        v_star = v_star,
        p_never_el_proficient = p_never,
        copula_u_star = inv$u_star,
        copula_cut = pl_star,
        copula_reached = inv$reached,
        copula_p_at_cut = inv$p_at_u_star,
        empirical_local_cut = emp_cuts[YEAR_T == yr & GRADE == gr, empirical_local],
        empirical_cumulative_cut = emp_cuts[YEAR_T == yr & GRADE == gr, empirical_cumulative],
        p_ge_4.3 = p_43,
        p_ge_5.0 = p_50,
        p_local_4.3 = p_43_loc,
        p_local_5.0 = p_50_loc
    )
}

if (length(fit_rows) > 0) {
    write_output(round_numeric(rbindlist(fit_rows, fill = TRUE), 4),
        "lagged_copula_fits.csv")
}
curve <- if (length(curve_rows) > 0) rbindlist(curve_rows, fill = TRUE) else data.table()
if (nrow(curve) > 0) {
    write_output(round_numeric(curve, 4), "lagged_copula_p_prof.csv")
}
cuts <- if (length(cut_rows) > 0) rbindlist(cut_rows, fill = TRUE) else data.table()
if (nrow(cuts) > 0) {
    write_output(round_numeric(cuts, 4), "lagged_fifty_fifty_cut.csv")
}

### Long form for the by-grade figure
if (nrow(cuts) > 0) {
    cut_long <- rbindlist(list(
        cuts[, .(YEAR_T, YEAR_T1, window, GRADE, n, method = "copula",
            wida_cut = copula_cut, reached = copula_reached)],
        cuts[, .(YEAR_T, YEAR_T1, window, GRADE, n, method = "empirical_local",
            wida_cut = empirical_local_cut, reached = is.finite(empirical_local_cut))],
        cuts[, .(YEAR_T, YEAR_T1, window, GRADE, n, method = "empirical_cumulative",
            wida_cut = empirical_cumulative_cut, reached = is.finite(empirical_cumulative_cut))]
    ), fill = TRUE)
    write_output(round_numeric(cut_long, 4), "lagged_fifty_fifty_cut_long.csv")
}

### Pooled primary-window inversion (one copula on all primary lagged pairs)
message("[08] Pooled primary-window copula.")
pool <- prof_ok[is.finite(U_WIDA) & is.finite(V_ILEARN)]
if (nrow(pool) >= MIN_N_COPULA_CELL) {
    pool[, U_WIDA := pobs_vec(WIDA_SCORE)]
    pool[, V_ILEARN := pobs_vec(ILEARN_SCORE)]
    ## Full-sample MPL on ~170k pairs is too slow; invert on a subsample
    ## (empirical P(prof) rates below still use every row).
    max_n_fit <- 12000L
    pool_fit <- if (nrow(pool) > max_n_fit) {
        pool[sample.int(.N, max_n_fit)]
    } else pool
    message(sprintf("[08] Pooled copula fit on %s of %s pairs.",
        scales::comma(nrow(pool_fit)), scales::comma(nrow(pool))))
    best_p <- fit_best_copula_object(as.matrix(pool_fit[, .(U_WIDA, V_ILEARN)]))
    v_star_p <- v_star_from_proficient(pool$ILEARN_SCORE, pool$ILEARN_PROFICIENT)
    inv_p <- copula_u_for_target_p(best_p$copula, v_star_p, target = LAGGED_P_TARGET)
    pl_p <- u_star_to_pl(inv_p$u_star, pool)
    p_never_p <- ilearn[YEAR %in% YEAR_STRATA$primary & GRADE %in% ANALYSIS_GRADES &
        IS_EL %in% FALSE, mean(ILEARN_PROFICIENT, na.rm = TRUE)]
    scan_pool <- scan[window == "primary", .(
        p = weighted.mean(p_local, n_local, na.rm = TRUE),
        n = sum(n_local, na.rm = TRUE)
    ), keyby = cut]

    pooled <- data.table(
        window = "primary",
        n = nrow(pool),
        family = best_p$family,
        v_star = v_star_p,
        p_never_el_proficient = p_never_p,
        copula_u_star = inv_p$u_star,
        copula_cut = pl_p,
        copula_reached = inv_p$reached,
        copula_p_at_cut = inv_p$p_at_u_star,
        empirical_local_cut = first_cross_cut(
            scan_pool$cut, scan_pool$p, scan_pool$n, MIN_N_LEVEL_SLICE
        ),
        p_ge_4.3 = mean(pool[WIDA_PL >= WIDA_EXIT_PROVISIONAL, ILEARN_PROFICIENT], na.rm = TRUE),
        p_ge_5.0 = mean(pool[WIDA_PL >= WIDA_EXIT_AUTO, ILEARN_PROFICIENT], na.rm = TRUE),
        p_local_4.3 = mean(pool[
            WIDA_PL >= WIDA_EXIT_PROVISIONAL - LAGGED_LOCAL_PL_HALFWIDTH &
                WIDA_PL < WIDA_EXIT_PROVISIONAL + LAGGED_LOCAL_PL_HALFWIDTH,
            ILEARN_PROFICIENT], na.rm = TRUE),
        p_local_5.0 = mean(pool[
            WIDA_PL >= WIDA_EXIT_AUTO - LAGGED_LOCAL_PL_HALFWIDTH &
                WIDA_PL < WIDA_EXIT_AUTO + LAGGED_LOCAL_PL_HALFWIDTH,
            ILEARN_PROFICIENT], na.rm = TRUE)
    )
    write_output(round_numeric(pooled, 4), "lagged_fifty_fifty_pooled.csv")
} else {
    pooled <- data.table()
    p_never_p <- NA_real_
}

### --- Figures --------------------------------------------------------------
if (nrow(scan) > 0 && nrow(curve) > 0) {
    save_figure(
        plot_lagged_p_prof(scan, curve, never_el_p = p_never_p),
        "08_lagged_p_prof_primary", width = 10, height = 6.5
    )
}
if (exists("cut_long") && nrow(cut_long) > 0) {
    save_figure(
        plot_lagged_fifty_fifty(cut_long),
        "08_lagged_fifty_fifty_by_grade", width = 9, height = 5.5
    )
}

if (exists("write_all_manifests")) write_all_manifests()

message("[08] Lagged proficiency-copula complete.")
