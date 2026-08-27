###########################################################################
### 06_extensions.R — additional ways to read the same paired data for an
### exit-criterion question. These sit after the ELA core, not instead of
### it. All outputs are aggregates.
###
###   1. Math contrast          — same pairing on ILEARN Mathematics
###   2. Never-EL reference     — paired ILEARN vs never-EL ILEARN cloud
###   3. Lagged pairing         — WIDA year t-1 vs ILEARN year t
###   4. Panel / time-to-exit   — ILEARN signal at first 4.3 / 5.0 crossing
###   5. Growth (secondary)     — ILEARN SGP vs WIDA SGP
###   6. 2026 dual-scale        — ranks on native vs OLD-scale scores
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[06] Extensions.")

pairs_ela <- readRDS(PAIRS_CACHE_PATH)
ilearn_ela <- get_ilearn("ELA")
wida <- get_wida()

### -----------------------------------------------------------------------
### 1. Math contrast
### -----------------------------------------------------------------------
message("[06] Math contrast.")
ilearn_math <- get_ilearn("MATHEMATICS")
pairs_math <- build_pairs(ilearn_math, wida, grades = ANALYSIS_GRADES)
add_pobs(pairs_math, wida_col = "WIDA_SCORE", ilearn_col = "ILEARN_SCORE")
pairs_math[, ILEARN_PCTILE := percentile_within(ILEARN_SCORE), by = .(YEAR, GRADE)]
pairs_math[, ILEARN_PROFICIENT := ilearn_is_proficient(ILEARN_LEVEL)]

math_corr <- pairs_math[, {
    m <- dependence_measures(U_WIDA, V_ILEARN)
    .(n = m$n, kendall = m$kendall, spearman = m$spearman)
}, keyby = .(YEAR, YEAR_STRATUM, GRADE)]
math_corr[, content := "MATHEMATICS"]
ela_corr <- pairs_ela[, {
    m <- dependence_measures(U_WIDA, V_ILEARN)
    .(n = m$n, kendall = m$kendall, spearman = m$spearman)
}, keyby = .(YEAR, YEAR_STRATUM, GRADE)]
ela_corr[, content := "ELA"]
contrast <- rbindlist(list(ela_corr, math_corr), use.names = TRUE, fill = TRUE)
write_output(round_numeric(contrast, 4), "ext_ela_vs_math_dependence.csv")

math_by_level <- pairs_math[!is.na(WIDA_LEVEL), {
    nn <- .N
    .(
        n = nn,
        kendall = if (nn >= MIN_N_LEVEL_SLICE) kendall_tau(U_WIDA, V_ILEARN) else NA_real_,
        ilearn_pctile_median = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        p_ilearn_proficient = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_
    )
}, keyby = .(YEAR, GRADE, WIDA_LEVEL)]
math_by_level[, content := "MATHEMATICS"]
write_output(round_numeric(math_by_level, 4), "ext_math_by_wida_level.csv")

### -----------------------------------------------------------------------
### 2. Never-EL reference
### -----------------------------------------------------------------------
message("[06] Never-EL reference.")
ilearn_ela[, IS_EL := is_current_el(EL_STATUS)]
never <- ilearn_ela[GRADE %in% ANALYSIS_GRADES & IS_EL %in% FALSE]
never_ref <- never[, .(
    n_never_el = .N,
    never_el_median = median(ILEARN_SCORE, na.rm = TRUE),
    never_el_q25 = quantile(ILEARN_SCORE, 0.25, na.rm = TRUE),
    never_el_q75 = quantile(ILEARN_SCORE, 0.75, na.rm = TRUE)
), keyby = .(YEAR, GRADE)]

paired_by_level <- pairs_ela[!is.na(WIDA_LEVEL), .(
    n_paired = .N,
    paired_median = median(ILEARN_SCORE, na.rm = TRUE),
    paired_pctile_median = median(ILEARN_PCTILE, na.rm = TRUE)
), keyby = .(YEAR, GRADE, WIDA_LEVEL)]

never_gap <- merge(paired_by_level, never_ref, by = c("YEAR", "GRADE"), all.x = TRUE)
never_gap[, gap_vs_never_el := paired_median - never_el_median]
never_gap[, suppress := n_paired < MIN_N_LEVEL_SLICE]
write_output(round_numeric(never_gap, 2), "ext_never_el_gap.csv")

### Primary-window figure: median ILEARN percentile by WIDA level vs 50.
prim_gap <- never_gap[YEAR %in% YEAR_STRATA$primary & !suppress]
if (nrow(prim_gap) > 0) {
    p_gap <- ggplot(prim_gap, aes(x = WIDA_LEVEL, y = paired_pctile_median,
            group = YEAR, color = YEAR)) +
        geom_hline(yintercept = 50, color = "grey60", linewidth = 0.4) +
        geom_line() + geom_point(size = 1.4) +
        facet_wrap(~ GRADE, labeller = label_both) +
        labs(
            title = "Paired EL ILEARN ELA percentile by WIDA level",
            subtitle = "Horizontal line = never-EL median (50th percentile of the grade x year)",
            x = paste(WIDA_AXIS_LABEL, "achievement level"),
            y = "Median ILEARN ELA percentile of paired EL students",
            caption = "Primary window 2021-2025. Convergence toward 50 is a second reading of access."
        ) +
        theme_signal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    save_figure(p_gap, "06_never_el_gap_primary", width = 10, height = 7)
}

### -----------------------------------------------------------------------
### 3. Lagged pairing (WIDA t-1, ILEARN t, grade advanced by 1)
### -----------------------------------------------------------------------
message("[06] Lagged pairing.")
wida_lag <- wida[GRADE %in% as.character(2:7),
    .(ID, YEAR_WIDA = YEAR, GRADE_WIDA = GRADE,
      WIDA_SCORE, WIDA_PL, WIDA_LEVEL)]
wida_lag[, YEAR := as.character(as.integer(YEAR_WIDA) + 1)]
wida_lag[, GRADE := as.character(as.integer(GRADE_WIDA) + 1)]
il_now <- ilearn_ela[GRADE %in% ANALYSIS_GRADES,
    .(ID, YEAR, GRADE, ILEARN_SCORE, ILEARN_LEVEL, EL_STATUS)]
setkey(il_now, ID, YEAR, GRADE)
setkey(wida_lag, ID, YEAR, GRADE)
lagged <- merge(il_now, wida_lag, by = c("ID", "YEAR", "GRADE"))
lagged[, YEAR_STRATUM := classify_year(YEAR)]
lagged[, U_WIDA := pobs_vec(WIDA_SCORE), by = .(YEAR, GRADE)]
lagged[, V_ILEARN := pobs_vec(ILEARN_SCORE), by = .(YEAR, GRADE)]
lagged[, ILEARN_PCTILE := percentile_within(ILEARN_SCORE), by = .(YEAR, GRADE)]
lagged[, ILEARN_PROFICIENT := ilearn_is_proficient(ILEARN_LEVEL)]

lag_corr <- lagged[, {
    m <- dependence_measures(U_WIDA, V_ILEARN)
    .(n = m$n, kendall = m$kendall, spearman = m$spearman)
}, keyby = .(YEAR, YEAR_STRATUM, GRADE)]
write_output(round_numeric(lag_corr, 4), "ext_lagged_dependence.csv")

lag_by_level <- lagged[!is.na(WIDA_LEVEL), {
    nn <- .N
    .(
        n = nn,
        kendall = if (nn >= MIN_N_LEVEL_SLICE) kendall_tau(U_WIDA, V_ILEARN) else NA_real_,
        ilearn_pctile_median = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        p_ilearn_proficient = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_
    )
}, keyby = .(YEAR, GRADE, WIDA_LEVEL)]
write_output(round_numeric(lag_by_level, 4), "ext_lagged_by_wida_level.csv")

### -----------------------------------------------------------------------
### 4. Panel / time-to-exit
### -----------------------------------------------------------------------
message("[06] Panel / time-to-exit.")
## Students observed in 2+ primary/2026 years on the ELA pairs.
panel <- pairs_ela[YEAR_STRATUM %in% c("primary", "ilearn_2026")]
n_years <- panel[, .(n_years = uniqueN(YEAR)), by = ID]
multi <- panel[ID %in% n_years[n_years >= 2, ID]]
setorder(multi, ID, YEAR)

first_cross <- function(dt, cut, label) {
    dt[, first_ge := YEAR[which(WIDA_PL >= cut)[1]], by = ID]
    crossed <- dt[!is.na(first_ge)]
    at <- crossed[YEAR == first_ge, .(
        n = .N,
        ilearn_pctile_at = median(ILEARN_PCTILE, na.rm = TRUE),
        p_proficient_at = mean(ILEARN_PROFICIENT, na.rm = TRUE)
    )]
    prior_rows <- crossed[YEAR == as.character(as.integer(first_ge) - 1)]
    prior <- if (nrow(prior_rows) == 0L) {
        data.table(n_prior = 0L, ilearn_pctile_prior = NA_real_,
            p_proficient_prior = NA_real_)
    } else {
        prior_rows[, .(
            n_prior = .N,
            ilearn_pctile_prior = median(ILEARN_PCTILE, na.rm = TRUE),
            p_proficient_prior = mean(ILEARN_PROFICIENT, na.rm = TRUE)
        )]
    }
    cbind(data.table(cut = cut, label = label), at, prior)
}

panel_43 <- first_cross(copy(multi), WIDA_EXIT_PROVISIONAL, "4.3 provisional")
panel_50 <- first_cross(copy(multi), WIDA_EXIT_AUTO, "5.0 auto-exit")
panel_summary <- rbindlist(list(panel_43, panel_50), fill = TRUE)
write_output(round_numeric(panel_summary, 3), "ext_panel_first_crossing.csv")

### -----------------------------------------------------------------------
### 5. Growth (secondary): ILEARN SGP vs WIDA SGP
### -----------------------------------------------------------------------
message("[06] Growth (SGP vs SGP; secondary).")
sgp_pairs <- pairs_ela[is.finite(ILEARN_SGP) & is.finite(WIDA_SGP)]
if (nrow(sgp_pairs) >= MIN_N_COPULA_CELL) {
    sgp_corr <- sgp_pairs[, {
        m <- dependence_measures(ILEARN_SGP, WIDA_SGP)
        .(n = m$n, kendall = m$kendall, spearman = m$spearman)
    }, keyby = .(YEAR, YEAR_STRATUM, GRADE)]
    write_output(round_numeric(sgp_corr, 4), "ext_sgp_vs_sgp.csv")
} else {
    message("[06] Too few paired SGPs; skipping growth extension.")
}

### -----------------------------------------------------------------------
### 6. 2026 dual-scale robustness
### -----------------------------------------------------------------------
message("[06] 2026 dual-scale robustness.")
p26 <- pairs_ela[YEAR == "2026"]
if (nrow(p26) >= MIN_N_COPULA_CELL) {
    native <- p26[, {
        m <- dependence_measures(WIDA_SCORE, ILEARN_SCORE)
        .(scale = "native_combined_LONG", n = m$n,
          kendall = m$kendall, spearman = m$spearman)
    }, keyby = GRADE]
    old <- p26[is.finite(WIDA_SCORE_OLD) & is.finite(ILEARN_SCORE_OLD), {
        m <- dependence_measures(WIDA_SCORE_OLD, ILEARN_SCORE_OLD)
        .(scale = "old_scale", n = m$n,
          kendall = m$kendall, spearman = m$spearman)
    }, keyby = GRADE]
    dual <- rbindlist(list(native, old), use.names = TRUE, fill = TRUE)
    write_output(round_numeric(dual, 4), "ext_2026_dual_scale.csv")
} else {
    message("[06] 2026 cell too small; skipping dual-scale check.")
}

message("[06] Extensions complete.")
