###########################################################################
### 05_exit_criterion.R — map the empirical transition onto Indiana's exit
### policy. For each candidate WIDA cut, measure agreement of "WIDA >= cut"
### with ILEARN proficiency (and with percentile >= 25 / 50 as sensitivity
### analyses). Compare the 4.3 provisional and 5.0 auto-exit anchors with
### the data-driven change-point, by grade and by year. Answers Q1-Q2.
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[05] Exit-criterion analysis.")

pairs <- readRDS(PAIRS_CACHE_PATH)
pairs[, ILEARN_PCTILE_GE25 := ILEARN_PCTILE >= 25]
pairs[, ILEARN_PCTILE_GE50 := ILEARN_PCTILE >= 50]

outcomes <- list(
    proficient   = "ILEARN_PROFICIENT",
    pctile_ge25  = "ILEARN_PCTILE_GE25",
    pctile_ge50  = "ILEARN_PCTILE_GE50"
)

### --- Agreement across cuts by year x grade x outcome ----------------------
cells <- unique(pairs[, .(YEAR, YEAR_STRATUM, GRADE)])
agree_list <- list()
for (i in seq_len(nrow(cells))) {
    yr <- cells$YEAR[i]; gr <- cells$GRADE[i]; st <- cells$YEAR_STRATUM[i]
    sub <- pairs[YEAR == yr & GRADE == gr]
    if (nrow(sub) < MIN_N_COPULA_CELL) next
    for (onm in names(outcomes)) {
        ocol <- outcomes[[onm]]
        if (all(is.na(sub[[ocol]]))) next
        rows <- rbindlist(lapply(WIDA_CUT_GRID, function(c0) {
            exit_agreement(sub$WIDA_PL, sub[[ocol]], c0)
        }))
        rows[, `:=`(YEAR = yr, GRADE = gr, YEAR_STRATUM = st, outcome = onm)]
        agree_list[[length(agree_list) + 1L]] <- rows
    }
}
agreement <- rbindlist(agree_list, fill = TRUE)
if (nrow(agreement) > 0) {
    write_output(round_numeric(agreement, 4), "exit_agreement_by_cut.csv")

    ### --- Optimal (max-Youden) cut per cell x outcome ----------------------
    optimal <- agreement[is.finite(youden),
        .SD[which.max(youden)], by = .(YEAR, GRADE, YEAR_STRATUM, outcome)]
    optimal <- optimal[, .(YEAR, YEAR_STRATUM, GRADE, outcome,
        best_cut = cut, youden, sens, spec, n)]
    write_output(round_numeric(optimal, 4), "exit_optimal_cut.csv")

    ### --- Compare policy anchors vs the data-driven optimum ----------------
    ### Youden at the fixed 4.3 and 5.0 anchors, next to the max-Youden cut.
    anchor_rows <- agreement[outcome == "proficient" &
        abs(cut - WIDA_EXIT_PROVISIONAL) < 1e-6 |
        outcome == "proficient" & abs(cut - WIDA_EXIT_AUTO) < 1e-6]
    if (nrow(anchor_rows) > 0) {
        anchor_wide <- dcast(anchor_rows, YEAR + GRADE ~ cut,
            value.var = "youden")
        write_output(round_numeric(anchor_wide, 4), "exit_anchor_youden.csv")
    }

    ### --- Q1/Q2 summary: is there a number, same by grade, stable by year? -
    q_summary <- optimal[outcome == "proficient", .(
        n_cells = .N,
        best_cut_median = median(best_cut, na.rm = TRUE),
        best_cut_min = min(best_cut, na.rm = TRUE),
        best_cut_max = max(best_cut, na.rm = TRUE),
        best_cut_sd = sd(best_cut, na.rm = TRUE)
    ), keyby = YEAR_STRATUM]
    write_output(round_numeric(q_summary, 3), "exit_q1q2_summary.csv")

    ### Grade consistency within the primary window.
    grade_consistency <- optimal[outcome == "proficient" & YEAR_STRATUM == "primary",
        .(best_cut_median = median(best_cut, na.rm = TRUE),
          best_cut_sd = sd(best_cut, na.rm = TRUE),
          n_years = uniqueN(YEAR)),
        keyby = GRADE]
    write_output(round_numeric(grade_consistency, 3), "exit_grade_consistency.csv")

    ### --- Figures ----------------------------------------------------------
    for (yr in YEAR_STRATA$primary) {
        sub <- agreement[YEAR == yr & outcome == "proficient"]
        if (nrow(sub) == 0) next
        save_figure(plot_exit_youden(sub, yr), sprintf("05_exit_youden_%s", yr))
    }
} else {
    message("[05] No cells met the N gate; skipping exit outputs.")
}

message("[05] Exit-criterion analysis complete.")
