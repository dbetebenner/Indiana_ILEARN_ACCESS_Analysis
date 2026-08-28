###########################################################################
### 03_global_dependence.R — per (year, grade) global dependence of ILEARN
### ELA on WIDA Overall: rank correlations, confirmatory copula fits
### (t / Frank / Gaussian), tail dependence, and pseudo-observation
### scatter figures for the primary window.
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[03] Global dependence per year x grade.")

pairs <- readRDS(PAIRS_CACHE_PATH)

### --- Rank correlations by year x grade ------------------------------------
corr_rows <- pairs[, {
    m <- dependence_measures(U_WIDA, V_ILEARN, dcor = FALSE)
    .(n = m$n, kendall = m$kendall, spearman = m$spearman, pearson = m$pearson)
}, keyby = .(YEAR, YEAR_STRATUM, GRADE)]
corr_rows[, suppress := n < MIN_N_LEVEL_SLICE]
write_output(round_numeric(corr_rows, 4), "dep_rank_correlations.csv")

## Heatmap of Kendall's tau by year x grade (primary window), N in-cell.
save_figure(plot_tau_heatmap(corr_rows), "03_tau_heatmap", width = 8.4, height = 5.2)

### --- Copula fits by year x grade ------------------------------------------
cells <- unique(pairs[, .(YEAR, GRADE)])
fit_list <- vector("list", nrow(cells))
for (i in seq_len(nrow(cells))) {
    yr <- cells$YEAR[i]; gr <- cells$GRADE[i]
    sub <- pairs[YEAR == yr & GRADE == gr & is.finite(U_WIDA) & is.finite(V_ILEARN)]
    if (nrow(sub) < MIN_N_COPULA_CELL) next
    u <- as.matrix(sub[, .(U_WIDA, V_ILEARN)])
    fits <- fit_copulas(u)
    fits[, `:=`(YEAR = yr, GRADE = gr)]
    fit_list[[i]] <- fits
}
copula_fits <- rbindlist(fit_list, fill = TRUE)
if (nrow(copula_fits) > 0) {
    setcolorder(copula_fits, c("YEAR", "GRADE", "family"))
    write_output(round_numeric(copula_fits, 4), "dep_copula_fits.csv")

    ## Best-family frequency table (across all cells).
    best_freq <- copula_fits[best_aic == TRUE, .(n_cells = .N), keyby = family]
    best_freq[, pct := round(100 * n_cells / sum(n_cells), 1)]
    write_output(best_freq, "dep_copula_best_family_freq.csv")
} else {
    message("[03] No cells met the copula N gate; skipping copula outputs.")
}

### --- Figures for the primary window ---------------------------------------
for (yr in YEAR_STRATA$primary) {
    sub <- pairs[YEAR == yr & is.finite(U_WIDA) & is.finite(V_ILEARN)]
    if (nrow(sub) < MIN_N_LEVEL_SLICE) next
    save_figure(plot_pobs_facets(sub, yr), sprintf("03_pobs_facets_%s", yr))
}

message("[03] Global dependence complete.")
