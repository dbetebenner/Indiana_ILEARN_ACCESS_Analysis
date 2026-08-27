###########################################################################
### 09_copula_asymmetry.R — the ASYMMETRY of the dependence structure.
###
### Steps 03-08 fit and read EXCHANGEABLE, RADIALLY SYMMETRIC copulas
### (t / Frank / Gaussian). By construction those families set
### lambda_lower = lambda_upper and C(u,v) = hat-C(u,v): they cannot see
### a copula whose lower-left corner (both tests low -- the WIDA floor)
### behaves differently from its upper-right corner (both high). But that
### corner-difference is exactly the signal-vs-noise hypothesis. This step
### measures the departure from symmetry directly and asks whether it is
### large, stable, and localized where the exit debate sits.
###
### Outputs (aggregate, rank-based -- no re-fitting, no student ids):
###   outputs/asym_by_year_grade.csv          per-cell scalar indices
###   outputs/asym_summary_by_year.csv         pooled-by-year indices
###   outputs/asym_surface_primary.csv         D(u,v) grid, primary window
###   outputs/asym_tail_functions_primary.csv  lambda_L(t), lambda_U(t)
###   figures/09_copula_asymmetry_surface.*    radial-asymmetry map
###   figures/09_copula_asymmetry_tails.*      tail-dependence functions
###   figures/09_copula_asymmetry_stats.*      indices by year
###   figures/05b_youden_explainer.*           Youden J teaching figure
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[09] Copula asymmetry (radial / tail / exchangeability).")

pairs <- readRDS(PAIRS_CACHE_PATH)
GRID19 <- seq(0.05, 0.95, by = 0.05)

### --- (1) Per (year, grade) scalar indices ---------------------------------
scal <- pairs[, copula_asymmetry_scalars(U_WIDA, V_ILEARN, GRID19),
    by = .(YEAR, YEAR_STRATUM, GRADE)]
scal[, suppress := n < MIN_N_LEVEL_SLICE]
write_output(round_numeric(scal, 4), "asym_by_year_grade.csv")

### --- (2) Pooled-by-year indices (primary + 2026 ranks) --------------------
yrs_keep <- c(YEAR_STRATA$primary, YEAR_STRATA$ilearn_2026)
byyear <- pairs[YEAR %in% yrs_keep,
    copula_asymmetry_scalars(U_WIDA, V_ILEARN, GRID19), by = .(YEAR)]
write_output(round_numeric(byyear, 4), "asym_summary_by_year.csv")

### --- (3) Radial-asymmetry surface, pooled primary window ------------------
prim <- pairs[YEAR_STRATUM == "primary" & is.finite(U_WIDA) & is.finite(V_ILEARN)]
surf <- radial_asymmetry_surface(prim$U_WIDA, prim$V_ILEARN,
    grid = seq(0.025, 0.975, by = 0.025))
write_output(round_numeric(surf, 5), "asym_surface_primary.csv")
save_figure(plot_copula_asymmetry_surface(surf),
    "09_copula_asymmetry_surface", width = 7.4, height = 6.6)

### --- (4) Tail-dependence functions, pooled primary ------------------------
tail_dt <- tail_dependence_curve(prim$U_WIDA, prim$V_ILEARN)
write_output(round_numeric(tail_dt, 4), "asym_tail_functions_primary.csv")
save_figure(plot_tail_dependence(tail_dt),
    "09_copula_asymmetry_tails", width = 8.4, height = 5.6)

### --- (5) Indices by year figure -------------------------------------------
metric_labs <- c(
    radial_index   = "Radial asymmetry index  (sup|D|)",
    tail_asymmetry = "Tail asymmetry  (lambda_U - lambda_L)",
    corner_tau_gap = "Corner tau gap  (high-high minus low-low)",
    exch_index     = "Exchangeability index  (sup|C(u,v)-C(v,u)|)"
)
stats_long <- melt(byyear[, .(YEAR, radial_index, tail_asymmetry,
    corner_tau_gap, exch_index)], id.vars = "YEAR",
    variable.name = "metric", value.name = "value")
stats_long[, metric := factor(metric_labs[as.character(metric)], levels = metric_labs)]
save_figure(plot_copula_asymmetry_stats(stats_long),
    "09_copula_asymmetry_stats", width = 9, height = 6)

### --- (6) Youden explainer figure ------------------------------------------
### Reads the agreement table written by step 05. Falls back to a live
### computation if 05 has not been run this session.
agree_path <- file.path(OUTPUTS_DIR, "exit_agreement_by_cut.csv")
agree <- if (file.exists(agree_path)) {
    fread(agree_path)
} else {
    message("[09] exit_agreement_by_cut.csv missing; computing a cell live.")
    sub <- pairs[YEAR == "2025" & GRADE == "5"]
    rbindlist(lapply(WIDA_CUT_GRID, function(c0)
        exit_agreement(sub$WIDA_PL, sub$ILEARN_PROFICIENT, c0)))[
        , `:=`(YEAR = 2025L, GRADE = 5L, outcome = "proficient")]
}
pick <- agree[YEAR == 2025 & GRADE == 5 & outcome == "proficient"]
if (nrow(pick) == 0) {
    pick <- agree[outcome == "proficient"][YEAR == max(YEAR)]
    pick <- pick[GRADE == pick$GRADE[1]]
}
if (nrow(pick) > 0) {
    save_figure(plot_youden_explainer(pick,
        sprintf("%s, grade %s", pick$YEAR[1], pick$GRADE[1])),
        "05b_youden_explainer", width = 11, height = 5.4)
}

message("[09] Copula asymmetry complete.")
