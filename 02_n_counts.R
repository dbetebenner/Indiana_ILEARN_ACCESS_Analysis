###########################################################################
### 02_n_counts.R — the N tables the analysis asked for: universe vs
### paired counts, counts by WIDA level and policy band, and the never-EL
### reference denominator. All aggregate; cells below the suppression gate
### are flagged.
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[02] Building N-count tables.")

pairs <- readRDS(PAIRS_CACHE_PATH)

flag_suppress <- function(dt, n_col = "N") {
    dt[, suppress := get(n_col) < MIN_N_SUPPRESS]
    dt[]
}

### --- Paired N by year x grade ---------------------------------------------
n_year_grade <- pairs[, .(N = .N), keyby = .(YEAR, YEAR_STRATUM, GRADE)]
flag_suppress(n_year_grade)
write_output(n_year_grade, "n_pairs_year_grade.csv")

### --- Universe vs paired (ILEARN ELA + WIDA universes, aggregate) ----------
### Recompute the single-assessment universes to contextualize the pair rate.
il_uni <- get_ilearn("ELA")[GRADE %in% ANALYSIS_GRADES,
    .(n_ilearn = .N), keyby = .(YEAR, GRADE)]
wd_uni <- get_wida()[GRADE %in% ANALYSIS_GRADES,
    .(n_wida = .N), keyby = .(YEAR, GRADE)]
universe <- merge(il_uni, wd_uni, by = c("YEAR", "GRADE"), all = TRUE)
universe <- merge(universe,
    pairs[, .(n_paired = .N), keyby = .(YEAR, GRADE)],
    by = c("YEAR", "GRADE"), all = TRUE)
universe[is.na(n_paired), n_paired := 0L]
universe[, wida_pair_rate := round(n_paired / n_wida, 3)]
write_output(universe, "n_universe_vs_paired.csv")

### --- Paired N by WIDA level x grade x year --------------------------------
n_level <- pairs[!is.na(WIDA_LEVEL),
    .(N = .N), keyby = .(YEAR, GRADE, WIDA_LEVEL)]
flag_suppress(n_level)
write_output(n_level, "n_pairs_wida_level.csv")

### --- Paired N by WIDA policy band -----------------------------------------
n_band <- pairs[!is.na(WIDA_BAND),
    .(N = .N,
      ilearn_prof_rate = round(mean(ILEARN_PROFICIENT, na.rm = TRUE), 3)),
    keyby = .(YEAR, GRADE, WIDA_BAND)]
flag_suppress(n_band)
write_output(n_band, "n_pairs_wida_band.csv")

### --- Never-EL reference denominator ---------------------------------------
### ILEARN ELA students who are NOT current EL, as the reference cloud for
### "can access content". Aggregate counts by year x grade only.
il_ela <- get_ilearn("ELA")[GRADE %in% ANALYSIS_GRADES]
il_ela[, IS_EL := is_current_el(EL_STATUS)]
never_el <- il_ela[, .(
    n_total = .N,
    n_el = sum(IS_EL %in% TRUE),
    n_never_el = sum(IS_EL %in% FALSE),
    n_el_unknown = sum(is.na(IS_EL))
), keyby = .(YEAR, GRADE)]
write_output(never_el, "n_never_el_reference.csv")

### --- Headline totals -------------------------------------------------------
totals <- pairs[, .(
    total_pairs = .N,
    primary_pairs = sum(YEAR_STRATUM == "primary"),
    years = paste(sort(unique(YEAR)), collapse = ","),
    grades = paste(sort(unique(GRADE)), collapse = ",")
)]
write_output(totals, "n_totals.csv")

### --- N heatmap ------------------------------------------------------------
save_figure(plot_n_heatmap(n_year_grade), "02_n_heatmap", width = 8, height = 6)

message(sprintf("[02] N tables complete. Total pairs: %s.",
    scales::comma(totals$total_pairs)))
