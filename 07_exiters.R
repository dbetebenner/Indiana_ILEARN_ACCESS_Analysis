###########################################################################
### 07_exiters.R — testing-pattern exit cohort.
###
### A student is an inferred exiter if they take ILEARN + WIDA ACCESS at
### (t, g) and ILEARN only at (t+1, g+1). That is not an official IDOE
### exit roster. <4.3 disappearances are documented, not treated as
### policy exits. Grade 8 at t cannot be followed (no grade 9 ILEARN)
### and is counted as attrition.
###
### Comparators: stayers (both tests both years), never-EL (ILEARN both
### years, no WIDA, EL flag FALSE where usable), attritors (both tests
### at t, no ILEARN at t+1).
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[07] Building inferred-exit cohort (testing-pattern, not official roster).")

ilearn <- get_ilearn("ELA")
wida   <- get_wida()

ilearn[, ILEARN_PCTILE := percentile_within(ILEARN_SCORE), by = .(YEAR, GRADE)]
ilearn[, ILEARN_PROFICIENT := ilearn_is_proficient(ILEARN_LEVEL)]
ilearn[, IS_EL := is_current_el(EL_STATUS)]

## WIDA takers by year (any grade) — used to decide "no ACCESS at t+1".
wida_ids_by_year <- unique(wida[, .(ID, YEAR)])

never_el_ref <- ilearn[GRADE %in% ANALYSIS_GRADES & IS_EL %in% FALSE, .(
    n_never_el = .N,
    never_el_median_score = median(ILEARN_SCORE, na.rm = TRUE),
    never_el_median_pctile = median(ILEARN_PCTILE, na.rm = TRUE),
    never_el_median_sgp = median(ILEARN_SGP, na.rm = TRUE)
), keyby = .(YEAR, GRADE)]

build_transition <- function(year_t, year_t1, window) {
    t_num  <- as.integer(year_t)
    t1_num <- as.integer(year_t1)

    ## Dual-tested at t, grades 3-7 (followable) and 8 (attrition only).
    at_t <- ilearn[YEAR == year_t & GRADE %in% ANALYSIS_GRADES]
    setkey(at_t, ID, YEAR, GRADE)
    wida_t <- wida[YEAR == year_t, .(ID, GRADE, WIDA_SCORE, WIDA_PL, WIDA_LEVEL)]
    setkey(wida_t, ID, GRADE)
    dual_t <- merge(at_t, wida_t, by = c("ID", "GRADE"))
    dual_t[, WIDA_EXIT_BAND := wida_exit_band(WIDA_PL)]
    dual_t[, GRADE_T1 := as.character(as.integer(GRADE) + 1L)]

    wida_t1_ids <- unique(wida_ids_by_year[YEAR == year_t1, ID])
    il_t1 <- ilearn[YEAR == year_t1,
        .(ID, GRADE,
          ILEARN_SCORE_T1 = ILEARN_SCORE,
          ILEARN_PCTILE_T1 = ILEARN_PCTILE,
          ILEARN_PROFICIENT_T1 = ILEARN_PROFICIENT,
          ILEARN_SGP_T1 = ILEARN_SGP,
          ILEARN_LEVEL_T1 = ILEARN_LEVEL,
          EL_STATUS_T1 = EL_STATUS,
          IS_EL_T1 = IS_EL)]

    ## Followable dual-tested (grades 3-7): join ILEARN at (t+1, g+1).
    follow <- dual_t[GRADE %in% EXITER_GRADES_T]
    follow <- merge(
        follow,
        il_t1,
        by.x = c("ID", "GRADE_T1"),
        by.y = c("ID", "GRADE"),
        all.x = TRUE
    )
    follow[, has_ilearn_t1 := !is.na(ILEARN_SCORE_T1)]
    follow[, has_wida_t1 := ID %in% wida_t1_ids]
    follow[, role := fifelse(
        !has_ilearn_t1, "attritor",
        fifelse(has_wida_t1, "stayer", "exiter")
    )]

    ## Grade 8 at t: no grade 9 ILEARN — all attritors.
    g8 <- dual_t[GRADE == "8"]
    if (nrow(g8) > 0) {
        g8[, `:=`(
            ILEARN_SCORE_T1 = NA_real_, ILEARN_PCTILE_T1 = NA_real_,
            ILEARN_PROFICIENT_T1 = NA, ILEARN_SGP_T1 = NA_real_,
            ILEARN_LEVEL_T1 = NA_character_, EL_STATUS_T1 = NA_character_,
            IS_EL_T1 = NA, has_ilearn_t1 = FALSE,
            has_wida_t1 = ID %in% wida_t1_ids, role = "attritor"
        )]
        follow <- rbindlist(list(follow, g8), use.names = TRUE, fill = TRUE)
    }

    follow[, `:=`(YEAR_T = year_t, YEAR_T1 = year_t1, window = window)]

    ## Never-EL reference: ILEARN both years, no WIDA either year, not EL.
    il_t_ref <- at_t[GRADE %in% EXITER_GRADES_T]
    il_t_ref[, GRADE_T1 := as.character(as.integer(GRADE) + 1L)]
    never <- merge(
        il_t_ref,
        il_t1,
        by.x = c("ID", "GRADE_T1"),
        by.y = c("ID", "GRADE"),
        all = FALSE
    )
    wida_t_ids <- unique(wida[YEAR == year_t, ID])
    never <- never[!(ID %in% wida_t_ids) & !(ID %in% wida_t1_ids)]
    never <- never[IS_EL %in% FALSE]
    if (year_t1 != "2026") never <- never[IS_EL_T1 %in% FALSE]
    never[, `:=`(
        role = "never_el",
        WIDA_SCORE = NA_real_, WIDA_PL = NA_real_, WIDA_LEVEL = NA_character_,
        WIDA_EXIT_BAND = NA,
        has_ilearn_t1 = TRUE, has_wida_t1 = FALSE,
        YEAR_T = year_t, YEAR_T1 = year_t1, window = window
    )]

    keep <- c(
        "ID", "YEAR_T", "YEAR_T1", "window", "role",
        "GRADE", "GRADE_T1", "WIDA_PL", "WIDA_LEVEL", "WIDA_EXIT_BAND",
        "ILEARN_SCORE", "ILEARN_PCTILE", "ILEARN_PROFICIENT", "ILEARN_SGP",
        "ILEARN_SCORE_T1", "ILEARN_PCTILE_T1", "ILEARN_PROFICIENT_T1",
        "ILEARN_SGP_T1", "IS_EL", "IS_EL_T1", "has_ilearn_t1", "has_wida_t1"
    )
    rbindlist(list(follow[, ..keep], never[, ..keep]), use.names = TRUE, fill = TRUE)
}

panel <- rbindlist(lapply(seq_len(nrow(EXITER_TRANSITIONS)), function(i) {
    build_transition(
        EXITER_TRANSITIONS$year_t[i],
        EXITER_TRANSITIONS$year_t1[i],
        EXITER_TRANSITIONS$window[i]
    )
}), fill = TRUE)

saveRDS(panel, EXITERS_CACHE_PATH)
message(sprintf("[07] Cached %s person-transitions to %s (gitignored).",
    scales::comma(nrow(panel)), EXITERS_CACHE_PATH))

### --- N tables -------------------------------------------------------------
n_role <- panel[, .(N = .N), keyby = .(YEAR_T, YEAR_T1, window, GRADE, role, WIDA_EXIT_BAND)]
n_role[, suppress := N < MIN_N_SUPPRESS]
write_output(n_role, "exiter_n_year_grade_band.csv")

n_totals <- panel[, .(N = .N), keyby = .(YEAR_T, window, role)]
write_output(n_totals, "exiter_n_totals.csv")

### --- Achievement before / after -------------------------------------------
message("[07] Achievement before vs after.")

ach <- panel[role %in% c("exiter", "stayer", "never_el") & has_ilearn_t1 == TRUE, {
    nn <- .N
    .(
        n = nn,
        ilearn_pctile_t = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        ilearn_pctile_t1 = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE_T1, na.rm = TRUE) else NA_real_,
        ilearn_pctile_delta = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_PCTILE_T1 - ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
        p_proficient_t = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_,
        p_proficient_t1 = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_PROFICIENT_T1, na.rm = TRUE) else NA_real_
    )
}, keyby = .(YEAR_T, YEAR_T1, window, GRADE, role, WIDA_EXIT_BAND)]

ach <- merge(ach, never_el_ref,
    by.x = c("YEAR_T", "GRADE"), by.y = c("YEAR", "GRADE"), all.x = TRUE)
ref_t1 <- copy(never_el_ref)
setnames(ref_t1,
    c("YEAR", "GRADE", "n_never_el", "never_el_median_score",
      "never_el_median_pctile", "never_el_median_sgp"),
    c("YEAR_T1", "GRADE_T1", "n_never_el_t1", "never_el_median_score_t1",
      "never_el_median_pctile_t1", "never_el_median_sgp_t1"))
ach[, GRADE_T1 := as.character(as.integer(GRADE) + 1L)]
ach <- merge(ach, ref_t1, by = c("YEAR_T1", "GRADE_T1"), all.x = TRUE)
ach[, gap_pctile_t := ilearn_pctile_t - never_el_median_pctile]
ach[, gap_pctile_t1 := ilearn_pctile_t1 - never_el_median_pctile_t1]
ach[YEAR_T1 == "2026", `:=`(p_proficient_t1 = NA_real_)]
ach[, suppress := n < MIN_N_LEVEL_SLICE]
write_output(round_numeric(ach, 3), "exiter_achievement.csv")

## Pooled primary-window summary for the percentile figure (by role x band).
ach_pool <- panel[window == "primary" & role %in% c("exiter", "stayer", "never_el") &
        has_ilearn_t1 == TRUE, {
    nn <- .N
    .(
        n = nn,
        ilearn_pctile_t = median(ILEARN_PCTILE, na.rm = TRUE),
        ilearn_pctile_t1 = median(ILEARN_PCTILE_T1, na.rm = TRUE),
        ilearn_pctile_delta = median(ILEARN_PCTILE_T1 - ILEARN_PCTILE, na.rm = TRUE),
        p_proficient_t = mean(ILEARN_PROFICIENT, na.rm = TRUE),
        p_proficient_t1 = mean(ILEARN_PROFICIENT_T1, na.rm = TRUE)
    )
}, keyby = .(window, role, WIDA_EXIT_BAND)]
write_output(round_numeric(ach_pool, 3), "exiter_achievement_pooled.csv")

### --- Exit-year growth (ILEARN SGP at t+1) ---------------------------------
message("[07] Exit-year ILEARN SGP.")

sgp <- panel[role %in% c("exiter", "stayer", "never_el") &
        has_ilearn_t1 == TRUE & is.finite(ILEARN_SGP_T1), {
    nn <- .N
    .(
        n = nn,
        sgp_median = if (nn >= MIN_N_LEVEL_SLICE) median(ILEARN_SGP_T1, na.rm = TRUE) else NA_real_,
        p_sgp_below_35 = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_SGP_T1 < 35, na.rm = TRUE) else NA_real_,
        p_sgp_above_65 = if (nn >= MIN_N_LEVEL_SLICE) mean(ILEARN_SGP_T1 > 65, na.rm = TRUE) else NA_real_
    )
}, keyby = .(YEAR_T, YEAR_T1, window, GRADE, role, WIDA_EXIT_BAND)]
sgp[, suppress := n < MIN_N_LEVEL_SLICE]
write_output(round_numeric(sgp, 3), "exiter_sgp.csv")

sgp_pool <- panel[window == "primary" & role %in% c("exiter", "stayer", "never_el") &
        has_ilearn_t1 == TRUE & is.finite(ILEARN_SGP_T1), {
    nn <- .N
    .(
        n = nn,
        sgp_median = median(ILEARN_SGP_T1, na.rm = TRUE),
        p_sgp_below_35 = mean(ILEARN_SGP_T1 < 35, na.rm = TRUE),
        p_sgp_above_65 = mean(ILEARN_SGP_T1 > 65, na.rm = TRUE)
    )
}, keyby = .(window, role, WIDA_EXIT_BAND)]
write_output(round_numeric(sgp_pool, 3), "exiter_sgp_pooled.csv")

never_el_sgp_med <- sgp_pool[role == "never_el", sgp_median][1]

### --- EL-status robustness (t+1 not 2026; flag usable) ---------------------
message("[07] EL-status robustness.")
el_rob <- panel[role == "exiter" & YEAR_T1 != "2026", {
    nn <- .N
    .(
        n = nn,
        n_el_known_both = sum(!is.na(IS_EL) & !is.na(IS_EL_T1)),
        p_el_to_not_el = mean(IS_EL %in% TRUE & IS_EL_T1 %in% FALSE),
        p_still_el = mean(IS_EL_T1 %in% TRUE),
        p_el_unknown_t1 = mean(is.na(IS_EL_T1))
    )
}, keyby = .(YEAR_T, YEAR_T1, window, WIDA_EXIT_BAND)]
write_output(round_numeric(el_rob, 3), "exiter_el_status_robustness.csv")

### --- Figures --------------------------------------------------------------
n_hm <- n_role[role == "exiter" & GRADE %in% EXITER_GRADES_T,
    .(N = sum(N), role = "exiter"), keyby = .(YEAR_T, WIDA_EXIT_BAND)]
save_figure(plot_exiter_n(n_hm), "07_exiter_n_heatmap", width = 8, height = 5)
if (nrow(ach_pool[role == "exiter"]) > 0) {
    save_figure(plot_exiter_percentile(ach_pool),
        "07_exiter_percentile", width = 8, height = 5.5)
}
if (nrow(sgp_pool[role %in% c("exiter", "stayer")]) > 0) {
    save_figure(plot_exiter_sgp(sgp_pool, never_el_sgp_med),
        "07_exiter_sgp", width = 9, height = 5)
}

message("[07] Exiter cohort complete.")
