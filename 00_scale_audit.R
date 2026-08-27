###########################################################################
### 00_scale_audit.R — documentary, aggregate-only audit of the two source
### files: score ranges by year x assessment, confirmation of the 2026
### column meanings, and a small data dictionary. No student rows retained.
###########################################################################

## Standalone bootstrap: source config + helpers if not already loaded.
if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[00] Auditing source scales (aggregate only).")

### --- ILEARN score ranges by year x content area ---------------------------
il_all <- rbindlist(list(get_ilearn("ELA"), get_ilearn("MATHEMATICS")))
il_audit <- il_all[, .(
    n = .N,
    min = round(min(ILEARN_SCORE, na.rm = TRUE), 1),
    q25 = round(quantile(ILEARN_SCORE, 0.25, na.rm = TRUE), 1),
    median = round(median(ILEARN_SCORE, na.rm = TRUE), 1),
    q75 = round(quantile(ILEARN_SCORE, 0.75, na.rm = TRUE), 1),
    max = round(max(ILEARN_SCORE, na.rm = TRUE), 1)
), keyby = .(CONTENT_AREA, YEAR)]
write_output(il_audit, "audit_ilearn_scale_by_year.csv")

### --- WIDA score ranges by year --------------------------------------------
wd_all <- get_wida()
wd_audit <- wd_all[, .(
    n = .N,
    min = round(min(WIDA_SCORE, na.rm = TRUE), 1),
    q25 = round(quantile(WIDA_SCORE, 0.25, na.rm = TRUE), 1),
    median = round(median(WIDA_SCORE, na.rm = TRUE), 1),
    q75 = round(quantile(WIDA_SCORE, 0.75, na.rm = TRUE), 1),
    max = round(max(WIDA_SCORE, na.rm = TRUE), 1),
    pl_min = round(min(WIDA_PL, na.rm = TRUE), 2),
    pl_max = round(max(WIDA_PL, na.rm = TRUE), 2)
), keyby = YEAR]
write_output(wd_audit, "audit_wida_scale_by_year.csv")

### --- 2026 ILEARN column-meaning check -------------------------------------
### The combined LONG carries 2026 ILEARN SCALE_SCORE on the equated OLD
### scale. Confirm the native NEW-scale file disagrees (new != old) so a
### future reader does not treat 2026 raw scores as comparable to 2021-2025.
audit_2026 <- data.table(
    check = character(), detail = character()
)
if (file.exists(ILEARN_2026_LONG_PATH)) {
    il26 <- load_single_object(ILEARN_2026_LONG_PATH)
    setDT(il26)
    il26e <- il26[VALID_CASE == "VALID_CASE" & CONTENT_AREA == "ELA" & !is.na(SCALE_SCORE)]
    audit_2026 <- rbind(audit_2026, data.table(
        check = c("ilearn_2026_new_scale_median",
                  "ilearn_2026_old_scale_median",
                  "ilearn_2026_new_equals_old"),
        detail = c(
            as.character(round(median(il26e$SCALE_SCORE, na.rm = TRUE), 1)),
            as.character(round(median(il26e$SCALE_SCORE_OLD_SCALE, na.rm = TRUE), 1)),
            as.character(isTRUE(all.equal(
                median(il26e$SCALE_SCORE, na.rm = TRUE),
                median(il26e$SCALE_SCORE_OLD_SCALE, na.rm = TRUE))))
        )
    ))
} else {
    audit_2026 <- rbind(audit_2026, data.table(
        check = "ilearn_2026_native_file", detail = "not found (skipped)"))
}
write_output(audit_2026, "audit_2026_scale_change.csv")

### --- Data dictionary -------------------------------------------------------
data_dictionary <- data.table(
    column = c("ID", "YEAR", "GRADE", "WIDA_SCORE", "WIDA_SCORE_OLD",
        "WIDA_LEVEL", "WIDA_PL", "ILEARN_SCORE", "ILEARN_SCORE_OLD",
        "ILEARN_LEVEL", "EL_STATUS", "U_WIDA", "V_ILEARN",
        "WIDA_BAND", "ILEARN_PCTILE", "ILEARN_PROFICIENT",
        "ILEARN_SGP", "WIDA_SGP"),
    meaning = c(
        "De-identified student id (join key; cache only, never committed).",
        "School year (spring administration).",
        "Grade (3-8 in scope).",
        "WIDA ACCESS Overall composite scale score (native scale for the year).",
        "WIDA Overall composite on the pre-2026 OLD scale where available.",
        "WIDA achievement level bucket (L1..L6, plus the 4.3 provisional band).",
        "WIDA decimal proficiency level (ACHIEVEMENT_LEVEL_ORIGINAL).",
        "ILEARN/ISTEP ELA (or Math) scale score, native scale for the year.",
        "ILEARN score on the equated OLD scale where available.",
        "ILEARN achievement level label.",
        "English learner status flag from the ILEARN demographics.",
        "WIDA within-cell pseudo-observation (rank in (0,1)) by year x grade.",
        "ILEARN within-cell pseudo-observation by year x grade.",
        "Coarse WIDA policy band (<4.0, 4.0-4.2, 4.3-4.9, >=5.0).",
        "ILEARN percentile within year x grade (0-100).",
        "Logical ILEARN proficiency (At/Above), NA if unrecognized.",
        "ILEARN cohort-referenced SGP (growth extension; secondary).",
        "WIDA ACCESS cohort-referenced SGP (growth extension; secondary)."
    )
)
write_output(data_dictionary, "data_dictionary.csv")

### --- Label inventories (aggregate; informs proficiency / EL classifiers) --
il_ela <- il_all[CONTENT_AREA == "ELA"]
write_output(il_ela[, .(N = .N), keyby = ILEARN_LEVEL], "audit_ilearn_levels.csv")
write_output(il_ela[, .(N = .N), keyby = EL_STATUS], "audit_el_status.csv")
write_output(wd_all[, .(N = .N), keyby = WIDA_LEVEL], "audit_wida_levels.csv")

message("[00] Scale audit complete.")
