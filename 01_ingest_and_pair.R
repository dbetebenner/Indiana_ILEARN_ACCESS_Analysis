###########################################################################
### 01_ingest_and_pair.R — load both long files, build same-year,
### same-grade ILEARN ELA x WIDA ACCESS pairs, derive rank/percentile/
### proficiency columns, and cache the student-level result (gitignored).
### A committed pair manifest records the shape only (no IDs).
###########################################################################

if (!exists("PROJECT_ROOT")) {
    .d <- tryCatch(dirname(sub("^--file=", "",
        grep("^--file=", commandArgs(FALSE), value = TRUE)[1])),
        error = function(e) getwd())
    source(file.path(.d, "config.R"))
    for (f in list.files(file.path(.d, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
}

message("[01] Ingesting and pairing ILEARN ELA x WIDA ACCESS.")

ilearn_ela <- get_ilearn("ELA")
wida       <- get_wida()

pairs <- build_pairs(ilearn_ela, wida, grades = ANALYSIS_GRADES)

### Derived columns used throughout the pipeline.
add_pobs(pairs, wida_col = "WIDA_SCORE", ilearn_col = "ILEARN_SCORE")
pairs[, ILEARN_PCTILE := percentile_within(ILEARN_SCORE), by = .(YEAR, GRADE)]
pairs[, ILEARN_PROFICIENT := ilearn_is_proficient(ILEARN_LEVEL)]

### Cache the student-level pairs (gitignored).
saveRDS(pairs, PAIRS_CACHE_PATH)
message(sprintf("[01] Cached %s pairs to %s (gitignored).",
    scales::comma(nrow(pairs)), PAIRS_CACHE_PATH))

### Committed manifest — shape and filters only, never IDs.
manifest <- pairs[, .(
    n_pairs = .N,
    n_grades = uniqueN(GRADE),
    grades = paste(sort(unique(GRADE)), collapse = ","),
    ilearn_score_min = round(min(ILEARN_SCORE, na.rm = TRUE), 1),
    ilearn_score_max = round(max(ILEARN_SCORE, na.rm = TRUE), 1),
    wida_pl_min = round(min(WIDA_PL, na.rm = TRUE), 2),
    wida_pl_max = round(max(WIDA_PL, na.rm = TRUE), 2)
), keyby = .(YEAR, YEAR_STRATUM)]
write_output(manifest, "pair_manifest.csv")

### Also record the filter provenance for reproducibility.
filters <- data.table(
    filter = c("ilearn_min_valid_score", "wida_loss_sentinel",
        "grades", "join_keys", "content_area"),
    value = c(as.character(ILEARN_MIN_VALID_SCORE),
        paste0("> ", WIDA_LOSS_SENTINEL),
        paste(ANALYSIS_GRADES, collapse = ","),
        "ID=STUDENT_ID, YEAR=SCHOOL_YEAR, GRADE=GRADE_ID",
        "ILEARN ELA vs WIDA Overall Composite (stored as READING)")
)
write_output(filters, "pair_filters.csv")

message("[01] Pairing complete.")
