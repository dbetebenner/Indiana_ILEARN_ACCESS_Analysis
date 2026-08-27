###########################################################################
### R/io.R — loading source data and writing aggregate artifacts.
###
### Loaders return de-identified data.tables restricted to the columns the
### pipeline needs. Writers append provenance and refuse to emit obviously
### student-level files to the committed outputs/ directory.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

#' Load an .Rdata file and return its single named object
#'
#' @param path Path to a .Rdata file expected to contain exactly one object.
#' @return The loaded object.
load_single_object <- function(path) {
    if (!file.exists(path)) {
        stop("Source file not found: ", path, call. = FALSE)
    }
    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)
    if (length(loaded) != 1L) {
        stop("Expected exactly one object in ", path,
            "; found: ", paste(loaded, collapse = ", "), call. = FALSE)
    }
    get(loaded, envir = env)
}

#' Load ILEARN/ISTEP ELA + Mathematics long data (de-identified subset)
#'
#' Harmonizes names to ID / YEAR / GRADE and keeps only the fields the
#' pipeline uses. Applies VALID_CASE and the ILEARN floor filter.
#'
#' @param content_area "ELA" (default) or "MATHEMATICS".
#' @return data.table: ID, YEAR, GRADE, CONTENT_AREA, ILEARN_SCORE,
#'   ILEARN_SCORE_OLD, ILEARN_LEVEL, EL_STATUS.
load_ilearn <- function(content_area = "ELA") {
    dt <- load_single_object(ILEARN_LONG_PATH)
    setDT(dt)

    has_sgp <- "SGP" %in% names(dt)
    keep <- dt[
        VALID_CASE == "VALID_CASE" &
            CONTENT_AREA == content_area &
            !is.na(SCALE_SCORE),
        .(
            ID       = as.character(STUDENT_ID),
            YEAR     = as.character(SCHOOL_YEAR),
            GRADE    = as.character(as.numeric(GRADE_ID)),
            CONTENT_AREA = as.character(CONTENT_AREA),
            ILEARN_SCORE     = as.numeric(SCALE_SCORE),
            ILEARN_SCORE_OLD = as.numeric(SCALE_SCORE_OLD_SCALE),
            ILEARN_LEVEL = as.character(ACHIEVEMENT_LEVEL),
            EL_STATUS    = as.character(ENGLISH_LEARNER_STATUS),
            ILEARN_SGP   = if (has_sgp) as.numeric(SGP) else NA_real_
        )
    ]

    ## Drop placeholder floor scores in ILEARN-era years (min == 100).
    keep <- keep[ILEARN_SCORE >= ILEARN_MIN_VALID_SCORE]
    keep[]
}

#' Load WIDA ACCESS Overall-composite long data (de-identified subset)
#'
#' NOTE: CONTENT_AREA is stored as "READING" but the score is the Overall
#' composite. We relabel to WIDA_COMPOSITE to prevent mislabeled axes.
#'
#' @return data.table: ID, YEAR, GRADE, WIDA_SCORE, WIDA_SCORE_OLD,
#'   WIDA_LEVEL, WIDA_PL (decimal proficiency level).
load_wida <- function() {
    dt <- load_single_object(WIDA_LONG_PATH)
    setDT(dt)

    old_scale_col <- if ("SCALE_SCORE_OLD_SCALE" %in% names(dt)) {
        "SCALE_SCORE_OLD_SCALE"
    } else {
        NA_character_
    }

    has_sgp <- "SGP" %in% names(dt)
    keep <- dt[
        VALID_CASE == "VALID_CASE" & !is.na(SCALE_SCORE),
        .(
            ID    = as.character(ID),
            YEAR  = as.character(YEAR),
            GRADE = as.character(as.numeric(GRADE)),
            WIDA_SCORE = as.numeric(SCALE_SCORE),
            WIDA_SCORE_OLD = if (!is.na(old_scale_col)) as.numeric(get(old_scale_col)) else NA_real_,
            WIDA_LEVEL = as.character(ACHIEVEMENT_LEVEL),
            WIDA_PL    = suppressWarnings(as.numeric(ACHIEVEMENT_LEVEL_ORIGINAL)),
            WIDA_SGP   = if (has_sgp) as.numeric(SGP) else NA_real_
        )
    ]

    ## Drop the LOSS sentinel unless a documented policy says otherwise.
    keep <- keep[WIDA_SCORE > WIDA_LOSS_SENTINEL]
    keep[]
}

#' Cached slim ILEARN extract (gitignored). Avoids reloading the 10M-row LONG.
#'
#' @param content_area "ELA" or "MATHEMATICS".
#' @param force If TRUE, rebuild the cache from the source LONG.
get_ilearn <- function(content_area = "ELA", force = FALSE) {
    path <- file.path(CACHE_DIR, paste0("ilearn_", tolower(content_area), ".rds"))
    if (!force && file.exists(path)) {
        message(sprintf("[io] cache hit: %s", basename(path)))
        return(readRDS(path))
    }
    message(sprintf("[io] building slim ILEARN cache (%s) ...", content_area))
    dt <- load_ilearn(content_area)
    saveRDS(dt, path)
    dt
}

#' Cached slim WIDA extract (gitignored).
#'
#' @param force If TRUE, rebuild the cache from the source LONG.
get_wida <- function(force = FALSE) {
    path <- file.path(CACHE_DIR, "wida.rds")
    if (!force && file.exists(path)) {
        message("[io] cache hit: wida.rds")
        return(readRDS(path))
    }
    message("[io] building slim WIDA cache ...")
    dt <- load_wida()
    saveRDS(dt, path)
    dt
}

#' Round all numeric (double) columns of a data.table for readable output
#'
#' Integer columns (e.g. counts) are left untouched.
#'
#' @param dt A data.table.
#' @param digits Number of digits.
#' @return The data.table with double columns rounded (by ref; also returned).
round_numeric <- function(dt, digits = 4) {
    dbl_cols <- names(dt)[vapply(dt, is.double, logical(1))]
    if (length(dbl_cols) > 0) {
        dt[, (dbl_cols) := lapply(.SD, round, digits = digits), .SDcols = dbl_cols]
    }
    dt[]
}

#' FERPA-style small-N redaction for a committed aggregate table
#'
#' Rows whose primary reporting count is in (0, `min_n`) are **dropped**.
#' Thin sides of a split (n_below / n_above / n_local / n_ge) have that
#' side's count and statistics blanked; the row is kept if the other side
#' is reportable. Does not touch student-level cache files.
#'
#' @param dt A data.table / data.frame of aggregates.
#' @param min_n Minimum publishable N (default `MIN_N_SUPPRESS`).
#' @return A data.table.
redact_small_n <- function(dt, min_n = MIN_N_SUPPRESS) {
    d <- as.data.table(copy(dt))
    side_pairs <- list(
        list(n = "n_below",
            stats = c("tau_below", "ilearn_prof_below", "ilearn_pctile_below")),
        list(n = "n_above",
            stats = c("tau_above", "ilearn_prof_above", "ilearn_pctile_above",
                      "tau_gap")),
        list(n = "n_local", stats = c("p_local")),
        list(n = "n_ge",    stats = c("p_ge")),
        list(n = "n_prior",
            stats = c("ilearn_pctile_prior", "p_proficient_prior")),
        list(n = "n_el_known_both",
            stats = c("p_el_to_not_el", "p_still_el", "p_el_unknown_t1"))
    )
    for (sp in side_pairs) {
        if (!sp$n %in% names(d)) next
        nval <- as.numeric(d[[sp$n]])
        thin <- is.finite(nval) & nval > 0 & nval < min_n
        if (!any(thin)) next
        d[thin, (sp$n) := NA_integer_]
        for (s in intersect(sp$stats, names(d))) {
            d[thin, (s) := NA]
        }
        if ("suppress" %in% names(d)) d[thin, suppress := TRUE]
    }

    primary <- intersect(c("N", "n", "n_pairs", "n_paired"), names(d))
    if (length(primary) >= 1L) {
        ncol <- primary[[1]]
        nval <- as.numeric(d[[ncol]])
        thin <- is.finite(nval) & nval > 0 & nval < min_n
        if (any(thin)) d <- d[!thin]
    }
    d[]
}

#' Re-apply small-N redaction to every committed CSV under outputs/
#'
#' Use after changing the redaction rule, or to sanitize a tree that was
#' written before `write_output()` started dropping thin cells.
redact_committed_outputs <- function(min_n = MIN_N_SUPPRESS) {
    csvs <- list.files(OUTPUTS_DIR, pattern = "\\.csv$", full.names = TRUE)
    n_dropped <- 0L
    for (p in csvs) {
        dt <- fread(p)
        before <- nrow(dt)
        red <- redact_small_n(dt, min_n = min_n)
        fwrite(red, p)
        n_dropped <- n_dropped + (before - nrow(red))
        message(sprintf("[io] redacted %s (%d → %d rows)",
            basename(p), before, nrow(red)))
    }
    invisible(n_dropped)
}

#' Write an aggregate table to outputs/ with a light student-level guard
#'
#' @param dt A data.table / data.frame of aggregates (no student IDs).
#' @param filename Basename written under OUTPUTS_DIR.
#' @param id_like Regex of column names that would indicate student-level
#'   leakage; if matched, the write aborts.
write_output <- function(dt, filename,
                         id_like = "^(ID|STUDENT_ID|STN)$") {
    stopifnot(is.data.frame(dt))
    offending <- grep(id_like, names(dt), value = TRUE, ignore.case = TRUE)
    if (length(offending) > 0) {
        stop("Refusing to write likely student-level columns to outputs/: ",
            paste(offending, collapse = ", "), call. = FALSE)
    }
    dt <- redact_small_n(dt)
    path <- file.path(OUTPUTS_DIR, filename)
    fwrite(dt, path)
    message(sprintf("[io] wrote %s (%d rows)", filename, nrow(dt)))
    invisible(path)
}

#' Write a JSON artifact under outputs/ (or a subdirectory)
#'
#' Same student-level column guard as write_output(). Requires jsonlite.
#'
#' @param x A data.frame or a nested list of aggregates.
#' @param filename Basename or relative path under OUTPUTS_DIR.
#' @param id_like Regex of column names that would indicate student-level leakage.
write_json <- function(x, filename, id_like = "^(ID|STUDENT_ID|STN)$") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("jsonlite is required to write JSON manifests.", call. = FALSE)
    }
    if (is.data.frame(x)) {
        offending <- grep(id_like, names(x), value = TRUE, ignore.case = TRUE)
        if (length(offending) > 0) {
            stop("Refusing to write likely student-level columns to JSON: ",
                paste(offending, collapse = ", "), call. = FALSE)
        }
    }
    path <- file.path(OUTPUTS_DIR, filename)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
        x, path,
        pretty = TRUE, auto_unbox = TRUE, na = "null",
        dataframe = "rows", POSIXt = "ISO8601", digits = 6
    )
    message(sprintf("[io] wrote %s", filename))
    invisible(path)
}
