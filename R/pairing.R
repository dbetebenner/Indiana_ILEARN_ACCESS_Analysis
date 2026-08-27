###########################################################################
### R/pairing.R — join ILEARN and WIDA into same-year, same-grade pairs,
### and provide the WIDA-level / proficiency-band derivations used
### downstream.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

#' Build same-year, same-grade ILEARN x WIDA pairs
#'
#' Inner join on (ID, YEAR, GRADE). Attaches WIDA proficiency level and a
#' coarse policy band, and flags the year stratum.
#'
#' @param ilearn data.table from load_ilearn().
#' @param wida data.table from load_wida().
#' @param grades Character vector of grades to keep.
#' @return data.table of pairs (student-level; cache only, never committed).
build_pairs <- function(ilearn, wida, grades = ANALYSIS_GRADES) {
    il <- ilearn[GRADE %in% grades]
    wd <- wida[GRADE %in% grades]

    setkey(il, ID, YEAR, GRADE)
    setkey(wd, ID, YEAR, GRADE)

    pairs <- merge(il, wd, by = c("ID", "YEAR", "GRADE"), all = FALSE)

    pairs[, YEAR_STRATUM := classify_year(YEAR)]
    pairs[, WIDA_BAND := wida_policy_band(WIDA_PL)]
    pairs[, IS_ILEARN_YEAR := YEAR %in% ILEARN_YEARS]
    pairs[]
}

#' Classify a year vector into the analysis strata from config.R
#'
#' @param year Character vector of years.
#' @return Character vector of stratum labels (or "other").
classify_year <- function(year) {
    out <- rep(NA_character_, length(year))
    for (nm in names(YEAR_STRATA)) {
        out[year %in% YEAR_STRATA[[nm]]] <- nm
    }
    out[is.na(out)] <- "other"
    out
}

#' Coarse WIDA policy band from the decimal proficiency level
#'
#' Bands align to Indiana's exit structure: below 4.0, the 4.0-4.2 band,
#' the 4.3-4.9 provisional pathway, and 5.0+ auto-exit.
#'
#' @param pl Numeric decimal proficiency level (ACHIEVEMENT_LEVEL_ORIGINAL).
#' @return Ordered factor of band labels.
wida_policy_band <- function(pl) {
    band <- fifelse(
        is.na(pl), NA_character_,
        fifelse(pl < 4.0, "<4.0",
        fifelse(pl < WIDA_EXIT_PROVISIONAL, "4.0-4.2",
        fifelse(pl < WIDA_EXIT_AUTO, "4.3-4.9 (provisional)",
                ">=5.0 (auto-exit)")))
    )
    factor(band, levels = c("<4.0", "4.0-4.2",
        "4.3-4.9 (provisional)", ">=5.0 (auto-exit)"))
}

#' Coarse last-WIDA band for the inferred-exit contrast
#'
#' Three bins: unexpected disappearance (<4.3), committee pathway (4.3-4.9),
#' auto-exit (>=5.0). Used by the exiter cohort (step 07), not the same-year
#' copula slices.
#'
#' @param pl Numeric decimal proficiency level.
#' @return Ordered factor.
wida_exit_band <- function(pl) {
    band <- fifelse(
        is.na(pl), NA_character_,
        fifelse(pl < WIDA_EXIT_PROVISIONAL, "<4.3",
        fifelse(pl < WIDA_EXIT_AUTO, "4.3-4.9", ">=5.0"))
    )
    factor(band, levels = c("<4.3", "4.3-4.9", ">=5.0"))
}

#' Determine whether an ILEARN achievement level denotes proficiency
#'
#' Indiana ILEARN uses "At Proficiency" / "Above Proficiency" as the
#' proficient set (labels vary slightly by year; matched case-insensitively).
#'
#' @param level Character vector of ILEARN achievement levels.
#' @return Logical vector; NA where the level is missing/unrecognized.
ilearn_is_proficient <- function(level) {
    lv <- tolower(trimws(level))
    prof <- grepl("at proficien|above proficien|^proficient|^advanced|pass\\+|^pass plus", lv)
    not_prof <- grepl("below|approaching|did not pass|^pass$|not proficien", lv)
    out <- rep(NA, length(level))
    out[prof] <- TRUE
    out[not_prof & !prof] <- FALSE
    out
}

#' Classify an ILEARN ENGLISH_LEARNER_STATUS value as current EL
#'
#' Indiana exports vary ("Y"/"N", "EL", "English Learner", ...). This is a
#' conservative matcher; unique raw values are inventoried in the audit.
#'
#' @param status Character vector of EL status labels.
#' @return Logical; TRUE = current EL, FALSE = not, NA = missing.
is_current_el <- function(status) {
    s <- tolower(trimws(as.character(status)))
    s[s %in% c("", "unknown")] <- NA_character_
    ## Meal-status values leaked into ENGLISH_LEARNER_STATUS in some years.
    s[grepl("meal", s)] <- NA_character_
    yes <- grepl("limited english|^(y|yes|1|el|ell|lep|current)$|english learner|english-learner|current el", s)
    no  <- grepl("not an? english|non.?el|never el|fluent english|native english|exited|^(n|no|0)$", s)
    out <- rep(NA, length(s))
    out[yes] <- TRUE
    out[no] <- FALSE
    out
}
