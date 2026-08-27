###########################################################################
###
### config.R — shared configuration for the ILEARN x WIDA ACCESS
###             signal-vs-noise pipeline.
###
### Sourced by every numbered step. Holds source paths, the analysis
### universe (grades / year strata), score filters, Indiana EL exit
### policy anchors, copula families, and minimum-N reporting gates.
###
### CONTAINMENT: this file names read-only source paths. Paired
### student-level data is written only under cache/ (gitignored).
### Committed artifacts are aggregates (outputs/) and figures/.
###
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

### -----------------------------------------------------------------------
### Paths
### -----------------------------------------------------------------------

## Resolve this project's root directory regardless of the working
## directory (Rscript --file=, source(), or an IDE run).
CONFIG_resolve_root <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
        return(normalizePath(dirname(sub("^--file=", "", file_arg[1])),
            winslash = "/", mustWork = FALSE))
    }
    ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) {
        return(normalizePath(dirname(ofile), winslash = "/", mustWork = FALSE))
    }
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

PROJECT_ROOT <- CONFIG_resolve_root()

## Read-only source data (aggregate/schema access only; never committed here).
ILEARN_LONG_PATH <- "/Users/conet/GitHub/CenterForAssessment/Indiana/master/SGP/Data/Indiana_SGP_LONG_Data.Rdata"
WIDA_LONG_PATH   <- "/Users/conet/GitHub/CenterForAssessment/WIDA_IN/master/Data/WIDA_IN_SGP_LONG_Data.Rdata"

## Fallback for 2026 ILEARN native NEW-scale scores (SCALE_SCORE = new,
## SCALE_SCORE_OLD_SCALE = equated old scale). Used only by the scale audit.
ILEARN_2026_LONG_PATH <- "/Users/conet/GitHub/CenterForAssessment/Indiana/master/SGP/Data/Indiana_Data_LONG_2026.Rdata"

## Output locations (all under the project; created on demand).
CACHE_DIR   <- file.path(PROJECT_ROOT, "cache")   # gitignored, student-level
OUTPUTS_DIR <- file.path(PROJECT_ROOT, "outputs") # committed, aggregate only
FIGURES_DIR <- file.path(PROJECT_ROOT, "figures") # committed

for (d in c(CACHE_DIR, OUTPUTS_DIR, FIGURES_DIR)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

PAIRS_CACHE_PATH <- file.path(CACHE_DIR, "pairs.rds")

### -----------------------------------------------------------------------
### Analysis universe
### -----------------------------------------------------------------------

ANALYSIS_GRADES <- as.character(3:8)

## Year strata. The primary estimand is same-year, same-grade dependence
## in the stable-scale window. 2019, 2026, and ISTEP are reported apart.
YEAR_STRATA <- list(
    primary        = as.character(2021:2025), # stable ILEARN + WIDA OLD scale
    ilearn_2019    = "2019",                  # first ILEARN year
    ilearn_2026    = "2026",                  # both scales reset; ranks only
    istep_contrast = as.character(2017:2018)  # ISTEP+ ELA, not ILEARN
)

## Years for which the ELA content test is ILEARN (not ISTEP+). Proficiency
## and raw-score interpretation are only comparable within these, and the
## 2026 standard setting further breaks comparability (use ranks in 2026).
ILEARN_YEARS <- as.character(c(2019, 2021:2026))

### -----------------------------------------------------------------------
### Score filters
### -----------------------------------------------------------------------

## ILEARN/ISTEP scale scores below this are placeholders (observed minima of
## 100 in 2017-2019). Real ILEARN ELA lives in the ~5000-6000 band.
ILEARN_MIN_VALID_SCORE <- 5000

## WIDA lowest-obtainable-score sentinel. Drop unless a documented LOSS
## policy says otherwise. WIDA composite scores live in the ~200-500 band.
WIDA_LOSS_SENTINEL <- 100

### -----------------------------------------------------------------------
### Indiana EL exit policy anchors (OLD WIDA scale, <= 2025)
### -----------------------------------------------------------------------
### Source: Indiana DOE, "Exit Criteria for English Learners Guidance
### 2025-2026" (pub. Apr 2025; ESSA State Plan amendment approved summer
### 2024). WIDA resets its scale in July 2026; new-scale anchors are TBD.

WIDA_EXIT_AUTO         <- 5.0 # absolute auto-exit, Overall composite
WIDA_EXIT_PROVISIONAL  <- 4.3 # lower bound of the 4.3-4.9 committee pathway

## Candidate Overall-composite cuts scanned for the empirical transition.
WIDA_CUT_GRID <- seq(2.0, 5.5, by = 0.1)

### -----------------------------------------------------------------------
### Copula families + dependence estimation
### -----------------------------------------------------------------------
### Family selection is settled upstream (Copula_Sensitivity_Analyses:
### t and Frank dominate educational pairs). We fit a small confirmatory
### set here rather than re-running the 38-hour AIC grid.

COPULA_FAMILIES <- c("t", "frank", "gaussian")

## Moving-window width (on the WIDA U = pseudo-observation scale) for the
## local tau(u) curve in step 04.
LOCAL_WINDOW_WIDTH <- 0.15
LOCAL_WINDOW_STEP  <- 0.02

### -----------------------------------------------------------------------
### Minimum-N reporting gates
### -----------------------------------------------------------------------

MIN_N_COPULA_CELL <- 200 # fit a copula for a (year, grade) cell
MIN_N_LEVEL_SLICE <- 50  # report a stat for a WIDA-level slice
MIN_N_SUPPRESS    <- 10  # suppress / flag any cell below this

### -----------------------------------------------------------------------
### Optional-package availability (degrade gracefully)
### -----------------------------------------------------------------------

HAS_ENERGY    <- requireNamespace("energy", quietly = TRUE)    # distance corr
HAS_HEXBIN    <- requireNamespace("hexbin", quietly = TRUE)    # hexbin plots
HAS_SEGMENTED <- requireNamespace("segmented", quietly = TRUE) # change-point

### -----------------------------------------------------------------------
### Reproducibility
### -----------------------------------------------------------------------

RNG_SEED <- 4283L

### -----------------------------------------------------------------------
### Exiter-cohort transitions (step 07)
### -----------------------------------------------------------------------
### Adjacent ILEARN years only. 2019->2021 (COVID hole) and 2018->2019
### (ISTEP->ILEARN) are excluded. 2025->2026 is ranks / percentiles only.

EXITER_TRANSITIONS <- data.table(
    year_t  = c("2021", "2022", "2023", "2024", "2025"),
    year_t1 = c("2022", "2023", "2024", "2025", "2026"),
    window  = c("primary", "primary", "primary", "primary", "ranks_only")
)
EXITER_GRADES_T <- as.character(3:7) # grade 8 at t cannot be followed
EXITERS_CACHE_PATH <- file.path(CACHE_DIR, "exiters.rds")

### -----------------------------------------------------------------------
### Lagged proficiency-copula (step 08) and JSON manifests
### -----------------------------------------------------------------------
### WIDA at t, ILEARN at t+1. 2025->2026 is ranks only (no proficiency cut).
### This is a documented contrast, not a recommended exit rule.

LAGGED_CACHE_PATH <- file.path(CACHE_DIR, "lagged.rds")
LAGGED_WIDA_GRADES <- as.character(2:7) # grade 2 WIDA -> grade 3 ILEARN
LAGGED_P_TARGET <- 0.5
LAGGED_LOCAL_PL_HALFWIDTH <- 0.15

MANIFEST_DIR <- file.path(OUTPUTS_DIR, "manifests")
if (!dir.exists(MANIFEST_DIR)) {
    dir.create(MANIFEST_DIR, recursive = TRUE, showWarnings = FALSE)
}

message(sprintf("[config] project root: %s", PROJECT_ROOT))
