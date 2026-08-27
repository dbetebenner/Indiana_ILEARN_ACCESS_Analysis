###########################################################################
###
### run_all.R — driver for the ILEARN x WIDA ACCESS signal-vs-noise
###             pipeline. Sources config + helpers, then runs the numbered
###             steps in order.
###
### Usage:
###   Rscript run_all.R            # full pipeline
###   Rscript run_all.R 1 2 3      # steps 1-3 only (00 audit always runs)
###   Rscript run_all.R 9          # copula asymmetry + Youden explainer
###   source("run_all.R")          # interactive
###
### Steps:
###   00 scale audit         (documentary; aggregate)
###   01 ingest + pair       (writes gitignored cache/pairs.rds)
###   02 N counts            (outputs/n_*.csv)
###   03 global dependence   (copulas per year x grade)
###   04 local dependence    (tau(u), WIDA-level slices, cut scan)
###   05 exit criterion      (4.3 vs 5.0 vs change-point)
###   06 extensions          (Math, never-EL, lagged, dual-scale)
###   07 exiters             (ILEARN after WIDA disappears)
###   08 lagged proficiency  (WIDA t → ILEARN t+1 50-50 cut; contrast)
###   09 copula asymmetry    (radial / tail / exchangeability + Youden)
###
###########################################################################

## Resolve project root and source config + helpers.
.run_all_dir <- local({
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
        return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), mustWork = FALSE))
    }
    ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) return(normalizePath(dirname(ofile), mustWork = FALSE))
    normalizePath(getwd(), mustWork = FALSE)
})

source(file.path(.run_all_dir, "config.R"))
for (f in list.files(file.path(.run_all_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
    source(f)
}
set.seed(RNG_SEED)

## Which steps to run (00 audit always runs; 01 must run before 02-09).
step_args <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)))
step_args <- step_args[!is.na(step_args)]
run_steps <- if (length(step_args) > 0) step_args else 1:9

step_files <- c(
    "00_scale_audit.R",
    "01_ingest_and_pair.R",
    "02_n_counts.R",
    "03_global_dependence.R",
    "04_local_dependence.R",
    "05_exit_criterion.R",
    "06_extensions.R",
    "07_exiters.R",
    "08_lagged_proficiency.R",
    "09_copula_asymmetry.R"
)

run_step <- function(file) {
    message("\n=============================================================")
    message(sprintf("[run_all] %s", file))
    message("=============================================================")
    source(file.path(.run_all_dir, file), local = TRUE)
}

## 00 is always run (fast, documentary).
run_step(step_files[1])

## 01 is a prerequisite for anything that reads the pairs cache.
needs_pairs <- any(run_steps >= 1)
if (needs_pairs && !file.exists(PAIRS_CACHE_PATH)) {
    run_step(step_files[2])
} else if (1 %in% run_steps) {
    run_step(step_files[2])
}

for (s in setdiff(run_steps, 1)) {
    if (s >= 2 && s <= 9) run_step(step_files[s + 1])
}

write_all_manifests()

message("\n[run_all] complete. Aggregates in outputs/, JSON in outputs/manifests/, figures in figures/.")
