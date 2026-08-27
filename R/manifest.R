###########################################################################
### R/manifest.R — JSON manifests of aggregate results for secondary
### analysis and reporting. Never includes student identifiers.
###########################################################################

read_output_csv <- function(filename) {
    path <- file.path(OUTPUTS_DIR, filename)
    if (!file.exists(path)) return(NULL)
    fread(path)
}

figure_catalog <- function() {
    pngs <- list.files(FIGURES_DIR, pattern = "\\.png$", full.names = FALSE)
    data.table(
        file = pngs,
        path = file.path("figures", pngs),
        stem = sub("\\.png$", "", pngs)
    )
}

table_catalog <- function() {
    csvs <- list.files(OUTPUTS_DIR, pattern = "\\.csv$", full.names = FALSE)
    data.table(
        file = csvs,
        path = file.path("outputs", csvs),
        json = file.path("outputs", "manifests", "tables",
            sub("\\.csv$", ".json", csvs))
    )
}

#' Curated findings object consumed by reporting and the docs/ deck
build_results_manifest <- function() {
    n_tot <- read_output_csv("n_totals.csv")
    dep <- read_output_csv("dep_rank_correlations.csv")
    lev <- read_output_csv("local_by_wida_level.csv")
    youden <- read_output_csv("exit_q1q2_summary.csv")
    grade_c <- read_output_csv("exit_grade_consistency.csv")
    first_x <- read_output_csv("ext_panel_first_crossing.csv")
    ex_n <- read_output_csv("exiter_n_totals.csv")
    ex_ach <- read_output_csv("exiter_achievement_pooled.csv")
    ex_sgp <- read_output_csv("exiter_sgp_pooled.csv")
    ex_el <- read_output_csv("exiter_el_status_robustness.csv")
    lag_cut <- read_output_csv("lagged_fifty_fifty_cut.csv")
    lag_pool <- read_output_csv("lagged_fifty_fifty_pooled.csv")
    lag_n <- read_output_csv("lagged_n_year_grade.csv")

    prim_tau <- if (!is.null(dep)) {
        d <- dep[YEAR_STRATUM == "primary"]
        list(
            n_cells = nrow(d),
            kendall_min = min(d$kendall, na.rm = TRUE),
            kendall_median = median(d$kendall, na.rm = TRUE),
            kendall_max = max(d$kendall, na.rm = TRUE)
        )
    } else NULL

    level_2025 <- if (!is.null(lev)) {
        lev[YEAR == "2025", .(
            n = sum(n),
            kendall = median(kendall, na.rm = TRUE),
            ilearn_pctile_median = median(ilearn_pctile_median, na.rm = TRUE),
            p_ilearn_proficient = median(p_ilearn_proficient, na.rm = TRUE)
        ), keyby = WIDA_LEVEL]
    } else NULL

    exiter_n_primary <- if (!is.null(ex_n)) {
        ex_n[window == "primary", .(N = sum(N)), keyby = role]
    } else NULL

    el_agree <- if (!is.null(ex_el)) {
        ex_el[, .(
            n = sum(n),
            p_el_to_not_el_min = min(p_el_to_not_el, na.rm = TRUE),
            p_el_to_not_el_max = max(p_el_to_not_el, na.rm = TRUE),
            p_still_el_min = min(p_still_el, na.rm = TRUE),
            p_still_el_max = max(p_still_el, na.rm = TRUE)
        ), keyby = WIDA_EXIT_BAND]
    } else NULL

    lagged_grade <- if (!is.null(lag_cut)) {
        lag_cut[window == "primary", .(
            n_cells = .N,
            n = sum(n),
            copula_cut_median = median(copula_cut, na.rm = TRUE),
            copula_cut_min = min(copula_cut, na.rm = TRUE),
            copula_cut_max = max(copula_cut, na.rm = TRUE),
            empirical_local_median = median(empirical_local_cut, na.rm = TRUE),
            p_ge_4.3 = median(p_ge_4.3, na.rm = TRUE),
            p_ge_5.0 = median(p_ge_5.0, na.rm = TRUE),
            p_local_4.3 = median(p_local_4.3, na.rm = TRUE),
            p_local_5.0 = median(p_local_5.0, na.rm = TRUE),
            p_never_el = median(p_never_el_proficient, na.rm = TRUE)
        )]
    } else NULL

    list(
        schema = "in.ilearn.wida.results.v1",
        generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        wida_label = "WIDA ACCESS Overall Composite",
        disclaimer = paste(
            "Aggregates only. Testing-pattern exit is not an official IDOE roster.",
            "The lagged 50-50 ILEARN-proficiency cut is a documented contrast,",
            "not a recommended EL exit rule: native English speakers are not all",
            "ILEARN-proficient."
        ),
        policy = list(
            auto_exit = WIDA_EXIT_AUTO,
            provisional = WIDA_EXIT_PROVISIONAL,
            source = "IDOE Exit Criteria for English Learners Guidance 2025-2026"
        ),
        windows = list(
            primary = YEAR_STRATA$primary,
            ranks_only = YEAR_STRATA$ilearn_2026
        ),
        n = list(
            pairs_total = if (!is.null(n_tot)) n_tot$total_pairs[1] else NA,
            pairs_primary = if (!is.null(n_tot)) n_tot$primary_pairs[1] else NA,
            lagged_primary = if (!is.null(lag_n))
                lag_n[window == "primary", sum(N)] else NA,
            exiters_primary = if (!is.null(exiter_n_primary))
                exiter_n_primary[role == "exiter", N] else NA
        ),
        same_year = list(
            kendall_primary = prim_tau,
            level_table_2025 = level_2025
        ),
        youden = list(
            by_stratum = youden,
            by_grade_primary = grade_c
        ),
        first_crossing = first_x,
        exiters = list(
            n_by_role_primary = exiter_n_primary,
            achievement_pooled = ex_ach,
            sgp_pooled = ex_sgp,
            el_status_agreement = el_agree
        ),
        lagged_proficiency = list(
            method = paste(
                "Copula C(U_WIDA_t, V_ILEARN_{t+1}); invert",
                "P(V >= v* | U = u) = 0.5, with v* the ILEARN proficiency",
                "cut on the paired rank scale. Empirical local window ±0.15 PL."
            ),
            critique = paste(
                "ILEARN proficiency is not a proxy for English access.",
                "Never-EL students are not all proficient."
            ),
            pooled_primary = lag_pool,
            by_grade_summary = lagged_grade,
            by_cell = lag_cut
        ),
        artifacts = list(
            tables = table_catalog(),
            figures = figure_catalog()
        )
    )
}

#' Write per-table JSON, index.json, and results.json under outputs/manifests/
write_all_manifests <- function() {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        message("[manifest] jsonlite not installed; skipping JSON manifests.")
        return(invisible(NULL))
    }
    tables_dir <- file.path(MANIFEST_DIR, "tables")
    if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

    csvs <- list.files(OUTPUTS_DIR, pattern = "\\.csv$", full.names = TRUE)
    index_tables <- list()
    for (p in csvs) {
        dt <- tryCatch(fread(p), error = function(e) NULL)
        if (is.null(dt)) next
        offending <- grep("^(ID|STUDENT_ID|STN)$", names(dt),
            value = TRUE, ignore.case = TRUE)
        if (length(offending) > 0) {
            message(sprintf("[manifest] skipping %s (id-like columns)", basename(p)))
            next
        }
        stem <- sub("\\.csv$", "", basename(p))
        write_json(dt, file.path("manifests", "tables", paste0(stem, ".json")))
        index_tables[[length(index_tables) + 1L]] <- list(
            name = stem,
            csv = file.path("outputs", basename(p)),
            json = file.path("outputs", "manifests", "tables", paste0(stem, ".json")),
            n_rows = nrow(dt),
            columns = names(dt)
        )
    }

    figs <- figure_catalog()
    index <- list(
        schema = "in.ilearn.wida.manifest.v1",
        generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        wida_label = "WIDA ACCESS Overall Composite",
        containment = "Aggregates only. cache/ is gitignored and is not listed here.",
        tables = index_tables,
        figures = lapply(seq_len(nrow(figs)), function(i) {
            list(
                file = figs$file[i],
                path = figs$path[i],
                pdf = file.path("figures", paste0(figs$stem[i], ".pdf"))
            )
        })
    )
    write_json(index, file.path("manifests", "index.json"))

    results <- build_results_manifest()
    write_json(results, file.path("manifests", "results.json"))
    message("[manifest] wrote outputs/manifests/{index,results}.json and tables/*.json")
    invisible(file.path(MANIFEST_DIR, "results.json"))
}
