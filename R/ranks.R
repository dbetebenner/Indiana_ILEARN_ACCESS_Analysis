###########################################################################
### R/ranks.R — pseudo-observations (within-cell ranks) and rank-based
### dependence measures. Rank transforms are invariant to the 2026 scale
### changes, which is why they are the primary metric.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

#' Pseudo-observations for a numeric vector (ranks scaled to (0,1))
#'
#' Uses the standard n/(n+1) scaling so no value maps to exactly 0 or 1.
#' Ties are averaged.
#'
#' @param x Numeric vector.
#' @return Numeric vector of pseudo-observations on (0, 1).
pobs_vec <- function(x) {
    r <- rank(x, na.last = "keep", ties.method = "average")
    n <- sum(!is.na(x))
    r / (n + 1)
}

#' Add within-(year, grade) pseudo-observations to a pairs data.table
#'
#' @param pairs data.table with WIDA_SCORE, ILEARN_SCORE, YEAR, GRADE.
#' @param wida_col Column to rank for the WIDA margin.
#' @param ilearn_col Column to rank for the ILEARN margin.
#' @return The data.table with U_WIDA and V_ILEARN columns added (by ref).
add_pobs <- function(pairs, wida_col = "WIDA_SCORE",
                     ilearn_col = "ILEARN_SCORE") {
    pairs[, U_WIDA   := pobs_vec(get(wida_col)),   by = .(YEAR, GRADE)]
    pairs[, V_ILEARN := pobs_vec(get(ilearn_col)), by = .(YEAR, GRADE)]
    pairs[]
}

#' Within-year percentile (0-100) of a score, by (year, grade)
#'
#' @param score Numeric vector.
#' @return Numeric percentile in [0, 100].
percentile_within <- function(score) {
    100 * (rank(score, na.last = "keep", ties.method = "average") /
        (sum(!is.na(score)) + 1))
}

#' Fast Kendall's tau (O(n log n) via pcaPP::cor.fk when available)
#'
#' @param u,v Numeric vectors of equal length.
#' @return Scalar Kendall's tau, or NA if n < 3.
kendall_tau <- function(u, v) {
    ok <- is.finite(u) & is.finite(v)
    n <- sum(ok)
    if (n < 3) return(NA_real_)
    if (requireNamespace("pcaPP", quietly = TRUE)) {
        return(as.numeric(pcaPP::cor.fk(u[ok], v[ok])))
    }
    suppressWarnings(cor(u[ok], v[ok], method = "kendall"))
}

#' Rank-based dependence measures for a bivariate sample
#'
#' @param u,v Numeric vectors (raw scores or pseudo-observations; rank
#'   measures are identical either way).
#' @param dcor Logical; compute distance correlation if the energy package
#'   is available.
#' @return Named list: n, kendall, spearman, pearson, dcor.
dependence_measures <- function(u, v, dcor = FALSE) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    n <- length(u)
    out <- list(
        n        = n,
        kendall  = if (n >= 3) kendall_tau(u, v) else NA_real_,
        spearman = if (n >= 3) suppressWarnings(cor(u, v, method = "spearman")) else NA_real_,
        pearson  = if (n >= 3) suppressWarnings(cor(u, v, method = "pearson"))  else NA_real_,
        dcor     = NA_real_
    )
    if (dcor && n >= 3 && isTRUE(HAS_ENERGY)) {
        out$dcor <- tryCatch(energy::dcor(u, v), error = function(e) NA_real_)
    }
    out
}
