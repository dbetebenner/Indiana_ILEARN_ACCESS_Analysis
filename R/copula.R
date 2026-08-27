###########################################################################
### R/copula.R — confirmatory copula fitting (t / Frank / Gaussian) and
### tail-dependence extraction. Family selection is settled upstream in
### Copula_Sensitivity_Analyses; here we fit a small set for description,
### not another AIC bake-off. No parametric-bootstrap GoF (slow) unless a
### later pass needs it.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
    library(copula)
})

#' Construct an unfitted copula object for a family name
#'
#' @param family "t", "frank", or "gaussian".
#' @return A copula object suitable for fitCopula().
make_copula <- function(family) {
    switch(family,
        t        = tCopula(dim = 2, dispstr = "un", df.fixed = FALSE),
        gaussian = normalCopula(dim = 2, dispstr = "un"),
        frank    = frankCopula(dim = 2),
        stop("Unknown copula family: ", family, call. = FALSE)
    )
}

#' Fit one copula family to a matrix of pseudo-observations
#'
#' @param u Two-column matrix of pseudo-observations in (0, 1).
#' @param family Family name.
#' @return data.table row: family, n, loglik, aic, bic, tau, rho, df,
#'   lambda_lower, lambda_upper, converged.
fit_one_copula <- function(u, family) {
    n <- nrow(u)
    empty <- data.table(
        family = family, n = n, loglik = NA_real_, aic = NA_real_,
        bic = NA_real_, tau = NA_real_, rho = NA_real_, df = NA_real_,
        lambda_lower = NA_real_, lambda_upper = NA_real_, converged = FALSE
    )
    if (n < MIN_N_COPULA_CELL) return(empty)

    cop <- make_copula(family)
    fit <- tryCatch(
        fitCopula(cop, data = u, method = "mpl"),
        error = function(e) NULL
    )
    if (is.null(fit)) {
        fit <- tryCatch(fitCopula(cop, data = u, method = "itau"),
            error = function(e) NULL)
    }
    if (is.null(fit)) return(empty)

    fitted_cop <- fit@copula
    k <- length(fit@estimate)
    ll <- as.numeric(logLik(fit))
    tail_dep <- tryCatch(lambda(fitted_cop), error = function(e) c(lower = NA, upper = NA))
    df_val <- if (family == "t") {
        tryCatch(fitted_cop@parameters[length(fitted_cop@parameters)], error = function(e) NA_real_)
    } else NA_real_

    data.table(
        family = family,
        n = n,
        loglik = ll,
        aic = tryCatch(AIC(fit), error = function(e) -2 * ll + 2 * k),
        bic = tryCatch(BIC(fit), error = function(e) -2 * ll + log(n) * k),
        tau = tryCatch(tau(fitted_cop), error = function(e) NA_real_),
        rho = tryCatch(rho(fitted_cop), error = function(e) NA_real_),
        df = df_val,
        lambda_lower = unname(tail_dep["lower"]),
        lambda_upper = unname(tail_dep["upper"]),
        converged = TRUE
    )
}

#' Fit all configured families to one pseudo-observation sample
#'
#' @param u Two-column matrix of pseudo-observations.
#' @param families Character vector of family names.
#' @return data.table stacking fit_one_copula() rows, best flag by AIC.
fit_copulas <- function(u, families = COPULA_FAMILIES) {
    res <- rbindlist(lapply(families, function(f) fit_one_copula(u, f)))
    if (any(res$converged)) {
        res[, best_aic := converged & aic == min(aic[converged], na.rm = TRUE)]
    } else {
        res[, best_aic := FALSE]
    }
    res[]
}

#' Fit one family and return the fitted copula object (or NULL)
#'
#' @param u Two-column matrix of pseudo-observations.
#' @param family Family name.
#' @return A fitted copula, or NULL if both mpl and itau fail.
fit_copula_object <- function(u, family) {
    cop <- make_copula(family)
    fit <- tryCatch(fitCopula(cop, data = u, method = "mpl"),
        error = function(e) NULL)
    if (is.null(fit)) {
        fit <- tryCatch(fitCopula(cop, data = u, method = "itau"),
            error = function(e) NULL)
    }
    if (is.null(fit)) return(NULL)
    fit@copula
}

#' Fit the configured families and return the AIC-best copula object
#'
#' @param u Two-column matrix of pseudo-observations.
#' @return list(family, copula, fits). `copula` is NULL if nothing converged.
fit_best_copula_object <- function(u, families = COPULA_FAMILIES) {
    fits <- fit_copulas(u, families)
    if (!any(fits$converged)) {
        return(list(family = NA_character_, copula = NULL, fits = fits))
    }
    fam <- fits[best_aic == TRUE, family][1]
    list(family = fam, copula = fit_copula_object(u, fam), fits = fits)
}

#' Conditional P(V >= v_star | U = u) from a fitted copula
#'
#' Uses `copula::cCopula()` (Rosenblatt / h-function): index 2 is
#' C_{2|1}(v|u) = P(V <= v | U = u).
#'
#' @param cop Fitted copula object.
#' @param u Numeric vector of conditioning U values in (0,1).
#' @param v_star Scalar threshold on the V (ILEARN rank) scale.
#' @return Numeric vector of conditional probabilities.
copula_p_v_ge_given_u <- function(cop, u, v_star) {
    if (is.null(cop) || !is.finite(v_star)) return(rep(NA_real_, length(u)))
    v_star <- min(max(v_star, 1e-6), 1 - 1e-6)
    u <- pmin(pmax(u, 1e-6), 1 - 1e-6)
    out <- tryCatch({
        p_le <- as.numeric(cCopula(cbind(u, v_star), copula = cop,
            indices = 2, drop = TRUE))
        1 - p_le
    }, error = function(e) rep(NA_real_, length(u)))
    out
}

#' Invert the copula conditional for the U at which P(V >= v_star | U) = target
#'
#' @param cop Fitted copula.
#' @param v_star ILEARN-rank location of the proficiency cut.
#' @param target Target conditional probability (default 0.5).
#' @param grid U-grid used for interpolation.
#' @return list(u_star, p_at_u_star, reached).
copula_u_for_target_p <- function(cop, v_star, target = 0.5,
                                  grid = seq(0.02, 0.98, by = 0.01)) {
    empty <- list(u_star = NA_real_, p_at_u_star = NA_real_, reached = FALSE)
    if (is.null(cop) || !is.finite(v_star)) return(empty)
    p <- copula_p_v_ge_given_u(cop, grid, v_star)
    ok <- is.finite(p)
    if (sum(ok) < 4) return(empty)
    grid <- grid[ok]; p <- p[ok]
    ## Positive dependence: p should rise with u. If the curve never
    ## crosses `target`, the 50-50 cut is outside the observed range.
    if (max(p) < target) {
        return(list(u_star = NA_real_, p_at_u_star = max(p), reached = FALSE))
    }
    if (min(p) > target) {
        return(list(u_star = grid[1], p_at_u_star = p[1], reached = TRUE))
    }
    u_star <- tryCatch(approx(p, grid, xout = target, ties = "ordered")$y,
        error = function(e) NA_real_)
    p_star <- if (is.finite(u_star)) {
        copula_p_v_ge_given_u(cop, u_star, v_star)[1]
    } else NA_real_
    list(u_star = u_star, p_at_u_star = p_star, reached = is.finite(u_star))
}

#' Map a U-scale location back to a WIDA proficiency level
#'
#' @param u_star Location on the WIDA pseudo-observation scale.
#' @param dt data.table with U_WIDA and WIDA_PL.
#' @return Median WIDA_PL among observations nearest u_star.
u_star_to_pl <- function(u_star, dt) {
    if (!is.finite(u_star)) return(NA_real_)
    d <- dt[is.finite(U_WIDA) & is.finite(WIDA_PL)]
    if (nrow(d) < 10) return(NA_real_)
    near <- d[abs(U_WIDA - u_star) <= 0.025]
    if (nrow(near) < 8) {
        near <- d[order(abs(U_WIDA - u_star))][1:min(80L, nrow(d))]
    }
    median(near$WIDA_PL, na.rm = TRUE)
}

#' Locate the ILEARN proficiency cut on the paired-sample V (rank) scale
#'
#' If achievement labels are ordered by score, this is the share of the
#' paired sample below the lowest proficient score. Overlapping labels
#' fall back to 1 - P(proficient).
#'
#' @param score Numeric ILEARN scores.
#' @param proficient Logical proficiency flags.
#' @return Scalar in (0, 1).
v_star_from_proficient <- function(score, proficient) {
    ok <- is.finite(score) & !is.na(proficient)
    score <- score[ok]
    proficient <- as.logical(proficient[ok])
    if (length(score) < 10L || !any(proficient) || !any(!proficient)) {
        return(mean(!proficient, na.rm = TRUE))
    }
    cut_lo <- min(score[proficient])
    cut_hi <- max(score[!proficient])
    if (is.finite(cut_lo) && is.finite(cut_hi) && cut_lo > cut_hi) {
        return(mean(score < cut_lo))
    }
    mean(!proficient)
}
