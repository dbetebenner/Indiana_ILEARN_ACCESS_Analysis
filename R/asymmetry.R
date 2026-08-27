###########################################################################
### R/asymmetry.R — copula ASYMMETRY diagnostics.
###
### Why this exists. The confirmatory families fit upstream (t, Frank,
### Gaussian) are all EXCHANGEABLE  [ C(u,v) = C(v,u) ]  and RADIALLY
### SYMMETRIC  [ C(u,v) = hat-C(u,v) = u+v-1 + C(1-u,1-v) ].  By
### construction they impose ZERO asymmetry and force lower-tail
### dependence to equal upper-tail dependence (lambda_L = lambda_U).
###
### But the signal-vs-noise hypothesis IS an asymmetry claim: the
### lower-left corner (both tests low -- the WIDA floor) is near-
### independent NOISE, while the upper-right corner (both high) is
### coupled SIGNAL. A radially symmetric copula cannot represent that.
### So the departure-from-symmetry is not a nuisance to be smoothed
### away -- it is the quantity of interest. This file measures it.
###
### Everything here is EMPIRICAL (rank based). No MPL re-fitting, so it
### is fast and inherits the scale-invariance that makes ranks the
### primary metric in this pipeline.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

#' Empirical copula on a rectangular grid
#'
#' C_n(a,b) = (1/n) sum 1{ U_i <= a, V_i <= b }. Inputs may be raw scores
#' or pseudo-observations; the copula is a rank object, so it is re-ranked
#' to (0,1) internally and the two are equivalent.
#'
#' @param u,v Numeric vectors (scores or pseudo-observations).
#' @param grid Node points in (0,1) for both axes.
#' @return data.table(u, v, C) over the grid crossing.
empirical_copula_grid <- function(u, v, grid = seq(0.05, 0.95, by = 0.05)) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    n <- length(u)
    if (n < 3L) {
        g <- CJ(u = grid, v = grid)
        g[, C := NA_real_]
        return(g[])
    }
    U <- rank(u, ties.method = "average") / (n + 1)
    V <- rank(v, ties.method = "average") / (n + 1)
    g <- CJ(u = grid, v = grid)
    g[, C := vapply(seq_len(.N), function(i) mean(U <= u[i] & V <= v[i]),
        numeric(1))]
    g[]
}

#' Radial-asymmetry surface  D_n(a,b) = C_n(a,b) - C_n^refl(a,b)
#'
#' C_n^refl is the empirical copula of the reflected sample (1-U, 1-V).
#' Under radial symmetry the two empirical copulas share a limit, so
#' D_n is ~0 everywhere. D_n(a,b) > 0 means MORE joint mass accumulates
#' in the lower-left box [0,a]x[0,b] than in the mirror-image upper-right
#' box -- i.e. lower-tail concordance exceeds upper-tail concordance.
#' D_n < 0 is the reverse (upper-tail / "signal-corner" dominance).
#'
#' @param u,v Numeric vectors.
#' @param grid Node points in (0,1).
#' @return data.table(u, v, C, C_refl, D).
radial_asymmetry_surface <- function(u, v, grid = seq(0.05, 0.95, by = 0.05)) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    C   <- empirical_copula_grid(u, v, grid)
    Cr  <- empirical_copula_grid(1 - u, 1 - v, grid)
    out <- C[, .(u, v, C)]
    out[, C_refl := Cr$C]
    out[, D := C - C_refl]
    out[]
}

#' Nonparametric tail-dependence coefficients and their asymmetry
#'
#' Tail-concordance estimators at a small threshold t:
#'   lambda_L(t) = P(V <= t | U <= t)  ~=  C_n(t,t) / t
#'   lambda_U(t) = P(V >= 1-t | U >= 1-t)  ~=  (1 - 2t + C_n(1-t,1-t)) / (1 - t)
#' The parametric fits force lambda_L = lambda_U (t) or 0 (Frank/Gaussian);
#' the gap lambda_U - lambda_L is exactly what those families erase.
#'
#' @param u,v Numeric vectors.
#' @param t Tail threshold (default 0.05); averaged over a small band for
#'   stability.
#' @return list(lambda_lower, lambda_upper, tail_asymmetry).
tail_dependence_np <- function(u, v, t = 0.05) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    n <- length(u)
    if (n < 50L) {
        return(list(lambda_lower = NA_real_, lambda_upper = NA_real_,
            tail_asymmetry = NA_real_))
    }
    U <- rank(u, ties.method = "average") / (n + 1)
    V <- rank(v, ties.method = "average") / (n + 1)
    ts <- seq(max(t - 0.02, 0.02), t + 0.02, by = 0.005)
    lamL <- mean(vapply(ts, function(tt) mean(U <= tt & V <= tt) / tt, numeric(1)))
    lamU <- mean(vapply(ts, function(tt) mean(U >= 1 - tt & V >= 1 - tt) / tt, numeric(1)))
    list(lambda_lower = lamL, lambda_upper = lamU,
        tail_asymmetry = lamU - lamL)
}

#' Scalar copula-asymmetry summary for one bivariate sample
#'
#' Radial index    R = sup |D_n(a,b)|             (0 iff radially symmetric)
#' Radial CvM      S = mean D_n(a,b)^2 over grid   (magnitude, integrated)
#' Exchangeability mu = sup |C_n(a,b) - C_n(b,a)|  (0 iff exchangeable)
#' Tail asymmetry  lambda_U - lambda_L
#' Corner tau gap  tau(upper-right quadrant) - tau(lower-left quadrant)
#'
#' @param u,v Numeric vectors.
#' @param grid Node points in (0,1).
#' @param min_n Minimum N to report.
#' @return one-row data.table of scalars.
copula_asymmetry_scalars <- function(u, v, grid = seq(0.05, 0.95, by = 0.05),
                                     min_n = MIN_N_LEVEL_SLICE) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    n <- length(u)
    empty <- data.table(n = n, radial_index = NA_real_, radial_cvm = NA_real_,
        exch_index = NA_real_, lambda_lower = NA_real_, lambda_upper = NA_real_,
        tail_asymmetry = NA_real_, tau_ll = NA_real_, tau_ur = NA_real_,
        corner_tau_gap = NA_real_)
    if (n < min_n) return(empty)

    surf <- radial_asymmetry_surface(u, v, grid)
    radial_index <- max(abs(surf$D), na.rm = TRUE)
    radial_cvm   <- mean(surf$D^2, na.rm = TRUE)

    ## Exchangeability: compare C(a,b) with C(b,a) on the grid.
    ec <- surf[, .(u, v, C)]
    setkey(ec, u, v)
    ec_swap <- ec[ec[, .(u = v, v = u)], on = .(u, v)]
    exch_index <- max(abs(ec$C - ec_swap$C), na.rm = TRUE)

    td <- tail_dependence_np(u, v)

    U <- rank(u, ties.method = "average") / (n + 1)
    V <- rank(v, ties.method = "average") / (n + 1)
    ll <- U < 0.5 & V < 0.5
    ur <- U >= 0.5 & V >= 0.5
    tau_ll <- if (sum(ll) >= min_n) kendall_tau(U[ll], V[ll]) else NA_real_
    tau_ur <- if (sum(ur) >= min_n) kendall_tau(U[ur], V[ur]) else NA_real_

    data.table(
        n = n,
        radial_index = radial_index,
        radial_cvm = radial_cvm,
        exch_index = exch_index,
        lambda_lower = td$lambda_lower,
        lambda_upper = td$lambda_upper,
        tail_asymmetry = td$tail_asymmetry,
        tau_ll = tau_ll,
        tau_ur = tau_ur,
        corner_tau_gap = tau_ur - tau_ll
    )
}

#' Upper- and lower-tail dependence FUNCTIONS across thresholds
#'
#' lambda_L(t) = P(V <= t | U <= t)  and  lambda_U(t) = P(V >= 1-t | U >= 1-t)
#' for t in (0, 0.5). Under radial symmetry the two functions coincide.
#' Their separation -- upper above lower -- is the "signal corner more
#' coupled than noise corner" statement, made rigorously and shown as it
#' emerges toward the tail (t -> 0). The parametric families force these
#' to be equal (t) or both zero (Frank/Gaussian), so they cannot show it.
#'
#' @param u,v Numeric vectors.
#' @param ts Threshold grid in (0, 0.5).
#' @return data.table(t, lambda, tail) with tail in {"upper", "lower"}.
tail_dependence_curve <- function(u, v, ts = seq(0.02, 0.5, by = 0.02)) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    n <- length(u)
    U <- rank(u, ties.method = "average") / (n + 1)
    V <- rank(v, ties.method = "average") / (n + 1)
    lamL <- vapply(ts, function(t) mean(U <= t & V <= t) / t, numeric(1))
    lamU <- vapply(ts, function(t) mean(U >= 1 - t & V >= 1 - t) / t, numeric(1))
    rbindlist(list(
        data.table(t = ts, lambda = lamL, tail = "lower (noise corner)"),
        data.table(t = ts, lambda = lamU, tail = "upper (signal corner)")
    ))
}
