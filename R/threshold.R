###########################################################################
### R/threshold.R — the local / threshold machinery that operationalizes
### "signal vs noise": moving-window tau(u), a threshold scan over WIDA
### cuts, a broken-stick change-point on tau(u), and exit-criterion
### agreement statistics.
###########################################################################

suppressPackageStartupMessages({
    library(data.table)
})

#' Moving-window Kendall's tau along the WIDA pseudo-observation axis
#'
#' Slides a window of width `width` (on U in (0,1)) and computes tau within
#' each window. A curve that is ~0 at low U and rises is the signature of a
#' noise-to-signal transition.
#'
#' @param u WIDA pseudo-observations (0,1).
#' @param v ILEARN pseudo-observations (0,1).
#' @param width Window width on the U scale.
#' @param step Window centre spacing.
#' @param min_n Minimum window N to report a tau.
#' @return data.table: u_center, n, tau.
local_tau_curve <- function(u, v, width = LOCAL_WINDOW_WIDTH,
                            step = LOCAL_WINDOW_STEP, min_n = MIN_N_LEVEL_SLICE) {
    ok <- is.finite(u) & is.finite(v)
    u <- u[ok]; v <- v[ok]
    centers <- seq(width / 2, 1 - width / 2, by = step)
    rows <- lapply(centers, function(c0) {
        lo <- c0 - width / 2
        hi <- c0 + width / 2
        idx <- u >= lo & u < hi
        nn <- sum(idx)
        tau <- if (nn >= min_n) kendall_tau(u[idx], v[idx]) else NA_real_
        data.table(u_center = c0, n = nn, tau = tau)
    })
    rbindlist(rows)
}

#' Threshold scan: dependence and outcomes below vs at/above each WIDA cut
#'
#' For each candidate Overall cut `c`, split on WIDA proficiency level and
#' compare rank dependence and ILEARN outcomes on the two sides.
#'
#' @param dt data.table with WIDA_PL, U_WIDA, V_ILEARN, ILEARN_PCTILE,
#'   ILEARN_PROFICIENT (logical).
#' @param cuts Numeric candidate cuts.
#' @param min_n Minimum side-N to report.
#' @return data.table keyed by cut with below/above stats.
threshold_scan <- function(dt, cuts = WIDA_CUT_GRID, min_n = MIN_N_LEVEL_SLICE) {
    d <- dt[is.finite(WIDA_PL) & is.finite(U_WIDA) & is.finite(V_ILEARN)]
    rows <- lapply(cuts, function(c0) {
        below <- d[WIDA_PL <  c0]
        above <- d[WIDA_PL >= c0]
        n_b <- nrow(below); n_a <- nrow(above)
        tau_b <- if (n_b >= min_n) kendall_tau(below$U_WIDA, below$V_ILEARN) else NA_real_
        tau_a <- if (n_a >= min_n) kendall_tau(above$U_WIDA, above$V_ILEARN) else NA_real_
        data.table(
            cut = c0,
            n_below = n_b,
            n_above = n_a,
            tau_below = tau_b,
            tau_above = tau_a,
            tau_gap = tau_a - tau_b,
            ilearn_prof_below = if (n_b >= min_n) mean(below$ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_,
            ilearn_prof_above = if (n_a >= min_n) mean(above$ILEARN_PROFICIENT, na.rm = TRUE) else NA_real_,
            ilearn_pctile_below = if (n_b >= min_n) median(below$ILEARN_PCTILE, na.rm = TRUE) else NA_real_,
            ilearn_pctile_above = if (n_a >= min_n) median(above$ILEARN_PCTILE, na.rm = TRUE) else NA_real_
        )
    })
    rbindlist(rows)
}

#' Broken-stick change-point on a tau(u) curve
#'
#' Finds the breakpoint that minimizes the residual sum of squares of a
#' two-segment continuous piecewise-linear fit. Uses segmented if available
#' for a refined estimate; otherwise the grid-search breakpoint stands.
#'
#' @param curve data.table from local_tau_curve() (u_center, tau, n).
#' @return list: u_break, slope_left, slope_right, method.
tau_changepoint <- function(curve) {
    d <- curve[is.finite(tau)]
    if (nrow(d) < 5) {
        return(list(u_break = NA_real_, slope_left = NA_real_,
            slope_right = NA_real_, method = "insufficient"))
    }
    x <- d$u_center; y <- d$tau

    ## Grid search over interior candidate breakpoints.
    cand <- x[x > quantile(x, 0.1) & x < quantile(x, 0.9)]
    best <- list(rss = Inf, brk = NA_real_)
    for (b in cand) {
        seg <- pmax(x - b, 0)
        fit <- tryCatch(lm(y ~ x + seg), error = function(e) NULL)
        if (is.null(fit)) next
        rss <- sum(residuals(fit)^2)
        if (rss < best$rss) best <- list(rss = rss, brk = b, fit = fit)
    }
    if (!is.finite(best$brk)) {
        return(list(u_break = NA_real_, slope_left = NA_real_,
            slope_right = NA_real_, method = "failed"))
    }

    method <- "grid"
    u_break <- best$brk
    co <- coef(best$fit)
    slope_left <- unname(co["x"])
    slope_right <- unname(co["x"] + co["seg"])

    if (isTRUE(HAS_SEGMENTED)) {
        ref <- tryCatch({
            lin <- lm(y ~ x)
            sg <- segmented::segmented(lin, seg.Z = ~x, psi = best$brk)
            list(psi = as.numeric(sg$psi[, "Est."]),
                 slopes = segmented::slope(sg)$x[, "Est."])
        }, error = function(e) NULL)
        if (!is.null(ref) && is.finite(ref$psi[1])) {
            u_break <- ref$psi[1]
            slope_left <- ref$slopes[1]
            slope_right <- ref$slopes[2]
            method <- "segmented"
        }
    }

    list(u_break = u_break, slope_left = slope_left,
        slope_right = slope_right, method = method)
}

#' Map a WIDA pseudo-observation break back to a proficiency-level cut
#'
#' @param u_break Break location on the U (0,1) scale.
#' @param dt data.table with U_WIDA and WIDA_PL.
#' @return Numeric WIDA proficiency level at the break (interpolated median).
u_break_to_pl <- function(u_break, dt) {
    if (!is.finite(u_break)) return(NA_real_)
    d <- dt[is.finite(U_WIDA) & is.finite(WIDA_PL)]
    if (nrow(d) < 10) return(NA_real_)
    ## PL of the observations nearest the break on the U scale.
    near <- d[abs(U_WIDA - u_break) <= 0.02]
    if (nrow(near) < 5) near <- d[order(abs(U_WIDA - u_break))][1:min(50, nrow(d))]
    median(near$WIDA_PL, na.rm = TRUE)
}

#' Exit-criterion agreement between "WIDA >= cut" and an ILEARN outcome
#'
#' @param wida_pl Numeric WIDA proficiency levels.
#' @param outcome Logical ILEARN outcome (e.g. proficient, or pctile>=50).
#' @param cut Numeric WIDA cut.
#' @return data.table: cut, n, sens, spec, youden, ppv, npv, accuracy.
exit_agreement <- function(wida_pl, outcome, cut) {
    ok <- is.finite(wida_pl) & !is.na(outcome)
    wida_pl <- wida_pl[ok]; outcome <- as.logical(outcome[ok])
    pred <- wida_pl >= cut
    tp <- sum(pred & outcome);  fp <- sum(pred & !outcome)
    fn <- sum(!pred & outcome); tn <- sum(!pred & !outcome)
    sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    data.table(
        cut = cut,
        n = length(outcome),
        sens = sens,
        spec = spec,
        youden = sens + spec - 1,
        ppv = if ((tp + fp) > 0) tp / (tp + fp) else NA_real_,
        npv = if ((tn + fn) > 0) tn / (tn + fn) else NA_real_,
        accuracy = (tp + tn) / length(outcome)
    )
}
