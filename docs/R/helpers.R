### Formatting helpers for the ILEARN × WIDA ACCESS deck.
### The deck reads outputs/manifests/results.json only — never student data.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

fmt_count <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  prettyNum(round(as.numeric(x[1])), big.mark = ",")
}

fmt_pct <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  paste0(formatC(100 * as.numeric(x[1]), format = "f", digits = digits), "%")
}

#' Format a probability on [0, 1], not a percent
fmt_prob <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  formatC(as.numeric(x[1]), format = "f", digits = digits)
}

fmt_num <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x[1])) return("—")
  formatC(as.numeric(x[1]), format = "f", digits = digits)
}

fmt_tau <- function(x) fmt_num(x, 2)

load_results <- function(path = "../outputs/manifests/results.json") {
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run `Rscript run_all.R 8` first.", call. = FALSE)
  }
  fromJSON(path, simplifyVector = TRUE, flatten = FALSE)
}

as_dt <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.table())
  as.data.table(x)
}

fig_src <- function(stem) {
  file.path("..", "figures", paste0(stem, ".png"))
}

fig_html <- function(stem, caption, alt = NULL, max_height = "620px") {
  src <- fig_src(stem)
  if (!file.exists(src)) {
    return(sprintf('<div style="padding:1em;color:#c0392b;font-size:0.6em">missing: %s</div>', src))
  }
  if (is.null(alt)) alt <- caption
  sprintf(
    '<figure style="margin:0;text-align:center">
       <img src="%s" alt="%s" style="max-height:%s;width:auto;max-width:100%%;display:block;margin:0 auto"/>
       <figcaption>%s</figcaption>
     </figure>', src, alt, max_height, caption)
}

emit_html <- function(x) cat("\n```{=html}\n", x, "\n```\n", sep = "")
