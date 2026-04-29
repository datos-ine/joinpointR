#' Average Annual Percent Change (AAPC)
#'
#' Estimates the Average Annual Percent Change (AAPC) and its 95% confidence
#' interval. Optionally displays statistical significance using significance
#' stars.
#'
#' @param mod Joinpoint regression model (segmented object) or linear regression model (lm object).
#' @param digits Number of decimal places to display (integer).
#' @param show_ci Logical; if TRUE, displays the 95% confidence interval.
#'   If FALSE, displays significance stars.
#' @param dec Character. Decimal separator to use (e.g., "." or ",").
#'
#' @return A character string with the AAPC and either its 95% confidence
#' interval or significance stars.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Load example data
#' data("plant", package = "segmented")
#'
#' names(plant)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = plant, value = "y", time = "time", group = "group", k = 2, test = TRUE)
#'
#' # AAPC of the first model
#' get_aapc(mods$RKW, digits = 1, show_ci = TRUE, dec = ".")

get_aapc <- function(mod, digits = 1, show_ci = TRUE, dec = ".") {
  # ---- Validations ----
  if (!inherits(mod, c("segmented", "lm"))) {
    stop("`mod` must be an object of class 'segmented' or 'lm'")
  }

  # ---- Helpers ----
  fmt_ci <- function(x, y, z) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = dec),
      " (",
      scales::percent(y, accuracy = 10^-digits, decimal.mark = dec),
      "; ",
      scales::percent(z, accuracy = 10^-digits, decimal.mark = dec),
      ")"
    )
  }

  fmt_stars <- function(x, stars) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = dec),
      ifelse(stars != "", paste0(" ", stars), "")
    )
  }

  # ---- Calculations ----
  if (inherits(mod, "segmented")) {
    aapc_obj <- segmented::aapc(mod)

    est <- unname(aapc_obj[grep("Est", names(aapc_obj))])
    lci <- unname(aapc_obj[grep("\\.l", names(aapc_obj))])
    uci <- unname(aapc_obj[grep("\\.u", names(aapc_obj))])
  } else {
    beta <- stats::coef(mod)[2]

    est <- exp(beta) - 1
    ci <- stats::confint(mod)[2, ]

    lci <- exp(ci[1]) - 1
    uci <- exp(ci[2]) - 1
  }

  # ---- Significance based on CI ----
  stars <- ifelse(lci > 0 | uci < 0, "*", "")

  # ---- Output ----
  if (show_ci) {
    fmt_ci(est, lci, uci)
  } else {
    fmt_stars(est, stars)
  }
}
