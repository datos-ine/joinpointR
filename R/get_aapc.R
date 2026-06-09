#' Average Annual Percent Change (AAPC)
#'
#' Estimates the Average Annual Percent Change (AAPC) and its 95% confidence
#' interval. Optionally displays statistical significance using significance
#' stars.
#'
#' @param mod Joinpoint regression models (segmented objects) or linear regression models (lm objects).
#' @param digits Number of decimal places to display (integer).
#' @param show_ci Logical; if TRUE, displays the 95% confidence interval.
#'   If FALSE, displays significance stars.
#' @param dec Character. Decimal separator to use (e.g., "." or ",").
#'
#' @return A tibble with the AAPC and either its 95% confidence
#' interval or significance stars for each model.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' names(hiv_data)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = hiv_data, value = "hiv_rate", time = "year", group = "region", k = 2, test = TRUE)
#'
#' # AAPC of the first model
#' get_aapc(mods, digits = 1, show_ci = TRUE, dec = ".")

get_aapc <- function(
  mods,
  digits = 1,
  show_ci = TRUE,
  dec = "."
) {
  # ---- Allow a single model or a list of models ----
  if (inherits(mods, c("lm", "segmented"))) {
    mods <- list(mods)
  }

  # ---- Format of 95% CI ----
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

  # ---- Format of significance stars ----
  fmt_stars <- function(x, stars) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = dec),
      ifelse(stars != "", paste0(" ", stars), "")
    )
  }

  # ---- Estimate AAPC for each model in the list ----
  purrr::map_dfr(
    mods,
    \(mod) {
      ## ---- Segmented objects ----
      if (inherits(mod, "segmented")) {
        aapc_obj <- segmented::aapc(mod)

        est <- unname(aapc_obj[grep("Est", names(aapc_obj))])
        lci <- unname(aapc_obj[grep("\\.l", names(aapc_obj))])
        uci <- unname(aapc_obj[grep("\\.u", names(aapc_obj))])
      } else {
        ## ---- Linear models ----
        beta <- stats::coef(mod)[2]
        est <- exp(beta) - 1

        ci <- stats::confint(mod)[2, ]
        lci <- exp(ci[1]) - 1
        uci <- exp(ci[2]) - 1
      }

      stars <- ifelse(lci > 0 | uci < 0, "*", "")

      # ---- Return object ----
      tibble::tibble(
        AAPC = if (show_ci) {
          fmt_ci(est, lci, uci)
        } else {
          fmt_stars(est, stars)
        }
      )
    },
    .id = "model"
  )
}
