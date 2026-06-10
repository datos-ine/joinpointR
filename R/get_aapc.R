#' Average Annual Percent Change (AAPC)
#'
#' Estimates the Average Annual Percent Change (AAPC) and its corresponding
#' 95% confidence interval for one or more regression models. Optionally,
#' statistical significance can be displayed using significance stars instead
#' of confidence intervals.
#'
#' @param mods A joinpoint regression model, a list of joinpoint regression
#'   models returned by \code{model_jp()}, or linear regression models (`lm`
#'   objects).
#' @param digits Integer. Number of decimal places used to display the results.
#' @param show_ci Logical. If `TRUE`, displays the 95% confidence interval.
#'   If `FALSE`, displays significance stars.
#' @param dec Character. Decimal separator to use (`"."` or `","`).
#'
#' @return
#' A tibble with one row per model containing the estimated AAPC and either
#' its 95% confidence interval or significance stars.
#'
#' @author Tamara Ricardo
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Fit joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = "hiv_rate",
#'   time = "year",
#'   group = "region",
#'   k = 2,
#'   test = TRUE
#' )
#'
#' # AAPC with 95% confidence intervals
#' get_aapc(mods, digits = 1, show_ci = TRUE, dec = ".")
#'
#' # AAPC with significance stars
#' get_aapc(mods, show_ci = FALSE)
#'
#' # AAPC for a single model
#' get_aapc(mods$Central)
#'
#' @export

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

        AAPC <- unname(aapc_obj[grep("Est", names(aapc_obj))])
        CI_low <- unname(aapc_obj[grep("\\.l", names(aapc_obj))])
        CI_upp <- unname(aapc_obj[grep("\\.u", names(aapc_obj))])
      } else {
        ## ---- Linear models ----
        beta <- stats::coef(mod)[2]
        AAPC <- exp(beta) - 1

        CI <- stats::confint(mod)[2, ]
        CI_low <- exp(CI[1]) - 1
        CI_upp <- exp(CI[2]) - 1
      }

      stars <- ifelse(CI_low > 0 | CI_upp < 0, "*", "")

      # ---- Return object ----
      tibble::tibble(
        AAPC = if (show_ci) {
          fmt_ci(AAPC, CI_low, CI_upp)
        } else {
          fmt_stars(AAPC, stars)
        }
      )
    },
    .id = "model"
  )
}
