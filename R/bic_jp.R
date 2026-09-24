#' Bayesian Information Criteria (BIC) for joinpoint regression models
#'
#' @description
#' Computes the Bayesian Information Criterion (BIC),
#' the penalized BIC (BIC3), and the weighted BIC (WBIC) for
#' models of class \code{model_jp}.
#'
#' @param mod Model of class \code{model_jp}.
#'
#' @return A number or a list of numbers containing the BIC values.
#'
#' @details
#' The Bayesian Information Criterion (BIC) for $k$ joinpoints is estimated as:
#'
#'  \deqn{BIC = \log(MSE) + \frac{param(k)}{n} * \log{n}}
#'
#' where \code{MSE} is the mean squared error of the fitted model,
#' \code{param(k)} is the number of parameters in the model, and \code{n} is the
#' number of observations.
#'
#' The number of parameters for the standard BIC is defined as:
#' \deqn{param(k) = 2k + 2}
#'
#' For BIC3, the penalty parameter is defined as:
#' \deqn{param(k) = 3k + 2}
#'
#' The weighted Bayesian Information Criterion (WBIC) combines BIC and BIC3 using
#' a weighted penalty term based on the data:
#'
#' \deqn{WBIC = BIC * (1 - wt) + BIC3 * wt}
#'
#' where \code{wt} is the calculated weighted penalty.
#'
#' @examples
#' # Create a reduced dataset
#' data <- hiv_data |>
#' dplyr::filter(admin == "ARG")
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year, group = "sex")
#'
#' bic_jp(mods$Female)
#'
#' @keywords internal
calc_bic_jp <- function(
  mod
) {
  # --- Mean squared errors ---
  mse <- mean(stats::residuals(mod)^2)

  # --- Number of observations ---
  n <- stats::nobs(mod)

  # --- Number of parameters ---
  n_jp <- length(mod$.jp_k)

  # --- Weights ---
  if (n_jp == 0) {
    wt <- 0
  } else {
    md <- stats::model.frame(mod)

    # Base SSE
    sse_base <- sum(
      stats::residuals(object = lm(y ~ x, data = md))^2
    )

    # Hinge variable names
    u_col <- md |>
      dplyr::select(dplyr::starts_with("U")) |>
      colnames()

    # Partial R2
    wt <- u_col |>
      purrr::map_dbl(function(u_var) {
        mod_u <- stats::lm(y ~ ., data = md)

        sse_u <- sum(stats::residuals(mod_u)^2)

        (sse_base - sse_u) / sse_base
      }) |>
      max(na.rm = TRUE)
  }

  # ---- BIC ----
  bic <- log(mse) + ((2 * n_jp + 2) / n) * log(n)

  # ---- BIC3 ----
  bic3 <- log(mse) + ((3 * n_jp + 2) / n) * log(n)

  # ---- WBIC ----
  wbic <- (bic * (1 - wt)) + (bic3 * wt)

  # ---- Return ----
  data.frame(
    BIC = bic,
    BIC3 = bic3,
    WBIC = wbic,
    Weight = wt
  )
}

#' @export
bic_jp <- function(mod) {
  calc_bic_jp(mod$fit)
}
