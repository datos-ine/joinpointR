#' Annual Percent Change by Segment
#'
#' Calculates the Annual Percent Change (APC) and corresponding 95% confidence
#' intervals for each segment of one or more joinpoint regression models.
#'
#' @param mods A joinpoint regression model or a list of joinpoint regression
#'   models returned by \code{model_jp()}.
#' @param digits Integer. Number of decimal places used to display the results.
#' @param dec Character. Decimal separator to use (e.g. `"."` or `","`).
#'
#' @return
#' A tibble with one row per segment and the variables
#' `model`, `segment`, `apc`, `lower`, and `upper`, where `lower`
#' and `upper` correspond to the limits of the 95\% confidence interval.
#'
#' @author Tamara Ricardo
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Filter dataset
#' hiv_data <- hiv_data |> 
#' dplyr::filter_out(sex == "Both" | admin != "ARG")
#'
#' # Fit joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = hiv_rate,
#'   time = year,
#'   group = "sex",
#'   k = 2
#' )
#'
#' # APC and 95% confidence intervals for all models
#' get_apc(mods, digits = 1, dec = ".")
#'
#' @export

get_apc <- function(
  mods,
  digits = 1,
  dec = "."
) {
  # ---- Validate input ----
  if (!is.list(mods)) {
    stop("`mods` must be a list returned by `model_jp()`.")
  }

  # ---- Estimate APC for each model ----
  purrr::map_dfr(
    mods,
    function(x) {
      mod <- x$model
      joinpoints <- x$joinpoints

      b <- stats::coef(mod)
      V <- stats::vcov(mod)

      # ---- Number of segments ----
      n_segments <- length(joinpoints) + 1

      # ---- Segment slopes ----
      slopes <- numeric(n_segments)
      slope_vars <- numeric(n_segments)

      for (i in seq_len(n_segments)) {
        # Coefficients contributing to the slope
        terms <- c(
          "time",
          if (i > 1) paste0("U", seq_len(i - 1), ".time")
        )

        # Linear combination: beta_time + beta_U1 + ...
        L <- numeric(length(b))
        names(L) <- names(b)
        L[terms] <- 1

        # Slope
        slopes[i] <- sum(L * b)

        # Variance of slope
        slope_vars[i] <- as.numeric(
          t(L) %*% V %*% L
        )
      }

      # ---- Standard errors ----
      slope_se <- sqrt(slope_vars)

      # ---- 95% confidence intervals on the log scale ----
      df <- stats::df.residual(mod)

      t_crit <- stats::qt(
        0.975,
        df = df
      )

      slope_low <- slopes - t_crit * slope_se
      slope_upp <- slopes + t_crit * slope_se

      # ---- Transform slope to APC ----
      apc <- 100 * (exp(slopes) - 1)

      ci_low <- 100 * (exp(slope_low) - 1)

      ci_upp <- 100 * (exp(slope_upp) - 1)

      # ---- Format output ----
      tibble::tibble(
        segment = as.character(seq_len(n_segments)),
        apc = scales::number(
          apc,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        ),
        lower = scales::number(
          ci_low,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        ),
        upper = scales::number(
          ci_upp,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        )
      )
    },
    .id = "model"
  )
}
