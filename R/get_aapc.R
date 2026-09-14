#' Average Annual Percent Change (AAPC)
#'
#' Estimates the Average Annual Percent Change (AAPC) and its corresponding
#' 95% confidence interval for one or more regression models. Optionally,
#' statistical significance can be displayed using significance stars instead
#' of confidence intervals.
#'
#' @param mods A list of models returned by \code{model_jp_grid()} or
#' \code{model_jp_step()}.
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
#' # Filter data
#' hiv_data <- hiv_data |>
#' dplyr::filter(sex == "Both" & admin == "ARG")
#'
#' # Fit joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = "hiv_rate",
#'   time = "year",
#'   k = 2
#' )
#'
#' # AAPC with 95% confidence intervals
#' get_aapc(mods, digits = 1, show_ci = TRUE, dec = ".")
#'
#' # AAPC with significance stars
#' get_aapc(mods, show_ci = FALSE)
#'
#' @export

get_aapc <- function(
  mods,
  digits = 1,
  show_ci = TRUE,
  dec = "."
) {
  # ---- Validate input ----
  if (!is.list(mods)) {
    stop("`mods` must be a list returned by `model_jp_grid()`.")
  }

  # ---- Format 95% CI ----
  fmt_ci <- function(x, y, z) {
    paste0(
      scales::percent(
        x,
        accuracy = 10^-digits,
        decimal.mark = dec
      ),
      " (",
      scales::percent(
        y,
        accuracy = 10^-digits,
        decimal.mark = dec
      ),
      "; ",
      scales::percent(
        z,
        accuracy = 10^-digits,
        decimal.mark = dec
      ),
      ")"
    )
  }

  # ---- Format significance stars ----
  fmt_stars <- function(x, stars) {
    paste0(
      scales::percent(
        x,
        accuracy = 10^-digits,
        decimal.mark = dec
      ),
      ifelse(stars != "", paste0(" ", stars), "")
    )
  }

  # ---- Estimate AAPC for each model ----
  purrr::map_dfr(
    mods,
    function(x) {
      mod <- x$model
      joinpoints <- x$joinpoints

      # ---- Time range ----
      time <- mod$model$time

      t_min <- min(time, na.rm = TRUE)
      t_max <- max(time, na.rm = TRUE)

      # ---- Segment slopes ----
      b <- stats::coef(mod)

      n_segments <- length(joinpoints) + 1

      slopes <- numeric(n_segments)

      for (i in seq_len(n_segments)) {
        terms <- c(
          "time",
          if (i > 1) {
            paste0("U", seq_len(i - 1), ".time")
          }
        )

        slopes[i] <- sum(b[terms])
      }

      # ---- Segment lengths ----
      breaks <- c(
        t_min,
        joinpoints,
        t_max
      )

      lengths <- diff(breaks)

      # ---- Weighted average slope ----
      beta_aapc <- sum(
        slopes * lengths
      ) /
        sum(lengths)

      # ---- AAPC ----
      AAPC <- exp(beta_aapc) - 1

      # ---- Variance of weighted slope ----
      L <- numeric(length(b))
      names(L) <- names(b)

      for (i in seq_len(n_segments)) {
        terms <- c(
          "time",
          if (i > 1) {
            paste0("U", seq_len(i - 1), ".time")
          }
        )

        L[terms] <- L[terms] +
          lengths[i] / sum(lengths)
      }

      var_beta <- as.numeric(
        t(L) %*% stats::vcov(mod) %*% L
      )

      se_beta <- sqrt(var_beta)

      # ---- 95% CI ----
      df <- stats::df.residual(mod)

      t_crit <- stats::qt(
        0.975,
        df = df
      )

      beta_low <- beta_aapc - t_crit * se_beta
      beta_upp <- beta_aapc + t_crit * se_beta

      CI_low <- exp(beta_low) - 1
      CI_upp <- exp(beta_upp) - 1

      # ---- Significance ----
      stars <- ifelse(
        CI_low > 0 | CI_upp < 0,
        "*",
        ""
      )

      # ---- Return object ----
      tibble::tibble(
        AAPC = if (show_ci) {
          fmt_ci(
            AAPC,
            CI_low,
            CI_upp
          )
        } else {
          fmt_stars(
            AAPC,
            stars
          )
        }
      )
    },
    .id = "model"
  )
}
