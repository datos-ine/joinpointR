#' Summary statistics for joinpoint regression models
#'
#' @description
#' Calculates the annual percent change (APC) and the average annual
#' percent change (AAPC) of joinpoint regression models.
#'
#' @param mods A model or a list of models of class \code{model_jp}.
#'
#' @param level Numeric. Confidence level used to calculate confidence
#' intervals. Must be between 0 and 1. Defaults to \code{0.95}.
#'
#' @param ci Character. Controls which confidence intervals are displayed
#' in the summary table. One of \code{"both"}, \code{"apc"},
#' \code{"aapc"}, or \code{"hide"}. Defaults to \code{"both"}.
#'
#' @param sig Logical. Whether to display significance stars for the APC
#' and AAPC. Defaults to \code{TRUE}.
#'
#' @return
#' For \code{get_summary()}, a \code{tibble} containing the APC and AAPC
#' for each model, together with their confidence intervals and/or
#' significance stars according to the values of \code{ci} and \code{sig}.
#'
#' For \code{get_apc()}, a \code{tibble} containing the APC for each
#' segment, including the corresponding time period, confidence interval,
#' and significance stars when \code{sig = TRUE}.
#'
#' For \code{get_aapc()}, a \code{tibble} containing the AAPC for each
#' model, including its confidence interval and significance stars when
#' \code{sig = TRUE}.
#'
#' @details
#' For each segment, the Annual Percent Change (APC) is calculated from the
#' estimated slope \eqn{\hat{\beta}} of the log-linear model as:
#'
#' \deqn{
#' APC = 100[\exp(\hat{\beta}) - 1].
#' }
#'
#' Confidence intervals for the APC are calculated using the Student's
#' \eqn{t} distribution and the standard error of the estimated slope.
#' The resulting confidence limits are then back-transformed to the APC scale:
#'
#' \deqn{
#' APC_{lower} =
#' 100[\exp{\hat{\beta} -
#' t_{1-\alpha/2,df}SE(\hat{\beta})} - 1],
#' }
#'
#' \deqn{
#' APC_{upper} =
#' 100[\exp{\hat{\beta} +
#' t_{1-\alpha/2,df}SE(\hat{\beta})} - 1].
#' }
#'
#' The standard errors used for the segment-specific slopes are obtained
#' from the variance-covariance matrix of the fitted joinpoint model.
#'
#' For each fitted model, the Annual Average Percent Change (AAPC) is
#' calculated as the exponential transformation of the weighted average
#' of the segment-specific slopes on the log scale:
#'
#' \deqn{
#' AAPC =
#' 100\left[
#' \exp\left(\sum_j w_j\hat{\beta}j\right) - 1
#' \right],
#' }
#'
#' where \eqn{\hat{\beta}j} is the estimated slope for segment \eqn{j},
#' and \eqn{w_j} is the length of segment \eqn{j} divided by the total
#' length of the selected time interval. Thus, the weights sum to one.
#' For models without joinpoints, the AAPC is equivalent to the APC.
#'
#' The confidence interval for the AAPC is calculated on the log scale.
#' The variance of the weighted slope is obtained from the
#' variance-covariance matrix of the segment-specific slopes. The confidence
#' limits are calculated using the Student's \eqn{t} distribution and the
#' residual degrees of freedom of the fitted model, and are then
#' back-transformed to the AAPC scale:
#'
#' \deqn{
#' AAPC{lower} =
#' 100\left[
#' \exp\left{
#' \hat{\beta}{AAPC} -
#' t_{1-\alpha/2,df}SE(\hat{\beta}{AAPC})
#' \right} - 1
#' \right],
#' }
#'
#' \deqn{
#' AAPC{upper} =
#' 100\left[
#' \exp\left{
#' \hat{\beta}{AAPC} +
#' t{1-\alpha/2,df}SE(\hat{\beta}_{AAPC})
#' \right} - 1
#' \right].
#' }
#'
#' The confidence intervals described above are based on the
#' variance-covariance matrix and residual degrees of freedom of the fitted
#' joinpoint model and may therefore differ from confidence intervals
#' reported by other joinpoint regression software using different
#' inferential procedures.
#'
#' @examples
#' # Create an example dataset
#' data <- hiv_data |>
#' dplyr::filter(admin == "ARG")
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year, group = "sex")
#'
#' # Obtain the model summary
#' get_summary(mods, level = 0.95, ci = "both", sig = TRUE)
#'
#' # Same output calling summary(mods)
#' summary(mods)
#'
#' # Obtain the APC with 95% CI
#' get_apc(mods = mods, level = 0.95, sig = TRUE)
#'
#' # Obtain the AAPC with 95% CI
#' get_aapc(mods = mods, level = 0.95, sig = TRUE)
#'
#' @name get_summary
#' @aliases get_summary get_apc get_aapc
#'
#' @export
#'
get_apc <- function(
  mods,
  level = 0.95,
  sig = TRUE
) {
  apc <- purrr::map(
    mods,
    function(x) {
      # ---- Get model data ----
      mod_data <- extract_jp_mod(x)

      # ---- Variance of the slopes ----
      var_slopes <- mod_data$L %*%
        mod_data$var %*%
        t(mod_data$L)

      # ---- Standard error ----
      se <- sqrt(diag(var_slopes))

      # ---- Critical value ----
      critical <- stats::qt(
        1 - (1 - level) / 2,
        df = stats::df.residual(mod_data$fit)
      )

      # ---- APC ----
      apc <- (exp(mod_data$slopes) - 1) * 100

      # ---- Confidence interval ----
      apc_lower <- (exp(mod_data$slopes - critical * se) - 1) * 100

      apc_upper <- (exp(mod_data$slopes + critical * se) - 1) * 100

      # ---- Return ----
      tibble::tibble(
        jp = mod_data$segments - 1,
        period = mod_data$period$period,
        apc = apc,
        apc_lower = apc_lower,
        apc_upper = apc_upper,
        apc_sig = ifelse(
          apc_lower > 0 | apc_upper < 0,
          "*",
          ""
        )
      )
    }
  )

  # ---- Return ----
  apc <- apc |>
    purrr::list_rbind(names_to = "model")

  if (!sig) {
    apc |> dplyr::select(-sig)
  } else {
    apc
  }
}


#' Get AAPC
#' @rdname get_summary
#' @export
#'
get_aapc <- function(
  mods,
  level = 0.95,
  sig = TRUE
) {
  aapc <- purrr::map(
    mods,
    function(x) {
      # ---- Get model data ----
      mod_data <- extract_jp_mod(x)

      # ---- Segment lengths ----
      years <- stringr::str_split_fixed(mod_data$period$period, "-", 2)

      segment_length <- as.numeric(years[, 2]) -
        as.numeric(years[, 1])

      # ---- Weights ----
      wt <- segment_length / sum(segment_length)

      # ---- Weighted slope ----
      beta <- sum(wt * mod_data$slopes)

      # ---- Variance-covariance matrix of slopes ----
      var_slopes <- mod_data$L %*%
        mod_data$var %*%
        t(mod_data$L)

      # ---- Variance of weighted slope ----
      var_beta <- t(wt) %*%
        var_slopes %*%
        wt

      # ---- Standard error ----
      se <- sqrt(as.numeric(var_beta))

      # ---- Critical value ----
      critical <- stats::qt(
        1 - (1 - level) / 2,
        df = stats::df.residual(mod_data$fit)
      )

      # ---- AAPC ----
      aapc <- (exp(beta) - 1) * 100

      # ---- Confidence interval ----
      aapc_lower <- (exp(beta - critical * se) - 1) * 100

      aapc_upper <- (exp(beta + critical * se) - 1) * 100

      # ---- Return ----
      tibble::tibble(
        aapc = aapc,
        aapc_lower = aapc_lower,
        aapc_upper = aapc_upper,
        aapc_sig = ifelse(
          aapc_lower > 0 | aapc_upper < 0,
          "*",
          ""
        )
      )
    }
  )

  # ---- Return ----
  aapc <- aapc |>
    purrr::list_rbind(names_to = "model")

  if (!sig) {
    aapc |>
      dplyr::select(-aapc_sig)
  } else {
    aapc
  }
}


#' Summarise Joinpoint Regression
#' @rdname get_summary
#' @export
#'
get_summary <- function(
  mods,
  ci = c("both", "apc", "aapc", "hide"),
  sig = TRUE,
  level = 0.95
) {
  # ---- Defaults ----
  ci <- match.arg(ci)

  # ---- Validations ----
  # --- Significance stars and CI ---
  if (ci == "hide" && !sig) {
    stop(
      "Summary table must include either the 95% confidence interval or the significance stars.",
      call. = FALSE
    )
  }

  # --- CI level ---
  if (level != 0.95) {
    message(paste0(
      "Confidence level changed to ",
      level,
      " (",
      scales::percent(level),
      "CI)."
    ))
  } else {
    message(
      "Confidence level set to default value (",
      scales::percent(level),
      " CI)."
    )
  }

  # ---- Calculate APC ----
  apc <- get_apc(mods, level = level, sig = sig)

  # ---- Calculate AAPC ----
  aapc <- get_aapc(mods, level = level, sig = sig)

  # ---- Generate table ----
  tab <- dplyr::left_join(
    apc,
    aapc,
    by = "model"
  )

  # ---- Select which CIs to display ----
  if (ci == "apc") {
    tab <- tab |>
      dplyr::select(-aapc_lower, -aapc_upper)
  } else if (ci == "aapc") {
    tab <- tab |>
      dplyr::select(-apc_lower, -apc_upper)
  } else if (ci == "hide") {
    tab <- tab |>
      dplyr::select(!dplyr::contains(c("lower", "upper")))
  }

  # ---- Return ----
  tab
}

# ---- Use summary ----
#' @export
summary.model_jp <- function(
  object,
  ...
) {
  get_summary(
    object,
    ...
  )
}


#' Extract model data
#' @keywords internal
extract_jp_mod <- function(x) {
  # --- Model fit ---
  fit <- x$fit

  # --- Coefficients ---
  beta <- stats::coef(fit)

  # --- Variance/covariance matrix ---
  var <- stats::vcov(fit)

  # --- Joinpoints ---
  jp <- x$joinpoints

  # --- Number of segments ---
  segments <- length(jp) + 1

  # --- Time breaks ---
  breaks <- sort(c(
    min(x$time, na.rm = TRUE),
    jp,
    max(x$time, na.rm = TRUE)
  ))

  # --- Time segments ---
  period <- tibble::tibble(
    period = paste(
      head(breaks, -1),
      tail(breaks, -1),
      sep = "-"
    )
  )

  # --- Hinge variable names ---
  delta <- grep(
    "^U\\.",
    names(beta),
    value = TRUE
  )

  # --- Slopes ---
  slopes <- c(
    beta["x"],
    beta["x"] + cumsum(beta[delta])
  )

  # --- Contrast matrix ---
  L <- matrix(
    0,
    nrow = segments,
    ncol = length(beta),
    dimnames = list(
      paste0("segment", seq_len(segments)),
      names(beta)
    )
  )

  # --- First segment ---
  L[1, "x"] <- 1

  # --- Remaining segments ---
  if (length(delta) > 0) {
    for (i in seq_along(delta)) {
      L[i + 1, "x"] <- 1

      L[i + 1, delta[seq_len(i)]] <- 1
    }
  }

  # --- Return ---
  list(
    fit = fit,
    beta = beta,
    delta = delta,
    slopes = slopes,
    var = var,
    L = L,
    jp = jp,
    segments = segments,
    period = period
  )
}
