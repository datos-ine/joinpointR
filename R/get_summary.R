#' Summary statistics for joinpoint regression models
#'
#' @description
#' Calculates the annual percent change (APC) and the average annual
#' percent change (AAPC) of joinpoint regression models.
#'
#' @param mods A model or a list of models of class \code{model_jp}.
#'
#' @param level.ci Numeric. Confidence level used to calculate confidence
#' intervals. Must be between 0 and 1. Defaults to \code{0.95}.
#'
#' @param stats Character. Summary statistics to display in the table.
#' One of \code{"both"}, \code{"apc"}, or \code{"aapc"}. Defaults to \code{"both"}.
#'
#' @param hide Character. Whether to hide the confidence interval \code{"ci"},
#' the significance stars (\code{"sig"}) or none (\code{"none"}). Defaults to \code{"none"}.
#'
#' @param as.ft Logical. Whether to display the model summary as a \code{flextable}
#' object. Defaults to \code{FALSE}.
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
#' get_summary(mods, level.ci = 0.95, ci = "both", sig = TRUE)
#'
#' # Same output calling summary(mods)
#' summary(mods, as.ft = TRUE)
#'
#' # Obtain the APC with 95% CI
#' get_apc(mods = mods, level.ci = 0.95, sig = TRUE)
#'
#' # Obtain the AAPC with 95% CI
#' get_aapc(mods = mods, level.ci = 0.95, sig = TRUE)
#'
#' @name get_summary
#' @aliases get_summary get_apc get_aapc

#' Get summary data
#' @export
get_summary <- function(
  mods,
  stats = c("both", "apc", "aapc"),
  hide = c("none", "ci", "sig"),
  level.ci = 0.95,
  as.ft = FALSE,
  dec = c(".", ",")
) {
  # ------------------------------------------------------------------------
  # ---- Set defaults ----
  # ------------------------------------------------------------------------
  stats <- match.arg(stats)

  hide <- match.arg(hide)

  dec <- match.arg(dec)

  # ------------------------------------------------------------------------
  # ---- Get model data ----
  # ------------------------------------------------------------------------
  extract_mod <- function(x) {
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

  # ------------------------------------------------------------------------
  # ---- Calculate APC ----
  # ------------------------------------------------------------------------
  apc <- purrr::map(
    mods,
    function(x) {
      # ---- Get model data ----
      data <- extract_mod(x)

      # ---- Variance of the slopes ----
      var_slopes <- data$L %*%
        data$var %*%
        t(data$L)

      # ---- Standard error ----
      se <- sqrt(diag(var_slopes))

      # ---- Critical value ----
      critical <- stats::qt(
        1 - (1 - level.ci) / 2,
        df = stats::df.residual(data$fit)
      )

      # ---- APC ----
      apc <- (exp(data$slopes) - 1) * 100

      # ---- Confidence interval ----
      apc_lower <- (exp(data$slopes - critical * se) - 1) * 100

      apc_upper <- (exp(data$slopes + critical * se) - 1) * 100

      # ---- Return ----
      tibble::tibble(
        jp = data$segments - 1,
        period = data$period$period,
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

  # ------------------------------------------------------------------------
  # ---- Calculate AAPC ----
  # ------------------------------------------------------------------------
  aapc <- purrr::map(
    mods,
    function(x) {
      # ---- Get model data ----
      data <- extract_mod(x)

      # ---- Segment lengths ----
      years <- stringr::str_split_fixed(data$period$period, "-", 2)

      segment_length <- as.numeric(years[, 2]) -
        as.numeric(years[, 1])

      # ---- Weights ----
      wt <- segment_length / sum(segment_length)

      # ---- Weighted slope ----
      beta <- sum(wt * data$slopes)

      # ---- Variance-covariance matrix of slopes ----
      var_slopes <- data$L %*%
        data$var %*%
        t(data$L)

      # ---- Variance of weighted slope ----
      var_beta <- t(wt) %*%
        var_slopes %*%
        wt

      # ---- Standard error ----
      se <- sqrt(as.numeric(var_beta))

      # ---- Critical value ----
      critical <- stats::qt(
        1 - (1 - level.ci) / 2,
        df = stats::df.residual(data$fit)
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

  # ------------------------------------------------------------------------
  # ---- Summary table ----
  # ------------------------------------------------------------------------
  tab <- purrr::map2(
    apc,
    aapc,
    ~ dplyr::bind_cols(
      .x,
      .y
    )
  ) |>
    purrr::list_rbind(names_to = "model")

  # ------------------------------------------------------------------------
  # ---- Table stats ----
  # ------------------------------------------------------------------------
  if (stats == "apc") {
    tab <- tab |> dplyr::select(!dplyr::starts_with("aapc"))
  } else if (stats == "aapc") {
    tab <- tab |> dplyr::select(!dplyr::starts_with("apc"))
  }

  # ------------------------------------------------------------------------
  # ---- Hide CI ----
  # ------------------------------------------------------------------------
  if (hide == "ci") {
    tab <- tab |> dplyr::select(!dplyr::ends_with(c("lower", "upper")))
  } else if (hide == "sig") {
    tab <- tab |> dplyr::select(!dplyr::ends_with("sig"))
  }

  # ------------------------------------------------------------------------
  # ---- Transform to flextable ----
  # ------------------------------------------------------------------------
  if (as.ft) {
    tab <- tab |>
      # --- Period as factor ---
      dplyr::mutate(jp = factor(jp)) |>

      # --- Separate grouping vars ---
      tidyr::separate_wider_delim(
        cols = model,
        delim = "_",
        names = c("group", "subgroup"),
        too_few = "align_start"
      ) |>

      # --- Remove empty cols ---
      dplyr::select(
        where(~ !all(is.na(.x) | .x == ""))
      ) |>

      # --- Flextable ---
      flextable::flextable() |>
      flextable::colformat_double(
        big.mark = if (dec == ",") "." else ",",
        decimal.mark = if (dec == ",") "," else ".",
        digits = 2
      )

    # --- Combine columns ---
    if ("subgroup" %in% names(tab)) {
      tab <- tab |>
        flextable::merge_v(
          j = c("group", "subgroup", "jp"),
          combine = TRUE
        )
    } else {
      tab <- tab |>
        flextable::merge_v(
          j = c("group", "jp"),
          combine = TRUE
        )
    }

    # --- Merge AAPC ---
    if (stats != "apc") {
      tab <- tab |>
        flextable::merge_v(
          j = grep("aapc", names(tab), value = TRUE),
          combine = TRUE
        )
    }
  }

  # ------------------------------------------------------------------------
  # ---- Return ----
  # ------------------------------------------------------------------------
  return(tab)
}

#' Get Annual Percent Change (APC)
#' @rdname get_summary
#' @export
#'
get_apc <- function(
  mods,
  stats = "apc",
  ...
) {
  get_summary(
    mods,
    stats = "apc",
    ...
  )
}


#' Get Average Annual Percent Change (APC)
#' @rdname get_summary
#' @export
#'
get_aapc <- function(
  mods,
  stats = "aapc",
  ...
) {
  get_summary(
    mods,
    stats = "aapc",
    ...
  )
}

#' Use summary()
#' @export
summary.model_jp <- function(
  mods,
  ...
) {
  get_summary(
    mods,
    ...
  )
}
