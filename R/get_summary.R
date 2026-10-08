#' Summary statistics for joinpoint regression models
#'
#' @description
#' Calculates the annual percent change (APC) and the average annual
#' percent change (AAPC) for joinpoint regression models.
#'
#' @param models A model or a list of models of class \code{model_jp}.
#'
#' @param stats Character. Summary statistics to display in the table.
#' One of \code{"both"}, \code{"apc"}, or \code{"aapc"}. Defaults to \code{"both"}.
#'
#' @param hide Character. Which elements to hide: the confidence interval (\code{"ci"}),
#'  significance stars (\code{"sig"}), or none (\code{"none"}). Defaults to \code{"none"}.
#'
#' @param level.ci Numeric. Confidence level used to calculate confidence
#' intervals. Must be between 0 and 1. Defaults to \code{0.95}.
#'
#' @param as.ft Logical. Whether to return the summary as a \code{flextable}
#' object instead of a \code{tibble}. Defaults to \code{FALSE}.
#'
#' @param dec Character. Decimal separator to use, either a point (\code{"."}) or
#'  a comma (\code{","}). Defaults to \code{"."}.
#'
#' @param ... Additional arguments passed to \code{get_summary()}.
#'
#' @return
#' For \code{get_summary()}, a \code{\link[tibble]{tibble}} or a
#' \code{\link[flextable]{flextable}} containing the APC and/or AAPC for each model,
#' along with their confidence intervals and significance stars according to the
#' values of \code{stats} and \code{hide}.
#'
#' For \code{get_apc()}, a \code{\link[tibble]{tibble}} or a
#' \code{\link[flextable]{flextable}} containing the APC for each model, along
#' with its confidence intervals and significance stars according to the
#' value of \code{hide}.
#'
#' For \code{get_aapc()}, a \code{\link[tibble]{tibble}} or a
#' \code{\link[flextable]{flextable}} containing the AAPC for each model, along
#' with its confidence intervals and significance stars according to the
#' value of \code{hide}.
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
#' AAPC_{lower} =
#' 100 \left[ \exp
#' \left\{ \hat{\beta}_{\text{AAPC}} -
#' t_{1-\alpha/2, \text{df}} SE(\hat{\beta}_{\text{AAPC}}) \right\} - 1 \right].
#' }
#'
#' \deqn{
#' AAPC_{upper} = 100 \left[ \exp \left\{ \hat{\beta}_{\text{AAPC}} +
#' t_{1-\alpha/2, \text{df}} SE(\hat{\beta}_{\text{AAPC}}) \right\} - 1 \right].
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
#' get_summary(models = mods)
#'
#' # Same output calling summary(mods)
#' summary(mods, as.ft = TRUE)
#'
#' # Obtain the APC with 95% CI
#' get_apc(models = mods)
#'
#' # Obtain the AAPC with 95% CI
#' get_aapc(models = mods)
#'
#' @name get_summary
#' @export
#'
get_summary <- function(
  models,
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

    # --- Time periods ---
    period <- tibble::tibble(
      period = paste(
        utils::head(breaks, -1),
        utils::tail(breaks, -1),
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
  # ---- Get summary stats ----
  # ------------------------------------------------------------------------
  apc_aapc <- purrr::map(
    models,
    function(x) {
      # ---- Get model data ----
      data <- extract_mod(x)

      # ---- Variance matrix of the slopes ----
      var_slopes <- data$L %*%
        data$var %*%
        t(data$L)

      # -------------------------------------------------------------
      # ---- Calculate APC ----
      # -------------------------------------------------------------
      # ---- Standard error ----
      se_apc <- sqrt(diag(var_slopes))

      # ---- Critical value ----
      crit_apc <- stats::qt(
        1 - (1 - level.ci) / 2,
        df = stats::df.residual(data$fit)
      )

      # ---- APC (CI) ----
      apc <- (exp(data$slopes) - 1) * 100

      apc_lower <- (exp(data$slopes - crit_apc * se_apc) - 1) * 100

      apc_upper <- (exp(data$slopes + crit_apc * se_apc) - 1) * 100

      # -------------------------------------------------------------
      # ---- Calculate AAPC ----
      # -------------------------------------------------------------
      # ---- Segment lengths ----
      years <- stringr::str_split_fixed(data$period$period, "-", 2)

      segment_length <- as.numeric(years[, 2]) -
        as.numeric(years[, 1])

      # ---- Weights ----
      wt <- segment_length / sum(segment_length)

      # ---- Weighted slope ----
      beta <- sum(wt * data$slopes)

      # ---- Variance of weighted slope ----
      var_beta <- t(wt) %*%
        var_slopes %*%
        wt

      # ---- Standard error ----
      se_aapc <- sqrt(as.numeric(var_beta))

      # ---- Critical value ----
      crit_aapc <- stats::qt(
        1 - (1 - level.ci) / 2,
        df = stats::df.residual(data$fit)
      )

      # ---- AAPC (CI) ----
      aapc <- (exp(beta) - 1) * 100

      aapc_lower <- (exp(beta - crit_aapc * se_aapc) - 1) * 100

      aapc_upper <- (exp(beta + crit_aapc * se_aapc) - 1) * 100

      # -------------------------------------------------------------
      # ---- Return ----
      # -------------------------------------------------------------
      return(
        tibble::tibble(
          n_jp = data$segments - 1,
          segment = seq_len(data$segments),
          period = data$period$period,
          apc = apc,
          apc_lower = apc_lower,
          apc_upper = apc_upper,
          apc_sig = ifelse(
            apc_lower > 0 | apc_upper < 0,
            "*",
            ""
          ),
          aapc = aapc,
          aapc_lower = aapc_lower,
          aapc_upper = aapc_upper,
          aapc_sig = ifelse(
            aapc_lower > 0 | aapc_upper < 0,
            "*",
            ""
          ),
        )
      )
    }
  )

  # ------------------------------------------------------------------------
  # ---- Summary table ----
  # ------------------------------------------------------------------------
  tab <- apc_aapc |>
    # --- Convert to tibble ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Format grouping variable ---
    dplyr::mutate(
      group_var = stringr::str_replace_all(
        .data$group_var,
        c("\\." = " ", "_" = ": ")
      )
    )

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
    ftab <- tab |>
      # --- Separate grouping variable ---
      tidyr::separate_wider_delim(
        .data$group_var,
        delim = ": ",
        names = c("group", "subgroup"),
        too_few = "align_start",
        too_many = "merge"
      ) |>

      # --- Drop empty cols ---
      dplyr::select(dplyr::where(~ !all(is.na(.x)))) |>

      # --- Transform to flextable ---
      flextable::flextable() |>

      # --- Format numbers ---
      flextable::colformat_double(
        j = setdiff(
          names(tab)[vapply(tab, is.numeric, logical(1))],
          c("n_jp", "segment")
        ),
        big.mark = if (dec == ",") "." else ",",
        decimal.mark = if (dec == ",") "," else ".",
        digits = 2
      ) |>

      # --- Group columns ----
      flextable::merge_v(
        j = grep("group|jp", names(tab)),
        combine = TRUE
      )

    if (stats != "apc") {
      ftab <- ftab |>
        flextable::merge_v(
          j = grep("aapc", names(tab), value = TRUE),
          combine = TRUE
        )
    }
  }

  # ------------------------------------------------------------------------
  # ---- Return ----
  # ------------------------------------------------------------------------
  if (!as.ft) return(tab) else return(ftab)
}

#' Get Annual Percent Change (APC) and its confidence interval (CI)
#' @rdname get_summary
#' @export
#'
get_apc <- function(
  models,
  stats = "apc",
  ...
) {
  get_summary(
    models,
    stats = "apc",
    ...
  )
}


#' Get Average Annual Percent Change (AAPC) and its confidence interval (CI)
#' @rdname get_summary
#' @export
#'
get_aapc <- function(
  models,
  stats = "aapc",
  ...
) {
  get_summary(
    models,
    stats = "aapc",
    ...
  )
}

#' Use the shortcut summary()
#' @param object Model or list of models to update.
#' @export
#'
summary.model_jp <- function(
  object,
  ...
) {
  get_summary(
    object,
    ...
  )
}
