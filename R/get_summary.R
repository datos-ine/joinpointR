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
#' @examples
#' # Load data
#' data(hiv_data)
#'
#' # Create a reduced dataset
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
#' @keywords internal
# Prepare data -----------------------------------------------------------
extract_jp_mod <- function(
  x
) {
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

      L[
        i + 1,
        delta[seq_len(i)]
      ] <- 1
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

#' @export
# Get APC ----------------------------------------------------------------
get_apc <- function(
  mods,
  level = 0.95,
  sig = TRUE
) {
  apc <- purrr::map(
    mods,
    function(x) {
      # --- Get model data ---
      ext <- extract_jp_mod(x)

      with(ext, {
        # --- Variance of slopes ---
        var_slopes <- L %*%
          var %*%
          t(L)

        se <- sqrt(
          diag(var_slopes)
        )

        # --- Critical value for APC ---
        critical <- stats::qt(
          1 - (1 - level) / 2,
          df = stats::df.residual(fit)
        )

        # --- CI on log scale ---
        lower <- slopes - critical * se

        upper <- slopes + critical * se

        # --- Transform to APC ---
        apc <- (exp(slopes) - 1) * 100

        apc_lower <- (exp(lower) - 1) * 100

        apc_upper <- (exp(upper) - 1) * 100

        # --- Return ---
        tibble::tibble(
          jp = segments - 1,
          segment = seq_len(segments),
          period = period$period,
          apc = apc,
          apc_lower = apc_lower,
          apc_upper = apc_upper,
          apc_sig = ifelse(
            apc_lower > 0 | apc_upper < 0,
            "*",
            ""
          )
        )
      })
    }
  )
  # -------------------------------------------------------
  # ---- Return ----
  # -------------------------------------------------------
  apc <- apc |>
    purrr::list_rbind(
      names_to = "model"
    )

  if (!sig) {
    apc |>
      dplyr::select(-apc_sig)
  } else {
    apc
  }
}

#' @export
# Get AAPC ---------------------------------------------------------------
#' @export
# Get AAPC ---------------------------------------------------------------
get_aapc <- function(
  mods,
  level = 0.95,
  sig = TRUE
) {
  # ---------------------------------------------------------
  # If there are no joinpoints, AAPC = APC
  # ---------------------------------------------------------
  if (
    all(
      purrr::map_int(
        mods,
        ~ length(.x$joinpoints)
      ) ==
        0
    )
  ) {
    apc <- get_apc(
      mods,
      level = level,
      sig = sig
    )

    return(
      apc |>
        dplyr::transmute(
          model = model,
          aapc = apc,
          aapc_lower = apc_lower,
          aapc_upper = apc_upper,
          aapc_sig = apc_sig
        )
    )
  }

  # ---------------------------------------------------------
  # AAPC for models with joinpoints
  # ---------------------------------------------------------
  aapc <- purrr::map(
    mods,
    function(x) {
      # --- Extract model information ---
      ext <- extract_jp_mod(x)

      with(ext, {
        # ---------------------------------------------------
        # Time weights
        # ---------------------------------------------------
        time <- x$time

        limits <- c(
          min(time, na.rm = TRUE),
          jp,
          max(time, na.rm = TRUE)
        )

        weights <- diff(limits) /
          (max(time, na.rm = TRUE) -
            min(time, na.rm = TRUE))

        # ---------------------------------------------------
        # Weighted slope
        # ---------------------------------------------------
        slope_aapc <- sum(
          weights * slopes
        )

        # ---------------------------------------------------
        # AAPC contrast
        # ---------------------------------------------------
        L_aapc <- numeric(
          length(beta)
        )

        names(L_aapc) <- names(beta)

        L_aapc["x"] <- 1

        if (length(delta) > 0) {
          for (j in seq_along(delta)) {
            L_aapc[delta[j]] <-
              sum(
                weights[(j + 1):segments]
              )
          }
        }

        # ---------------------------------------------------
        # Variance of AAPC slope
        # ---------------------------------------------------
        var_aapc <- drop(
          L_aapc %*%
            var %*%
            L_aapc
        )

        se_aapc <- sqrt(
          var_aapc
        )

        # ---------------------------------------------------
        # Parametric critical value
        #
        # Joinpoint:
        # AAPC with joinpoints -> normal distribution
        # ---------------------------------------------------
        critical <- stats::qnorm(
          1 - (1 - level) / 2
        )

        # ---------------------------------------------------
        # CI on log scale
        # ---------------------------------------------------
        lower_slope <- slope_aapc -
          critical * se_aapc

        upper_slope <- slope_aapc +
          critical * se_aapc

        # ---------------------------------------------------
        # Transform to percentage scale
        # ---------------------------------------------------
        aapc <- (exp(slope_aapc) - 1) * 100

        aapc_lower <- (exp(lower_slope) - 1) * 100

        aapc_upper <- (exp(upper_slope) - 1) * 100

        # ---------------------------------------------------
        # Significance
        # ---------------------------------------------------
        aapc_sig <- ifelse(
          aapc_lower > 0 | aapc_upper < 0,
          "*",
          ""
        )

        # ---------------------------------------------------
        # Return
        # ---------------------------------------------------
        tibble::tibble(
          aapc = aapc,
          aapc_lower = aapc_lower,
          aapc_upper = aapc_upper,
          aapc_sig = aapc_sig
        )
      })
    }
  )

  # ---------------------------------------------------------
  # Bind models
  # ---------------------------------------------------------
  aapc <- aapc |>
    purrr::list_rbind(
      names_to = "model"
    )

  # ---------------------------------------------------------
  # Remove significance if requested
  # ---------------------------------------------------------
  if (!sig) {
    aapc |>
      dplyr::select(-aapc_sig)
  } else {
    aapc
  }
}

#' @export
# Summarise data ---------------------------------------------------------
get_summary <- function(
  mods,
  ci = c("both", "apc", "aapc", "hide"),
  sig = TRUE,
  level = 0.95
) {
  # ----------------------------------------------------------
  # ---- Defaults ----
  # ----------------------------------------------------------
  ci <- match.arg(ci)

  # ----------------------------------------------------------
  # ---- Validations ----
  # ----------------------------------------------------------
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
