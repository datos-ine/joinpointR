#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models by groups for age-standardized rates
#' using joinpoint regression. Models are fitted using a grid search method. The function
#' applies a log transformation to the response variable
#'
#' @param data A data frame containing age-standardized rates.
#' @param value Response variable.
#' @param time Time variable.
#' @param k Maximum number of joinpoints to estimate.
#' @param min_dist Minimum number of observations required per segment.
#' Defaults to 2 to prevent joinpoints from being too close.
#'
#' @return A named list of joinpoint regression models by group.
#' @author Tamara Ricardo
#' @details
#' The National Cancer Institute (NCI) recommends the following maximum number
#' of joinpoints according to the length of the time series (Kim et al., 2000):
#'
#' \itemize{
#' \item 0--6 time points: 0 joinpoints.
#' \item 7--11 time points: 1 joinpoint.
#' \item 12--16 time points: 2 joinpoints.
#' \item 17--21 time points: 3 joinpoints.
#' \item 22--26 time points: 4 joinpoints.
#' \item 27--31 time points: 5 joinpoints.
#' \item 32--36 time points: 6 joinpoints.
#' \item 37 or more time points: 7 joinpoints.
#' }
#'
#' @references
#' Kim HJ, Fay MP, Feuer EJ, Midthune DN (2000).
#' "Permutation Tests for Joinpoint Regression with Applications to Cancer Rates."
#' \emph{Statistics in Medicine}, 19(3), 335--351.
#' doi:10.1002/(sici)1097-0258(20000215)19:3<335::aid-sim336>3.0.co;2-z.
#'
#' Muggeo, V.M.R., Adelfio, G. (2011).
#' Efficient change point detection in genomic sequences of continuous
#' measurements. \emph{Bioinformatics}, 27, 161–166.
#'
#' Muggeo, Vito. (2020).
#' Selecting number of breakpoints in segmented regression:
#' implementation in the R package segmented. 10.13140/RG.2.2.12891.39201.
#'
model_jp_grid <- function(
  data,
  value,
  time,
  k = 2,
  min_dist = 2
) {
  # ---- Define variables ----
  value <- rlang::ensym(value)
  time <- rlang::ensym(time)

  # ---- Validate response variable ----
  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop("The response variable must be > 0 to apply log() transformation")
  }

  # ---- Validate number of joinpoints ----
  if (k < 0 || k > 2) {
    stop("This function currently supports k = 0, 1 or 2")
  }

  if (nrow(data) < 2 * (k + 1)) {
    stop("Not enough observations for the requested number of joinpoints")
  }

  # ---- Prepare data ----
  data <- data |>
    dplyr::mutate(
      .jp_time = !!time,
      .jp_log_value = log(!!value)
    ) |>
    dplyr::arrange(.jp_time)

  # ---- Fit segmented regression ----
  fit_jp <- function(jp) {
    x <- data$.jp_time

    X <- data.frame(
      y = data$jp_log_value,
      time = x
    )

    if (length(jp) >= 1) {
      X$hinge1 <- pmax(x - jp[1], 0)
    }

    if (length(jp) >= 2) {
      X$hinge2 <- pmax(x - jp[2], 0)
    }

    if (length(jp) == 0) {
      stats::lm(
        y ~ time,
        data = X
      )
    } else if (length(jp) == 1) {
      stats::lm(
        y ~ time + hinge1,
        data = X
      )
    } else {
      stats::lm(
        y ~ time + hinge1 + hinge2,
        data = X
      )
    }
  }

  # ---- Validate joinpoints ----
  valid_jp <- function(jp) {
    idx <- match(jp, data$.jp_time)

    # ---- At least one observation before first joinpoint ----
    left <- idx[1] - 1

    # ---- At least two observations between joinpoints ----
    if (length(jp) > 1) {
      middle <- diff(idx) - 1
    } else {
      middle <- numeric(0)
    }

    # ---- At least one observation after last joinpoint ----
    right <- nrow(data) - idx[length(idx)]

    all(
      left >= min_dist,
      middle >= min_dist,
      right >= min_dist
    )
  }

  # ---- Model results ----

  results <- list()

  #------------------------------------------
  # 0 joinpoints
  #------------------------------------------

  mod0 <- fit_jp(numeric(0))

  results[[length(results) + 1]] <-
    tibble::tibble(
      n_jp = 0,
      jp1 = NA_real_,
      jp2 = NA_real_,
      BIC = stats::BIC(mod0)
    )

  #------------------------------------------
  # 1 joinpoint
  #------------------------------------------

  if (k >= 1) {
    for (jp1 in data$.jp_time) {
      jp <- c(jp1)

      if (!valid_jp(jp)) {
        next
      }

      mod <- fit_jp(jp)

      results[[length(results) + 1]] <-
        tibble::tibble(
          n_jp = 1,
          jp1 = jp1,
          jp2 = NA_real_,
          BIC = stats::BIC(mod)
        )
    }
  }

  #------------------------------------------
  # 2 joinpoints
  #------------------------------------------

  if (k >= 2) {
    for (jp1 in data$.jp_time) {
      for (jp2 in data$.jp_time) {
        if (jp2 <= jp1) {
          next
        }

        jp <- c(jp1, jp2)

        if (!valid_jp(jp)) {
          next
        }

        mod <- fit_jp(jp)

        results[[length(results) + 1]] <-
          tibble::tibble(
            n_jp = 2,
            jp1 = jp1,
            jp2 = jp2,
            BIC = stats::BIC(mod)
          )
      }
    }
  }

  # ---- Full list of models ----

  results <- dplyr::bind_rows(results) |>
    dplyr::arrange(BIC)

  # ---- Select best fit model ----

  best <- results |>
    dplyr::slice_min(
      BIC,
      n = 1,
      with_ties = FALSE
    )

  # ---- Selected joinpoints ----
  joinpoints <- best |>
    dplyr::select(jp1, jp2) |>
    unlist(use.names = FALSE) |>
    stats::na.omit()

  # --- Final model ----
  model <- fit_jp(joinpoints)

  # ---- Model results ----
  list(
    model = model,
    joinpoints = joinpoints,
    BIC = best$BIC,
    results = results
  )
}
