#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models for standardized or crude rates
#' using joinpoint regression with grid search emulating Joinpoint Regression
#' Software. The function selects the optimal model (containing up to seven
#' joinpoints) based on the Bayesian Information Criterion (BIC), applying an
#'  internal log transformation to the response variable to ensure appropriate
#' variance stabilization.
#'
#' @param data A data frame or tibble containing crude or age-standardized rates.
#' @param rate Column name for the response variable.
#' @param time Column name for the time variable.
#' @param group Column name(s) of the grouping variable(s). Defaults to \code{NULL}.
#' @param k Maximum number of joinpoints to estimate. Defaults to 2.
#' @param min_dist Minimum number of observations required per segment. Defaults to 2.
#'
#' @return A named list of joinpoint regression models fit by group, where each
#' element contains the following components:
#' \itemize{
#'   \item \code{model}: Coefficients and parameters of the best-fitting model.
#'   \item \code{joinpoints}: Positions of the joinpoint(s) detected by the model.
#'   \item \code{time}: Vector of original time values.
#'   \item \code{log_rate}: Vector of log-transformed rates.
#'   \item \code{BIC}: Bayesian Information Criterion of the selected model.
#' }
#'
#' @author Tamara Ricardo
#'
#' @details
#' The National Cancer Institute (NCI) recommends setting the maximum number
#' of joinpoints based on the length of the time series (Kim et al., 2000):
#'
#' \itemize{
#'   \item 0--6 time points: 0 joinpoints.
#'   \item 7--11 time points: 1 joinpoint.
#'   \item 12--16 time points: 2 joinpoints.
#'   \item 17--21 time points: 3 joinpoints.
#'   \item 22--26 time points: 4 joinpoints.
#'   \item 27--31 time points: 5 joinpoints.
#'   \item 32--36 time points: 6 joinpoints.
#'   \item 37+ time points: 7 joinpoints.
#' }
#'
#' @references
#' Kim HJ, Fay MP, Feuer EJ, Midthune DN (2000).
#' "Permutation Tests for Joinpoint Regression with Applications to Cancer Rates."
#' \emph{Statistics in Medicine}, 19(3), 335--351.
#' \doi{10.1002/(SICI)1097-0258(20000215)19:3<335::AID-SIM336>3.0.CO;2-Z}
#'
#' Muggeo VMR, Adelfio G (2011).
#' "Efficient change point detection in genomic sequences of continuous measurements."
#' \emph{Bioinformatics}, 27(2), 161--166.
#' \doi{10.1093/bioinformatics/btq647}
#'
#' Muggeo VMR (2020).
#' "Selecting number of breakpoints in segmented regression: implementation in the R package segmented."
#' \doi{10.13140/RG.2.2.12891.39201}
#'
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' # Filter data
#' data <- hiv_data |>
#' dplyr::filter(sex == "Both")
#'
#' # Check group levels
#' levels(data$admin)
#'
#' # Fit models
#' mods2 <- model_jp_grid(
#' data = data,
#' rate = hiv_rate,
#' time = year,
#' group = "admin",
#' k = 2,
#' min_dist = 2
#' )
#'
#' # Show the output of the first model by calling its index
#' mods[[2]]
#'
#' # Same output will be obtained when calling model name
#' mods$CABA
#'
#' @export
#'
model_jp_grid <- function(
  data,
  rate,
  time,
  group = NULL,
  k = 2,
  min_dist = 2
) {
  # ==========================================================
  # Define variables ----
  # ==========================================================
  rate_name <- rlang::as_name(rlang::ensym(rate))

  time_name <- rlang::as_name(rlang::ensym(time))

  # ==========================================================
  # Validate and prepare data ----
  # ==========================================================
  jp <- jp_data(
    data = data,
    rate = rate_name,
    time = time_name,
    group = group,
    k = k
  )

  data <- jp$data
  groups <- jp$groups
  max_jp <- jp$max_jp

  # ==========================================================
  # Function for one group ----
  # ==========================================================
  fit_group <- function(data) {
    # --------------------------------------------------------
    # Define x and y ----
    # --------------------------------------------------------
    x <- data$.time
    y <- data$.log_rate

    # --------------------------------------------------------
    # Validate joinpoints ----
    # --------------------------------------------------------
    valid_jp <- function(jp) {
      # No joinpoints
      if (length(jp) == 0) {
        return(TRUE)
      }

      # Joinpoints must be observed time points
      idx <- match(
        jp,
        x
      )

      if (anyNA(idx)) {
        return(FALSE)
      }

      # Sort joinpoints
      idx <- sort(idx)

      # Observations before first joinpoint
      left <- idx[1] - 1

      # Observations between joinpoints
      middle <- if (length(idx) > 1) {
        diff(idx) - 1
      } else {
        numeric(0)
      }

      # Observations after last joinpoint
      right <- length(x) - idx[length(idx)]

      # Check minimum observations per segment
      all(
        left >= min_dist,
        middle >= min_dist,
        right >= min_dist
      )
    }

    # ========================================================
    # Calculate Joinpoint BIC ----
    # ========================================================
    bic_jp <- function(mod, n_jp) {
      mse <- mean(
        stats::residuals(mod)^2
      )

      n <- stats::nobs(mod)

      log(mse) +
        2 * n_jp * log(n) / n
    }

    # ========================================================
    # Model results -----
    # ========================================================
    results <- list()

    # ========================================================
    # 0 joinpoints -----
    # ========================================================
    jp <- numeric(0)

    mod <- fit_jp(
      x = x,
      y = y,
      joinpoints = jp
    )

    results[[length(results) + 1]] <-
      tibble::tibble(
        n_jp = 0,
        jp1 = NA_real_,
        jp2 = NA_real_,
        jp3 = NA_real_,
        jp4 = NA_real_,
        jp5 = NA_real_,
        jp6 = NA_real_,
        jp7 = NA_real_,
        BIC = bic_jp(
          mod,
          0
        )
      )

    # ========================================================
    # 1 to k joinpoints -----
    # ========================================================
    if (k >= 1) {
      for (n_jp in seq_len(k)) {
        # ----------------------------------------------------
        # Generate all combinations of n_jp joinpoints
        # ----------------------------------------------------
        jp_grid <- utils::combn(
          sort(unique(x)),
          n_jp,
          simplify = FALSE
        )

        # ----------------------------------------------------
        # Keep only valid combinations
        # ----------------------------------------------------
        jp_grid <- purrr::keep(
          jp_grid,
          valid_jp
        )

        # ----------------------------------------------------
        # Fit every candidate
        # ----------------------------------------------------
        if (length(jp_grid) > 0) {
          for (jp in jp_grid) {
            mod <- fit_jp(
              x = x,
              y = y,
              joinpoints = jp
            )

            jp_values <- rep(
              NA_real_,
              7
            )

            jp_values[
              seq_along(jp)
            ] <- jp

            results[[length(results) + 1]] <-
              tibble::tibble(
                n_jp = n_jp,
                jp1 = jp_values[1],
                jp2 = jp_values[2],
                jp3 = jp_values[3],
                jp4 = jp_values[4],
                jp5 = jp_values[5],
                jp6 = jp_values[6],
                jp7 = jp_values[7],
                BIC = bic_jp(
                  mod,
                  n_jp
                )
              )
          }
        }
      }
    }

    # ========================================================
    # Full list of models -----
    # ========================================================
    results <- dplyr::bind_rows(
      results
    ) |>
      dplyr::arrange(BIC)

    # ========================================================
    # Select best model -----
    # ========================================================
    best <- results |>
      dplyr::slice_min(
        BIC,
        n = 1,
        with_ties = FALSE
      )

    # ========================================================
    # Selected joinpoints -----
    # ========================================================
    joinpoints <- best |>
      dplyr::select(
        jp1,
        jp2,
        jp3,
        jp4,
        jp5,
        jp6,
        jp7
      ) |>
      unlist(
        use.names = FALSE
      ) |>
      stats::na.omit() |>
      as.numeric()

    # ========================================================
    # Final model -----
    # ========================================================
    model <- fit_jp(
      x = x,
      y = y,
      joinpoints = joinpoints
    )

    # ========================================================
    # Return -----
    # ========================================================
    list(
      model = model,
      joinpoints = joinpoints,
      time = x,
      log_rate = y,
      BIC = best$BIC,
      results = results
    )
  }

  # ==========================================================
  # Fit models by group -----
  # ==========================================================
  mods <- fit_jp_groups(
    data = data,
    groups = groups,
    fit_group = fit_group
  )

  # ==========================================================
  # Return -----
  # ==========================================================
  mods
}
