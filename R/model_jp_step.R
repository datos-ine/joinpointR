#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models for standardized or crude rates
#' using joinpoint regression. It selects the optimal model
#' (containing up to seven joinpoints) using a stepwise procedure based on the
#' Bayesian Information Criterion (BIC) or the Akaike Information Criteria (AIC),
#' applying an internal log transformation to the response variable to ensure
#' appropriate variance stabilization. Optionally, a fixed number of joinpoints
#' can be selected using the argument \code{method = "fixed"}.
#'
#' @param data A data frame or tibble containing crude or age-standardized rates.
#' @param rate Column name for the response variable.
#' @param time Column name for the time variable.
#' @param group Column name(s) of the grouping variable(s). Defaults to \code{NULL}.
#' @param k Maximum number of joinpoints to estimate. Defaults to 2.
#' @param min_dist Minimum number of observations required per segment. Defaults to 2.
#' @param method Criterion adopted to select the best fit model. Defaults to BIC.
#'
#' @return A named list of joinpoint regression models fit by group, where each
#' element contains the following components:
#' \itemize{
#'   \item \code{model}: Coefficients and parameters of the best-fitting model.
#'   \item \code{joinpoints}: Positions of the joinpoint(s) detected by the model.
#'   \item \code{time}: Vector of original time values.
#'   \item \code{log_rate}: Vector of log-transformed rates.
#'   \item \code{BIC}: Bayesian Information Criterion of the selected model.
#'   \item \code{AIC}: Akaike Information Criterion of the selected model.
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
#' mods <- model_jp_step(
#' data = data,
#' rate = hiv_rate,
#' time = year,
#' group = "admin",
#' k = 2,
#' min_dist = 2,
#' method = "bic"
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
model_jp_step <- function(
  data,
  rate,
  time,
  group = NULL,
  k = 2,
  min_dist = 2,
  method = "bic"
) {
  # ==========================================================
  # Define variables ----
  # ==========================================================
  rate_name <- rlang::as_name(rlang::ensym(rate))

  time_name <- rlang::as_name(rlang::ensym(time))

  method <- match.arg(
    method,
    choices = c("bic", "aic", "fixed")
  )

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
  n <- jp$n
  max_jp <- jp$max_jp

  # ==========================================================
  # Fit models by group ----
  # ==========================================================
  fit_group <- function(data) {
    x <- data$.time
    y <- data$.log_rate

    # --------------------------------------------------------
    # Validate joinpoints ----
    # --------------------------------------------------------
    valid_jp <- function(jp) {
      if (length(jp) == 0) {
        return(length(x) >= 2)
      }

      jp <- sort(jp)

      # Joinpoints must correspond to observed time points
      if (!all(jp %in% x)) {
        return(FALSE)
      }

      idx <- match(jp, x)

      # Minimum observations before first JP
      before <- idx[1] - 1

      # Minimum observations between JPs
      middle <- if (length(idx) > 1) {
        diff(idx) - 1
      } else {
        numeric(0)
      }

      # Minimum observations after last JP
      after <- length(x) - idx[length(idx)]

      all(
        before >= min_dist,
        middle >= min_dist,
        after >= min_dist
      )
    }

    # --------------------------------------------------------
    # Fit model with zero joinpoints ----
    # --------------------------------------------------------
    fit0 <- fit_jp(
      x = x,
      y = y,
      joinpoints = numeric(0)
    )

    # ========================================================
    # Fixed number of joinpoints ----
    # ========================================================
    if (method == "fixed") {
      if (k == 0) {
        final_fit <- fit0

        return(
          list(
            model = final_fit,
            joinpoints = numeric(0),
            BIC = stats::BIC(final_fit),
            AIC = stats::AIC(final_fit)
          )
        )
      }

      # All possible combinations of k joinpoints
      candidate_jp <- combn(
        sort(unique(x)),
        k,
        simplify = FALSE
      )

      # Keep only valid combinations
      candidate_jp <- purrr::keep(
        candidate_jp,
        valid_jp
      )

      if (length(candidate_jp) == 0) {
        warning(
          "No valid set of joinpoints was found.",
          call. = FALSE
        )

        return(
          list(
            model = fit0,
            joinpoints = numeric(0),
            BIC = stats::BIC(fit0),
            AIC = stats::AIC(fit0)
          )
        )
      }

      # Fit all candidates
      candidates <- purrr::map_dfr(
        candidate_jp,
        function(jp) {
          mod <- fit_jp(
            x = x,
            y = y,
            joinpoints = jp
          )

          tibble::tibble(
            joinpoints = list(jp),
            BIC = stats::BIC(mod),
            AIC = stats::AIC(mod)
          )
        }
      )

      # Select model
      criterion <- if (method == "bic") "BIC" else "AIC"

      best <- candidates |>
        dplyr::slice_min(
          .data[[criterion]],
          n = 1,
          with_ties = FALSE
        )

      joinpoints <- best$joinpoints[[1]]

      final_fit <- fit_jp(
        x = x,
        y = y,
        joinpoints = joinpoints
      )

      return(
        list(
          model = final_fit,
          joinpoints = joinpoints,
          BIC = stats::BIC(final_fit),
          AIC = stats::AIC(final_fit)
        )
      )
    }

    # ========================================================
    # Stepwise selection ----
    # ========================================================
    joinpoints <- numeric(0)

    current_fit <- fit0

    current_criterion <- if (method == "bic") {
      stats::BIC(current_fit)
    } else {
      stats::AIC(current_fit)
    }

    # --------------------------------------------------------
    # Add one joinpoint at a time ----
    # --------------------------------------------------------
    for (i in seq_len(min(k, max_jp))) {
      # Candidate joinpoints are observed time points
      candidate_times <- sort(unique(x))

      # Exclude already selected joinpoints
      candidate_times <- setdiff(
        candidate_times,
        joinpoints
      )

      # Generate candidate sets
      candidate_jp <- lapply(
        candidate_times,
        function(jp) {
          sort(c(joinpoints, jp))
        }
      )

      # Keep valid candidates
      candidate_jp <- purrr::keep(
        candidate_jp,
        valid_jp
      )

      if (length(candidate_jp) == 0) {
        break
      }

      # Fit candidates
      candidates <- purrr::map_dfr(
        candidate_jp,
        function(jp) {
          mod <- fit_jp(
            x = x,
            y = y,
            joinpoints = jp
          )

          tibble::tibble(
            joinpoints = list(jp),
            BIC = stats::BIC(mod),
            AIC = stats::AIC(mod)
          )
        }
      )

      # Select best candidate
      criterion <- if (method == "bic") "BIC" else "AIC"

      best <- candidates |>
        dplyr::slice_min(
          .data[[criterion]],
          n = 1,
          with_ties = FALSE
        )

      new_criterion <- best[[criterion]][1]

      # ------------------------------------------------------
      # Stop if adding a joinpoint does not improve criterion
      # ------------------------------------------------------
      if (new_criterion >= current_criterion) {
        break
      }

      # Accept new joinpoint
      joinpoints <- best$joinpoints[[1]]
      current_criterion <- new_criterion

      current_fit <- fit_jp(
        x = x,
        y = y,
        joinpoints = joinpoints
      )
    }

    # ========================================================
    # Final model ----
    # ========================================================
    final_fit <- current_fit

    list(
      model = final_fit,
      time = data$.time,
      log_rate = data$.log_rate,
      joinpoints = joinpoints,
      BIC = stats::BIC(final_fit),
      AIC = stats::AIC(final_fit)
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
