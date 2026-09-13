#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models for standardized or crude rates
#' using joinpoint regression with grid search. It selects the optimal model
#' (containing up to seven joinpoints) based on the Bayesian Information
#' Criterion (BIC), applying an internal log transformation to the response
#' variable to ensure appropriate variance stabilization.
#'
#' @param data A data frame or tibble containing crude or age-standardized rates.
#' @param value Column name for the response variable.
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
#' hiv_data <- hiv_data |>
#' dplyr::filter(sex == "Both") 
#'
#' # Check group levels
#' levels(hiv_data$admin)
#'
#' # Fit models
#' mods <- model_jp(
#' data = hiv_data,
#' value = hiv_rate,
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
model_jp <- function(
  data,
  value,
  time,
  group = NULL,
  k = 2,
  min_dist = 2
) {
  # ==========================================================
  # Define variables
  # ==========================================================

  value <- rlang::ensym(value)
  time <- rlang::ensym(time)

  # ==========================================================
  # Validate response variable
  # ==========================================================

  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop(
      "The response variable must be > 0 to apply log() transformation"
    )
  }

  # ==========================================================
  # Validate maximum number of joinpoints
  # ==========================================================
  if (k > 7) {
    stop("This function supports a maximum of 7 joinpoints")
  }

  # ==========================================================
  # Validate optimal number of joinpoints
  # ==========================================================

  t <- dplyr::n_distinct(data[[time]])

  max_jp <- max(
    0,
    min(
      5,
      floor((t - 2) / 5)
    )
  )

  if (k > max_jp) {
    warning(
      "The selected number of joinpoints (",
      k,
      ") may be too high for your ",
      "time series with ",
      t,
      " time points. The recommended maximum number of ",
      "joinpoints is ",
      max_jp,
      " (check Details)."
    )
  }

  # ==========================================================
  # Validate grouping variables
  # ==========================================================

    if (!all(group %in% names(data))) {
      stop("Some grouping variable names are not present in data")
    }

    if (length(group) > 2) {
      stop("This function allows a maximum of two grouping variables")
    }
  

  # ==========================================================
  # Prepare data
  # ==========================================================

  if (!is.null(group)) {
    data <- data |>
      dplyr::mutate(
        .time = !!time,
        .log_value = log(!!value),
        .jp_group = forcats::fct_cross(
          !!!rlang::syms(group),
          sep = "_",
          keep_empty = FALSE
        )
      ) |>
      dplyr::arrange(.jp_group, .time)
  } else {
    data <- data |>
      dplyr::mutate(
        .time = !!time,
        .log_value = log(!!value),
        .jp_group = "Model 1"
      ) |>
      dplyr::arrange(.time)
  }

  # ==========================================================
  # Define groups
  # ==========================================================

  groups <- dplyr::group_keys(
    dplyr::group_by(data, .jp_group)
  )$.jp_group

  # ==========================================================
  # Function for one group
  # ==========================================================

  fit_group <- function(data) {
    # --------------------------------------------------------
    # Fit segmented regression
    # --------------------------------------------------------

    fit_jp <- function(jp) {
      x <- data$.time

      X <- data.frame(
        y = data$.log_value,
        time = x
      )

      # ---- Create hinge functions ----

      if (length(jp) >= 1) {
        X$U1.time <- pmax(x - jp[1], 0)
      }

      if (length(jp) >= 2) {
        X$U2.time <- pmax(x - jp[2], 0)
      }

      if (length(jp) >= 3) {
        X$U3.time <- pmax(x - jp[3], 0)
      }

      if (length(jp) >= 4) {
        X$U4.time <- pmax(x - jp[4], 0)
      }

      if (length(jp) >= 5) {
        X$U5.time <- pmax(x - jp[5], 0)
      }

      if (length(jp) >= 6) {
        X$U6.time <- pmax(x - jp[6], 0)
      }

      if (length(jp) >= 7) {
        X$U7.time <- pmax(x - jp[7], 0)
      }

      # ---- Fit model ----

      if (length(jp) == 0) {
        stats::lm(
          y ~ time,
          data = X
        )
      } else if (length(jp) == 1) {
        stats::lm(
          y ~ time + U1.time,
          data = X
        )
      } else if (length(jp) == 2) {
        stats::lm(
          y ~ time + U1.time + U2.time,
          data = X
        )
      } else if (length(jp) == 3) {
        stats::lm(
          y ~ time + U1.time + U2.time + U3.time,
          data = X
        )
      } else if (length(jp) == 4) {
        stats::lm(
          y ~ time + U1.time + U2.time + U3.time + U4.time,
          data = X
        )
      } else if (length(jp) == 5) {
        stats::lm(
          y ~ time +
            U1.time +
            U2.time +
            U3.time +
            U4.time +
            U5.time,
          data = X
        )
      } else if (length(jp) == 6) {
        stats::lm(
          y ~ time +
            U1.time +
            U2.time +
            U3.time +
            U4.time +
            U5.time +
            U6.time,
          data = X
        )
      } else {
        stats::lm(
          y ~ time +
            U1.time +
            U2.time +
            U3.time +
            U4.time +
            U5.time +
            U6.time +
            U7.time,
          data = X
        )
      }
    }

    # --------------------------------------------------------
    # Calculate Joinpoint BIC
    # --------------------------------------------------------

    bic_jp <- function(mod, n_jp) {
      mse <- mean(stats::residuals(mod)^2)
      n <- stats::nobs(mod)

      log(mse) +
        2 * n_jp * log(n) / n
    }

    # --------------------------------------------------------
    # Validate joinpoints
    # --------------------------------------------------------

    valid_jp <- function(jp) {
      idx <- match(
        jp,
        data$.time
      )

      left <- idx[1] - 1

      if (length(jp) > 1) {
        middle <- diff(idx) - 1
      } else {
        middle <- numeric(0)
      }

      right <-
        nrow(data) - idx[length(idx)]

      all(
        left >= min_dist,
        middle >= min_dist,
        right >= min_dist
      )
    }

    # --------------------------------------------------------
    # Model results
    # --------------------------------------------------------

    results <- list()

    # ========================================================
    # 0 joinpoints
    # ========================================================

    mod0 <- fit_jp(numeric(0))

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
        BIC = bic_jp(mod0, 0)
      )

    # ========================================================
    # 1 joinpoint
    # ========================================================

    if (k >= 1) {
      for (jp1 in data$.time) {
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
            jp3 = NA_real_,
            jp4 = NA_real_,
            jp5 = NA_real_,
            jp6 = NA_real_,
            jp7 = NA_real_,
            BIC = bic_jp(mod, 1)
          )
      }
    }

    # ========================================================
    # 2 joinpoints
    # ========================================================

    if (k >= 2) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
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
              jp3 = NA_real_,
              jp4 = NA_real_,
              jp5 = NA_real_,
              jp6 = NA_real_,
              jp7 = NA_real_,
              BIC = bic_jp(mod, 2)
            )
        }
      }
    }

    # ========================================================
    # 3 joinpoints
    # ========================================================

    if (k >= 3) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.time) {
            if (jp3 <= jp2) {
              next
            }

            jp <- c(jp1, jp2, jp3)

            if (!valid_jp(jp)) {
              next
            }

            mod <- fit_jp(jp)

            results[[length(results) + 1]] <-
              tibble::tibble(
                n_jp = 3,
                jp1 = jp1,
                jp2 = jp2,
                jp3 = jp3,
                jp4 = NA_real_,
                jp5 = NA_real_,
                jp6 = NA_real_,
                jp7 = NA_real_,
                BIC = bic_jp(mod, 3)
              )
          }
        }
      }
    }

    # ========================================================
    # 4 joinpoints
    # ========================================================

    if (k >= 4) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.time) {
              if (jp4 <= jp3) {
                next
              }

              jp <- c(jp1, jp2, jp3, jp4)

              if (!valid_jp(jp)) {
                next
              }

              mod <- fit_jp(jp)

              results[[length(results) + 1]] <-
                tibble::tibble(
                  n_jp = 4,
                  jp1 = jp1,
                  jp2 = jp2,
                  jp3 = jp3,
                  jp4 = jp4,
                  jp5 = NA_real_,
                  jp6 = NA_real_,
                  jp7 = NA_real_,
                  BIC = bic_jp(mod, 4)
                )
            }
          }
        }
      }
    }

    # ========================================================
    # 5 joinpoints
    # ========================================================

    if (k >= 5) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.time) {
              if (jp4 <= jp3) {
                next
              }

              for (jp5 in data$.time) {
                if (jp5 <= jp4) {
                  next
                }

                jp <- c(
                  jp1,
                  jp2,
                  jp3,
                  jp4,
                  jp5
                )

                if (!valid_jp(jp)) {
                  next
                }

                mod <- fit_jp(jp)

                results[[length(results) + 1]] <-
                  tibble::tibble(
                    n_jp = 5,
                    jp1 = jp1,
                    jp2 = jp2,
                    jp3 = jp3,
                    jp4 = jp4,
                    jp5 = jp5,
                    jp6 = NA_real_,
                    jp7 = NA_real_,
                    BIC = bic_jp(mod, 5)
                  )
              }
            }
          }
        }
      }
    }

    # ========================================================
    # 6 joinpoints
    # ========================================================

    if (k >= 6) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.time) {
              if (jp4 <= jp3) {
                next
              }

              for (jp5 in data$.time) {
                if (jp5 <= jp4) {
                  next
                }

                for (jp6 in data$.time) {
                  if (jp6 <= jp5) {
                    next
                  }

                  jp <- c(
                    jp1,
                    jp2,
                    jp3,
                    jp4,
                    jp5,
                    jp6
                  )

                  if (!valid_jp(jp)) {
                    next
                  }

                  mod <- fit_jp(jp)

                  results[[length(results) + 1]] <-
                    tibble::tibble(
                      n_jp = 6,
                      jp1 = jp1,
                      jp2 = jp2,
                      jp3 = jp3,
                      jp4 = jp4,
                      jp5 = jp5,
                      jp6 = jp6,
                      jp7 = NA_real_,
                      BIC = bic_jp(mod, 6)
                    )
                }
              }
            }
          }
        }
      }
    }

    # ========================================================
    # 7 joinpoints
    # ========================================================

    if (k >= 7) {
      for (jp1 in data$.time) {
        for (jp2 in data$.time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.time) {
              if (jp4 <= jp3) {
                next
              }

              for (jp5 in data$.time) {
                if (jp5 <= jp4) {
                  next
                }

                for (jp6 in data$.time) {
                  if (jp6 <= jp5) {
                    next
                  }

                  for (jp7 in data$.time) {
                    if (jp7 <= jp6) {
                      next
                    }

                    jp <- c(
                      jp1,
                      jp2,
                      jp3,
                      jp4,
                      jp5,
                      jp6,
                      jp7
                    )

                    if (!valid_jp(jp)) {
                      next
                    }

                    mod <- fit_jp(jp)

                    results[[length(results) + 1]] <-
                      tibble::tibble(
                        n_jp = 7,
                        jp1 = jp1,
                        jp2 = jp2,
                        jp3 = jp3,
                        jp4 = jp4,
                        jp5 = jp5,
                        jp6 = jp6,
                        jp7 = jp7,
                        BIC = bic_jp(mod, 7)
                      )
                  }
                }
              }
            }
          }
        }
      }
    }

    # ========================================================
    # Full list of models
    # ========================================================

    results <- dplyr::bind_rows(results) |>
      dplyr::arrange(BIC)

    # ========================================================
    # Select best model
    # ========================================================

    best <- results |>
      dplyr::slice_min(
        BIC,
        n = 1,
        with_ties = FALSE
      )

    # ========================================================
    # Selected joinpoints
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
      stats::na.omit()

    # ========================================================
    # Final model
    # ========================================================

    model <- fit_jp(joinpoints)

    # ========================================================
    # Return
    # ========================================================

    list(
      model = model,
      joinpoints = joinpoints,
      time = data$.time,
      log_rate = data$.log_value,
      BIC = best$BIC,
      results = results
    )
  }

  # ==========================================================
  # Fit each group
  # ==========================================================

  data_split <- data |>
    dplyr::group_by(.jp_group) |>
    dplyr::group_split()

  mods <- purrr::map(
    data_split,
    fit_group
  )

  # ==========================================================
  # Name models
  # ==========================================================

  names(mods) <- as.character(groups)

  # ==========================================================
  # Message
  # ==========================================================

  purrr::iwalk(
    mods,
    ~ {
      jp <- .x$joinpoints

      message(
        if (is.na(.y)) {
          "Model"
        } else {
          .y
        },
        " | Joinpoint(s): ",
        if (length(jp) == 0) {
          "No joinpoints detected"
        } else {
          paste(
            scales::number(
              jp,
              big.mark = ""
            ),
            collapse = "; "
          )
        }
      )
    }
  )

  # ==========================================================
  # Return
  # ==========================================================

  mods
}
