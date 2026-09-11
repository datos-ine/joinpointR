#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models for rates using joinpoint
#' regression with an exhaustive Grid Search and the Bayesian Information
#' Criterion (BIC) to select the optimal model with up to five joinpoints.
#' Internally, the function applies a log transformation to the response
#' variable.
#'
#' @param data A data frame containing crude or age standardized rates.
#' @param value Response variable.
#' @param time Time variable.
#' @param group Names of the grouping variable(s). Defaults to NULL.
#' @param k Maximum number of joinpoints to estimate.
#' @param min_dist Minimum number of observations required per segment.
#'
#' @return A named list of joinpoint regression models by group.
#' @author Tamara Ricardo
#'
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' # Check group levels
#' levels(hiv_data$region)
#'
#' # Fit models
#' mods <- model_jp(
#' data = hiv_data,
#'  value = hiv_rate,
#' time = year,
#' group = c("region", "sex"),
#' k = 2,
#' min_dist = 3
#' )
#'
#' # Show the output of the first model by calling its index
#' mods[[1]]
#'
#' # Same output will be obtained when calling model name
#' mods$Central_Female
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
  # ---- Define response variable ----
  value <- rlang::ensym(value)

  # ---- Define time variable ----
  time <- rlang::ensym(time)

  # ---- Validate response variable ----
  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop(
      "The response variable must be > 0 to apply log() transformation"
    )
  }

  # ---- Validate maximum number of joinpoints ----
  if (k > 5) {
    stop("This function currently supports a maximum of 5 joinpoints")
  }

  # ---- Validate optimal number of joinpoints ----
  if (
    k > 0 &
      length(time) < 7 |
      k > 1 & length(time) < 12 |
      k > 2 & length(time) < 17 |
      k > 3 & length(time) < 22 |
      k > 4 & length(time) < 27
  ) {
    warning(
      "The selected number of joinpoints is too high for your time series (check Details)"
    )
  }

  # ---- Validate grouping variables ----
  if (!all(group %in% names(data))) {
    stop("Some grouping variable names are not present in data")
  }

  # ---- Validate number of grouping variables ----
  if (length(group) > 2) {
    stop("This function allows a maximum of two grouping variables")
  }

  # ---- Prepare data ----
  if (!is.null(group)) {
    data <- data |>
      dplyr::mutate(
        .jp_time = !!time,
        .jp_log_value = log(!!value),
        .jp_group = forcats::fct_cross(
          !!!rlang::syms(group),
          sep = "_",
          keep_empty = FALSE
        )
      ) |>
      dplyr::arrange(.jp_group, .jp_time)
  } else {
    data <- data |>
      dplyr::mutate(
        .jp_time = !!time,
        .jp_log_value = log(!!value),
        .jp_group = NA_character_
      ) |>
      dplyr::arrange(.jp_time)
  }

  # ---- Define groups ----
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
      x <- data$.jp_time

      X <- data.frame(
        y = data$.jp_log_value,
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
      } else {
        stats::lm(
          y ~ time + U1.time + U2.time + U3.time + U4.time + U5.time,
          data = X
        )
      }
    }

    # --------------------------------------------------------
    # Validate joinpoints
    # --------------------------------------------------------
    valid_jp <- function(jp) {
      idx <- match(
        jp,
        data$.jp_time
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

    # --------------------------------------------------------
    # 0 joinpoints
    # --------------------------------------------------------
    mod0 <- fit_jp(numeric(0))

    results[[length(results) + 1]] <-
      tibble::tibble(
        n_jp = 0,
        jp1 = NA_real_,
        jp2 = NA_real_,
        jp3 = NA_real_,
        jp4 = NA_real_,
        jp5 = NA_real_,
        BIC = stats::BIC(mod0)
      )

    # --------------------------------------------------------
    # 1 joinpoint
    # --------------------------------------------------------
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
            jp3 = NA_real_,
            jp4 = NA_real_,
            jp5 = NA_real_,
            BIC = stats::BIC(mod)
          )
      }
    }

    # --------------------------------------------------------
    # 2 joinpoints
    # --------------------------------------------------------
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
              jp3 = NA_real_,
              jp4 = NA_real_,
              jp5 = NA_real_,
              BIC = stats::BIC(mod)
            )
        }
      }
    }

    # --------------------------------------------------------
    # 3 joinpoints
    # --------------------------------------------------------
    if (k >= 3) {
      for (jp1 in data$.jp_time) {
        for (jp2 in data$.jp_time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.jp_time) {
            if (jp3 <= jp2) {
              next
            }

            jp <- c(
              jp1,
              jp2,
              jp3
            )

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
                BIC = stats::BIC(mod)
              )
          }
        }
      }
    }

    # --------------------------------------------------------
    # 4 joinpoints
    # --------------------------------------------------------
    if (k >= 4) {
      for (jp1 in data$.jp_time) {
        for (jp2 in data$.jp_time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.jp_time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.jp_time) {
              if (jp4 <= jp3) {
                next
              }

              jp <- c(
                jp1,
                jp2,
                jp3,
                jp4
              )

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
                  BIC = stats::BIC(mod)
                )
            }
          }
        }
      }
    }

    # --------------------------------------------------------
    # 5 joinpoints
    # --------------------------------------------------------
    if (k >= 5) {
      for (jp1 in data$.jp_time) {
        for (jp2 in data$.jp_time) {
          if (jp2 <= jp1) {
            next
          }

          for (jp3 in data$.jp_time) {
            if (jp3 <= jp2) {
              next
            }

            for (jp4 in data$.jp_time) {
              if (jp4 <= jp3) {
                next
              }

              for (jp5 in data$.jp_time) {
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
                    BIC = stats::BIC(mod)
                  )
              }
            }
          }
        }
      }
    }

    # --------------------------------------------------------
    # Full list of models
    # --------------------------------------------------------
    results <- dplyr::bind_rows(results) |>
      dplyr::arrange(BIC)

    # --------------------------------------------------------
    # Select best model
    # --------------------------------------------------------
    best <- results |>
      dplyr::slice_min(
        BIC,
        n = 1,
        with_ties = FALSE
      )

    # --------------------------------------------------------
    # Selected joinpoints
    # --------------------------------------------------------
    joinpoints <- best |>
      dplyr::select(
        jp1,
        jp2,
        jp3,
        jp4,
        jp5
      ) |>
      unlist(
        use.names = FALSE
      ) |>
      stats::na.omit()

    # --------------------------------------------------------
    # Final model
    # --------------------------------------------------------
    model <- fit_jp(joinpoints)

    # --------------------------------------------------------
    # Return
    # --------------------------------------------------------
    list(
      model = model,
      joinpoints = joinpoints,
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

  # ---- Name models ----
  names(mods) <- as.character(groups)

  # ---- Message ----
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

  # ---- Return ----
  mods
}
