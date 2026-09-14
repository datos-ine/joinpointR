#' Internal data preparation for Joinpoint models
#'
#' @keywords internal

# Validate and prepare data ----------------------------------------------
jp_data <- function(
  data,
  rate,
  time,
  group = NULL,
  k = 2
) {
  # Define variables -----------------------------------------
  rate_name <- rate
  time_name <- time

  # Validate response ----------------------------------------
  if (any(data[[rate_name]] <= 0, na.rm = TRUE)) {
    stop(
      "The response variable must be > 0 to apply 'log()' transformation"
    )
  }

  # Maximum number of joinpoints -----------------------------
  if (k > 7) {
    stop(
      "This function supports a maximum of 7 joinpoints"
    )
  }

  # Grouping variables ---------------------------------------
  if (!is.null(group)) {
    if (!all(group %in% names(data))) {
      stop(
        "Some grouping variable names are not present in data"
      )
    }

    if (length(group) > 2) {
      stop(
        "This function allows a maximum of two grouping variables"
      )
    }
  }

  # Number of time points ------------------------------------
  n <- dplyr::n_distinct(data[[time_name]])

  # Maximum recommended number of joinpoints ----------------
  max_jp <- max(
    0,
    min(
      7,
      floor((n - 2) / 5)
    )
  )

  if (k > max_jp) {
    warning(
      "The selected number of joinpoints (",
      k,
      ") may be too high for your time series with ",
      n,
      " time points. The recommended maximum number of ",
      "joinpoints is ",
      max_jp,
      " (check Details)."
    )
  }

  # Prepare data ---------------------------------------------
  data <- data |>
    dplyr::mutate(
      .time = .data[[time_name]],
      .log_rate = log(.data[[rate_name]])
    )

  # Define groups --------------------------------------------
  if (is.null(group)) {
    groups <- "All"
  } else {
    data <- data |>
      dplyr::mutate(
        .jp_group = forcats::fct_cross(
          !!!rlang::syms(group),
          sep = "_",
          keep_empty = FALSE
        )
      )

    groups <- data |>
      dplyr::distinct(.jp_group) |>
      dplyr::pull(.jp_group) |>
      as.character()
  }

  # Sort data ------------------------------------------------
  data <- data |>
    dplyr::arrange(.time)

  # Return ---------------------------------------------------
  list(
    data = data,
    groups = groups,
    n = n,
    max_jp = max_jp
  )
}

#' Fit a Joinpoint regression model
#'
#' @keywords internal
# Fit the linear model ---------------------------------------------------
fit_jp <- function(x, y, joinpoints = NULL) {
  dat <- data.frame(
    y = y,
    time = x
  )

  if (length(joinpoints) > 0) {
    for (i in seq_along(joinpoints)) {
      dat[[paste0("U", i, ".time")]] <-
        pmax(x - joinpoints[i], 0)
    }
  }

  stats::lm(
    y ~ .,
    data = dat
  )
}

#' Fit Joinpoint models by groups
#'
#' @keywords internal
#'
fit_jp_groups <- function(
  data,
  groups,
  fit_group
) {
  # Fit models ----------------------------------------------
  data_split <- data |>
    dplyr::group_by(.jp_group) |>
    dplyr::group_split()

  mods <- purrr::map(
    data_split,
    fit_group
  )

  # Name models ---------------------------------------------
  names(mods) <- as.character(
    groups
  )

  # Message -------------------------------------------------
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

  # Return ---------------------------------------------------
  mods
}
