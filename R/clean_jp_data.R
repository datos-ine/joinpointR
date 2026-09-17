#' Prepare data for joinpoint regression
#'
#' @param data Name of the dataset containing the rates and times.
#' @param rate Name of the variable containing the standardized rates.
#' @param time Name of the variable containing the time points. Defaults to 'year'.
#' @param group Character. Name of the variable(s) used for grouping data.
#' Allows a maximum of two variables. Defaults to 'NULL'.
#' @param k Integer. Maximum number of joinpoints to test. Allows a maximum of 7
#' breakpoints. Defaults to 2.
#'
#' @keywords internal
clean_jp_data <- function(
  data,
  rate,
  time = year,
  group = NULL,
  k = 2
) {
  # ============================================================
  # ---- Set defaults ----
  # ============================================================
  # --- Response variable ---
  rate <- rlang::ensym(rate)

  # --- Vector of times ---
  time <- rlang::ensym(time)

  # --- Number of levels of time ---
  time_n <- if (!is.null(time)) {
    dplyr::n_distinct(data |> dplyr::select(dplyr::all_of(time)))
  }

  # --- Maximum number of joinpoints ---
  max_jp <- max(
    0,
    min(
      7,
      floor((time_n - 2) / 5)
    )
  )

  # ============================================================
  # ---- Validations ----
  # ============================================================
  # ---- Response variable ----
  if (is.null(rate)) {
    stop(
      call. = FALSE
    )
  }

  # ---- Logaritmic transformation ----
  if (any(data[[rate]] <= 0, na.rm = TRUE)) {
    stop(
      "The response variable must be > 0 to apply 'log()' transformation",
      call. = FALSE
    )
  }

  # ---- Maximum number of joinpoints -----
  if (k > 7) {
    stop(
      "This function supports a maximum of 7 joinpoints"
    )
  }

  #  ---- Grouping variable(s) name(s) ----
  if (!is.null(group) && !all(group %in% names(data))) {
    stop(
      "Some grouping variable names are not present in the dataset.",
      call. = FALSE
    )
  }

  # ---- Number of grouping variables ----
  if (length(group) > 2) {
    stop(
      "This function allows a maximum of two grouping variables.",
      call. = FALSE
    )
  }

  # ---- Maximum number of joinpoints ----
  if (k > max_jp) {
    warning(
      paste0(
        "The selected number of joinpoints (",
        k,
        ") is too high for the length of your time series (",
        time_n,
        " time points). It is recommended to set k to a maximum of ",
        max_jp,
        "joinpoints."
      ),
      call. = FALSE
    )
  }

  # ============================================================
  # ---- Clean data ----
  # ============================================================
  data <- data |>
    # --- Rename variables ---
    dplyr::rename(
      time = dplyr::all_of(time),
      rate = dplyr::all_of(rate)
    ) |>

    # --- Create new variables ---
    dplyr::mutate(
      log_rate = log(rate),
      group_var = if (!is.null(group)) {
        interaction(!!!rlang::syms(group), sep = "_")
      } else {
        NA
      }
    ) |>

    dplyr::select(
      time,
      rate,
      log_rate,
      group_var,
      group = group[1],
      subgroup = if (length(group) == 2) group[2] else NULL
    ) |>

    # --- Sort data ---
    dplyr::arrange(time)

  # ============================================================
  # ---- Return ----
  # ============================================================
  list(
    data,
    groups = group,
    time_points = time_n,
    k = k,
    max_jp = max_jp
  )
}
