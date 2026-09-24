#' Prepare Data for Joinpoint Regression
#'
#' @description 
#' Cleans and prepares data for fitting joinpoint regression models.
#' 
#' @param data A dataset containing the rates, time points, and, optionally,
#' grouping variables.
#'
#' @param rate Name of the variable containing the standardized or crude rates.
#'
#' @param time Name of the variable containing the time points.
#'
#' @param group Character vector specifying the name(s) of the variable(s)
#' used to group the data. A maximum of two grouping variables is allowed.
#'
#' @return An object of class \code{jp_data} containing:
#' \itemize{
#' \item \code{group_data} A list of tibbles, one for each group level,
#' containing the time points, observed rates, and log-transformed rates.
#' \item \code{groups} Character vector containing the names of the
#' grouping variables.
#' \item \code{group_levels} Character vector containing the names of the groups.
#' \item \code{time_points} An integer containing the number of unique time points
#'  in the series.
#' \item \code{k} An integer containing the recommended maximum number of joinpoints to
#'  test.}
#'
#' @keywords internal

clean_jp_data <- function(
  data,
  rate,
  time,
  group = NULL
) {
  # ============================================================
  # ---- Set defaults ----
  # ============================================================
  # --- Response variable ---
  rate <- as.character(rate)

  # --- Vector of times ---
  time <- as.character(time)

  # ============================================================
  # ---- Validations ----
  # ============================================================
  # ---- Response variable ----
  if (!rate %in% names(data)) {
    stop(
      "The response variable is not present in the dataset.",
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

  # ---- Time variable ----
  if (!time %in% names(data)) {
    stop(
      "The time variable is not present in the dataset.",
      call. = FALSE
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

  # ---- Check for NAs ----
  if (anyNA(data[[time]]) || anyNA(data[[rate]])) {
    stop(
      "The data must not contain missing values.",
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
        NA_character_
      }
    ) |>

    # --- Sort data ---
    dplyr::arrange(time, group_var) |>

    # --- Select columns ---
    dplyr::select(
      time,
      rate,
      log_rate,
      group_var
    )

  # ============================================================
  # ---- Extract elements ----
  # ============================================================
  #  --- Group levels ---
  group_levels <- data |>
    dplyr::distinct(group_var) |>
    dplyr::pull(group_var) |>
    as.character()

  # --- Number of levels of time ---
  time_n <- dplyr::n_distinct(data$time)

  # --- Maximum number of joinpoints ---
  max_jp <- max(
    0,
    min(
      7,
      floor((time_n - 2) / 5)
    )
  )

  # ============================================================
  # ---- Split data by groups ----
  # ============================================================
  if (!is.null(group)) {
    data_split <- data |>
      dplyr::group_split(group_var, .keep = FALSE) |>
      purrr::set_names(group_levels)
  } else {
    data_split <- list(data)
  }

  # ============================================================
  # ---- Return ----
  # ============================================================
  structure(
    list(
      group_data = data_split,
      groups = group,
      group_levels = group_levels,
      time_points = time_n,
      k = max_jp
    ),
    class = "jp_data"
  )
}
