#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models by groups for age-standardized rates
#' using joinpoint regression. Models can be fitted using either a stepwise
#' selection procedure based on the Bayesian Information Criterion (BIC) or a
#' fixed number of joinpoints. Internally calls
#' \code{segmented::selgmented()} or \code{segmented::segmented()} and applies
#' a log transformation to the response variable.
#'
#' @param data Data frame containing age-standardized rates.
#' @param value Name of the response variable (character).
#' @param time Name of the time variable (character).
#' @param group Name of one or more grouping variables (character vector).
#' @param k Maximum number of joinpoints to be estimated (integer).
#' @param step Use stepwise procedure to select the number of joinpoints (logical).
#' @param test whether to test for differences in slope t-values during the stepwise selection
#' procedure. Only used when \code{step = TRUE}.
#' @return A list of models by group.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Generate example data
#' library(dplyr)
#' df <- mtcars |>
#' mutate(
#'   year = seq(2000, length.out = n(), by = 1),
#'   group = factor(paste("cyl", cyl, sep = "_")),
#'   rate = mpg
#' ) |>
#' select(year, group, rate)
#'
#' # Check group levels
#' levels(df$group)
#'
#' # Fit models
#' mods <- model_jp(data = df, value = "rate", time = "year", group = "group", k = 2, step = TRUE, test = TRUE)
#'
#' # Show the output of the first model
#' mods$cyl_6
#' summary(mods$cyl_6)

model_jp <- function(
  data,
  value,
  time,
  group,
  k = 2,
  step = TRUE,
  test = TRUE
) {
  # ---- Validate response variable ----
  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop("The response variable must be > 0 to apply log() transformation")
  }

  # ---- Validate grouping variable/s ----
  if (!all(group %in% names(data))) {
    stop("Some grouping variables are not present in data")
  }

  # ---- Validate test and step arguments ----
  if (!step && test) {
    warning("'test' is ignored when step = FALSE")
  }

  # ---- Prepare data ----
  data <- data |>
    # Transform the response variable
    dplyr::mutate(
      .jp_log_value = log(.data[[value]])
    ) |>

    # Create grouping variable
    dplyr::mutate(
      .jp_group = interaction(
        !!!rlang::syms(group),
        sep = "_",
        drop = TRUE
      )
    ) |>

    # Group data
    dplyr::group_by(.jp_group)

  # ---- Formula ----
  formula <- stats::reformulate(
    termlabels = time,
    response = ".jp_log_value"
  )

  # ---- Group names ----
  groups <- data |>
    dplyr::distinct(.jp_group) |>
    dplyr::pull(.jp_group)

  # ---- Model fit ----
  if (step) {
    mods <- data |>
      dplyr::group_map(
        ~ segmented::selgmented(
          olm = stats::lm(formula, data = .x),
          Kmax = k,
          type = "bic",
          th = 2,
          stop.if = 4,
          check.dslope = test
        )
      )
  } else {
    mods <- data |>
      dplyr::group_map(
        ~ segmented::segmented(
          obj = stats::lm(formula, data = .x),
          seg.Z = stats::as.formula(
            paste0("~", time)
          ),
          npsi = k
        )
      )
  }

  # ---- Name models ----
  rlang::set_names(mods, groups)
}
