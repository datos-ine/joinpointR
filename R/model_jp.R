#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models by groups for age-standardized rates,
#' using a stepwise procedure based on the Bayesian Information Criterion (BIC).
#' Internally calls \code{segmented::selgmented()} and applies a log transformation
#' to the response variable.
#'
#' @param data Data frame containing age-standardized rates.
#' @param value Response variable (character).
#' @param time Time variable (character).
#' @param group Grouping variable (character).
#' @param k Maximum number of joinpoints (integer).
#' @param test Test for differences in the t-values of the slope (logical).
#'
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
#' mods <- model_jp(data = df, value = "rate", time = "year", group = "group", k = 2, test = TRUE)
#'
#' # Show the output of the first model
#' mods$cyl_6
#' summary(mods$cyl_6)

model_jp <- function(data, value, time, group, k = 2, test = TRUE) {
  # ---- Validations ----
  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop("The response variable must be > 0 to apply log() transformation")
  }

  # ---- Formula ----
  formula <- stats::reformulate(
    termlabels = time,
    response = paste0("log(", value, ")")
  )

  # ---- Model fit ----
  groups <- unique(data[[group]])

  mods <- data |>
    dplyr::group_by(.data[[group]]) |>
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

  # ---- Name models ----
  rlang::set_names(mods, groups)
}
