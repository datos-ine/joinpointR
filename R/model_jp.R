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
#' mods <- model_jp(data = df, value = "rate", time = "year", group = "group",
#'  k = 2, step = TRUE, test = TRUE)
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

  # ---- Validate number of joinpoints ----
  if (!step && k > 1 && dplyr::n_distinct(data[[time]]) < 11) {
    message(
      "Note: fitting two or more joinpoints is not recommended for short time series."
    )
  }

  # ---- Validate test and step arguments ----
  if (!step && test) {
    warning("'test' is ignored when step = FALSE")
  }

  # ---- Prepare data ----
  data <- data |>
    dplyr::mutate(
      .jp_time = .data[[time]],
      .jp_log_value = log(.data[[value]]),
      .jp_group = forcats::fct_cross(
        !!!rlang::syms(group),
        sep = "_",
        keep_empty = FALSE
      )
    ) |>
    dplyr::group_by(.data$.jp_group)

  # ---- Group names ----
  groups <- dplyr::group_keys(data)$.jp_group

  # ---- Fit the linear model ----
  lm_fit <- function(.x) {
    lm(
      .jp_log_value ~ .jp_time,
      data = .x
    )
  }

  # ---- Fit joinpoint regression by groups ----
  if (step) {
    mods <- data |>
      dplyr::group_map(
        ~ {
          mod <- segmented::selgmented(
            olm = lm_fit(.x),
            Kmax = k,
            type = "bic",
            th = 2,
            stop.if = 4,
            check.dslope = test,
            msg = FALSE
          )

          mod$call <- substitute(
            segmented::selgmented(
              olm = lm(
                formula = log(Y) ~ X
              ),
              Kmax = K
            ),
            list(
              Y = as.name(value),
              X = as.name(time),
              K = k
            )
          )

          mod
        }
      )
  } else {
    mods <- data |>
      dplyr::group_map(
        ~ {
          mod <- segmented::segmented(
            obj = lm_fit(.x),
            seg.Z = ~.jp_time,
            npsi = k
          )

          mod$call <- substitute(
            segmented::segmented(
              obj = lm(
                formula = log(Y) ~ X
              ),
              seg.Z = ~X,
              npsi = K
            ),
            list(
              Y = as.name(value),
              X = as.name(time),
              K = k
            )
          )

          mod
        }
      )
  }

  # ---- Name models ----
  mods <- rlang::set_names(mods, groups)

  # ---- Messages ----
  purrr::iwalk(
    mods,
    ~ {
      jp <- tryCatch(.x$psi[, 2], error = \(e) numeric())

      message(
        "Model: ",
        .y,
        " | Joinpoint(s): ",
        dplyr::if_else(
          length(jp) == 0,
          "no significant joinpoints detected",
          paste(scales::number(jp, big.mark = ""), collapse = "; ")
        )
      )
    }
  )
}
