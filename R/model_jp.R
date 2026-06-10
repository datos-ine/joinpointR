#' Joinpoint Regression Models by Groups
#'
#' Fits segmented linear regression models by groups for age-standardized rates
#' using joinpoint regression. Models can be fitted using either a stepwise
#' selection procedure based on the Bayesian Information Criterion (BIC) or a
#' fixed number of joinpoints. Internally, the function calls
#' \code{segmented::selgmented()} or \code{segmented::segmented()} and applies
#' a log transformation to the response variable.
#'
#' @param data A data frame containing age-standardized rates.
#' @param value Response variable.
#' @param time Time variable.
#' @param group Names of one or more grouping variables.
#' @param k Maximum number of joinpoints to estimate.
#' @param step Logical. If \code{TRUE}, uses a stepwise procedure to select the
#' number of joinpoints based on BIC. If \code{FALSE}, fits a model with a
#' fixed number of joinpoints specified by \code{k}.
#' @param test Logical. If \code{TRUE}, tests differences in slope t-values
#' during the stepwise selection procedure. Only used when
#' \code{step = TRUE}.
#' @return A named list of joinpoint regression models by group.
#' @author Tamara Ricardo
#' @details
#' The National Cancer Institute (NCI) recommends the following maximum number
#' of joinpoints according to the length of the time series (Kim et al., 2000):
#'
#' \itemize{
#' \item 0--6 time points: 0 joinpoints.
#' \item 7--11 time points: 1 joinpoint.
#' \item 12--16 time points: 2 joinpoints.
#' \item 17--21 time points: 3 joinpoints.
#' \item 22--26 time points: 4 joinpoints.
#' \item 27--31 time points: 5 joinpoints.
#' \item 32--36 time points: 6 joinpoints.
#' \item 37 or more time points: 7 joinpoints.
#' }
#'
#' #' @references
#' Kim HJ, Fay MP, Feuer EJ, Midthune DN (2000).
#' "Permutation Tests for Joinpoint Regression with Applications to Cancer Rates."
#' \emph{Statistics in Medicine}, 19(3), 335--351.
#' doi:10.1002/(sici)1097-0258(20000215)19:3<335::aid-sim336>3.0.co;2-z.
#'
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' # Check group levels
#' levels(hiv_data$region)
#'
#' # Fit models
#' mods <- model_jp(data = hiv_data, value = hiv_rate, time = year, group = c("region", "sex"),
#'  k = 2, step = TRUE, test = TRUE)
#'
#' # Show the output of the first model by calling its index
#' mods[[1]]
#'
#' # Same output will be obtained when calling model name
#' mods$Central
#'
#' @export

model_jp <- function(
  data,
  value,
  time,
  group,
  k = 2,
  step = TRUE,
  test = TRUE
) {
  # ---- Define variables ----
  value <- rlang::ensym(value)
  time <- rlang::ensym(time)

  value_name <- rlang::as_string(value)
  time_name <- rlang::as_string(time)

  # ---- Validate response variable ----
  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop("The response variable must be > 0 to apply log() transformation")
  }

  # ---- Validate grouping variable/s ----
  if (!all(group %in% names(data))) {
    stop("Some grouping variable names are not present in data")
  }

  # ---- Validate number of joinpoints ----
  if (!step && k > 1 && dplyr::n_distinct(data[[time]]) < 11) {
    message(
      "Note: fitting two or more joinpoints is not recommended for short time series (see Details)."
    )
  }

  # ---- Validate test and step arguments ----
  if (!step && test) {
    message("'test' is ignored when step = FALSE")
  }

  # ---- Prepare data ----
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
    dplyr::group_by(.data$.jp_group)

  # ---- Group names ----
  groups <- dplyr::group_keys(data)$.jp_group

  # ---- Fit the linear model ----
  lm_fit <- function(.x) {
    stats::lm(
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
              olm = stats::lm(
                formula = log(Y) ~ X
              ),
              Kmax = K
            ),
            list(
              Y = value,
              X = time,
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
              obj = stats::lm(
                formula = log(Y) ~ X
              ),
              seg.Z = ~X,
              npsi = K
            ),
            list(
              Y = value,
              X = time,
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
