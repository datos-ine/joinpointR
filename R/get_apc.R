#' Annual Percent Change by Segment
#'
#' Calculates the Annual Percent Change (APC) and corresponding 95% confidence
#' intervals for each segment of one or more joinpoint regression models.
#'
#' @param mods A joinpoint regression model or a list of joinpoint regression
#'   models returned by \code{model_jp()}.
#' @param digits Integer. Number of decimal places used to display the results.
#' @param dec Character. Decimal separator to use (e.g. `"."` or `","`).
#'
#' @return
#' A tibble with one row per segment and the variables
#' `group`, `segment`, `apc`, `lower`, and `upper`, where `lower`
#' and `upper` correspond to the limits of the 95\% confidence interval.
#'
#' @author Tamara Ricardo
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Fit joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = "hiv_rate",
#'   time = "year",
#'   group = "region",
#'   k = 2,
#'   test = TRUE
#' )
#'
#' # APC and 95% confidence intervals for all models
#' get_apc(mods, digits = 1, dec = ".")
#'
#' # APC and 95% confidence intervals for a single model
#' get_apc(mods$Central)
#'
#' @export

get_apc <- function(
  mods,
  digits = 1,
  dec = "."
) {
  # ---- Allow a single model or a list of models ----
  if (inherits(mods, c("lm", "segmented"))) {
    mods <- list(mods)
  }

  # ---- Estimate APC ----
  purrr::map_dfr(
    mods,
    \(mod) {
      # ---- Linear models ----
      if (!inherits(mod, "segmented")) {
        return(
          tibble::tibble(
            segment = NA_character_,
            APC = NA_character_,
            CI_low = NA_character_,
            CI_upp = NA_character_
          )
        )
      }

      # ---- Segmented models ----
      apc <- segmented::slope(mod, APC = TRUE)[[1]] |>
        # Transform to dataframe
        data.frame() |>
        # Extract the number of segments
        tibble::rownames_to_column("segment") |>
        # Rename variables
        dplyr::rename(
          APC = 2,
          CI_low = 3,
          CI_upp = 4
        ) |>
        # Modify variable levels
        dplyr::mutate(segment = stringr::str_remove(segment, "slope")) |>
        dplyr::mutate(
          dplyr::across(
            c(APC, CI_low, CI_upp),
            ~ scales::number(
              .x,
              accuracy = 10^-digits,
              decimal.mark = dec,
              suffix = "%"
            )
          )
        )
    },
    .id = "model"
  )
}
