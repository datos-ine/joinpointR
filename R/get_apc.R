#' Annual Percent Change by Segment (APC)
#'
#' Calculates the Annual Percent Change (APC) for each segment and its 95%
#' confidence interval.
#'
#' @param mod Joinpoint regression model (segmented object).
#' @param digits Number of decimal places to display (integer).
#' @param time Time variable used in the model (character).
#' @param dec Character. Decimal separator to use (e.g., "." or ",").
#'
#' @return A character vector with APC and 95% CI for each segment.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Load example data
#' data("plant", package = "segmented")
#'
#' names(plant)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = plant, value = "y", time = "time", group = "group", k = 2, test = TRUE)
#'
#' # Obtain APC (95% CI)
#' get_apc(mods$RKW, digits = 1, time = "time", dec = ".")

get_apc <- function(mod, digits = 1, time = "time", dec = ".") {
  segmented::slope(mod, APC = TRUE)[1] |>
    data.frame() |>
    dplyr::rename(APC = 1, CI_l = 2, CI_u = 3) |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ scales::number(
          .x,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        )
      )
    ) |>
    tidyr::unite("CI", CI_l, CI_u, sep = "; ")
}
