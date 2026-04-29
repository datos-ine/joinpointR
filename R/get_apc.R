#' Annual Percent Change by Segment (APC)
#'
#' Calculates the Annual Percent Change (APC) for each segment and its 95%
#' confidence interval.
#'
#' @param mod Joinpoint regression model (segmented object).
#' @param digits Number of decimal places to display (integer).
#' @param time Time variable used in the model (character).
#'
#' @return A character vector with APC and 95% CI for each segment.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \donttest{
#' get_apc(mod, digits = 1, time = "anio", dec = ".")
#' }
get_apc <- function(mod, digits = 1, time = "anio") {
  fmt <- function(x, y, z) {
    paste0(
      scales::number(
        x,
        accuracy = 10^-digits,
        decimal.mark = dec,
        suffix = "%"
      ),
      " (IC95%: ",
      scales::number(y, accuracy = 10^-digits, decimal.mark = dec),
      ", ",
      scales::number(z, accuracy = 10^-digits, decimal.mark = dec),
      ")"
    )
  }

  segmented::slope(mod, APC = TRUE)[[time]] |>
    # as.data.frame() |>
    dplyr::as_tibble() #|>
  # purrr::pmap_chr(~ fmt(..1, ..2, ..3))
}
