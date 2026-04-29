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
#' get_apc(mod, digits = 1, time = "year", dec = ".")
#' }
get_apc <- function(mod, digits = 1, time = "year", dec = ".") {
  segmented::slope(mod, APC = TRUE)[[time]] |>
    dplyr::as_tibble() |>
    dplyr::rename(
      APC = 1,
      lci = 2,
      uci = 3
    ) |>

    # ---- APC ----
    dplyr::mutate(
      APC = scales::number(
        APC,
        accuracy = 10^-digits,
        decimal.mark = dec,
        suffix = "%"
      )
    ) |>
    # ---- 95% CI ----
    dplyr::mutate(
      CI = paste0(
        scales::number(
          lci,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        ),
        "; ",
        scales::number(
          uci,
          accuracy = 10^-digits,
          decimal.mark = dec,
          suffix = "%"
        )
      )
    ) |>

    # ---- Discard columns ----
    dplyr::select(-lci, -uci)
}
