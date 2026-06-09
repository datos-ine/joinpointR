#' Annual Percent Change by Segment (APC)
#'
#' Calculates the Annual Percent Change (APC) for each model segment and its 95%
#' confidence interval.
#'
#' @param mods Joinpoint regression models (segmented objects).
#' @param digits Number of decimal places to display (integer).
#' @param dec Character. Decimal separator to use (e.g., "." or ",").
#'
#' @return A tibble with APCs and 95% CI for each segment of each model.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Load example data
#'data(hiv_data)
#'
#' names(hiv_data)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = hiv_data, value = "hiv_rate", time = "year", group = "region", k = 2, test = TRUE)
#'
#' # Obtain APC (95% CI)
#' get_apc(mods, digits = 1, dec = ".")
#'
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
