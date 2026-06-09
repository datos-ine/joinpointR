#' Summary Tables for Joinpoint Regression Models
#'
#' Generates a summary table containing the number of joinpoints (JP),
#' time periods, APC and its 95% confidence interval, and AAPC with
#' statistical significance for each joinpoint regression model.
#'
#' @param mods List of joinpoint regression models (output of model_jp()).
#' @param digits Number of decimal places to display (integer).
#' @param dec Character used as decimal separator. Must be "." or ",".
#'
#' @return
#' A tibble containing the grouping variables, number of joinpoints (JP),
#' period, APC, 95% confidence interval, and AAPC for each model.
#'
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' # Fit the joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = "hiv_rate",
#'   time = "year",
#'   group = "region"
#' )
#'
#' # Summarize models
#' summary_jp(mods, digits = 1, dec = ".")

summary_jp <- function(
  mods,
  digits = 1,
  dec = c(".", ",")
) {
  # ---- Default values ----
  dec <- match.arg(dec)

  # ---- Summary table ----
  purrr::imap_dfr(
    mods,
    \(mod, group) {
      # ---- Calculate periods ----
      if (inherits(mod, "segmented")) {
        # Time
        time <- names(mod$model)[2]

        # Breaks
        breaks <- sort(c(
          min(mod$model[[time]]),
          mod$psi[, "Est."],
          max(mod$model[[time]])
        ))

        # Joinpoints
        jp = nrow(mod$psi)
      } else {
        # Breaks
        breaks <- NA_character_

        # Joinpoints
        jp <- 0
      }

      # ---- Generate table ----
      tab <- get_apc(
        mod,
        digits = digits,
        dec = dec
      ) |>

        # Transform to tibble
        dplyr::as_tibble() |>

        # Add columns
        dplyr::mutate(
          group = group,
          jp = jp,
          period = dplyr::if_else(
            jp != 0,
            paste(
              round(head(breaks, -1)),
              round(tail(breaks, -1)),
              sep = "-"
            ),
            NA_character_
          ),
          .before = 1
        ) |>

        # Add AAPC
        dplyr::mutate(
          AAPC = dplyr::if_else(
            dplyr::row_number() == 1,
            get_aapc(
              mod,
              digits = digits,
              show_ci = FALSE,
              dec = dec
            )$AAPC,
            NA_character_
          )
        ) |>

        # Separate grouping variables
        tidyr::separate_wider_delim(
          cols = group,
          names = c("group", "subgroup"),
          delim = "_",
          cols_remove = TRUE,
          too_few = "align_start"
        )

      if (all(is.na(tab$subgroup))) {
        tab <- tab |> dplyr::select(-subgroup, -model, -segment)
      } else {
        tab <- tab |> dplyr::select(-model, -segment)
      }
    }
  )
}
