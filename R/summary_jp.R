#' Summary Tables for Joinpoint Regression Models
#'
#' Generates summary tables for one or more joinpoint regression models,
#' including the number of joinpoints (JP), time periods, Annual Percent
#' Change (APC) with 95% confidence intervals, and Average Annual Percent
#' Change (AAPC) with statistical significance.
#'
#' @param mods A list of models returned by \code{model_jp()}.
#' @param digits Integer. Number of decimal places used to display the results.
#' @param dec Character. Decimal separator to use (e.g. `"."` or `","`).
#'
#' @return
#' A tibble with one row per segment containing the grouping variable(s),
#' number of joinpoints (JP), time period(s), APC and its 95% confidence
#' interval, and AAPC with significance stars.
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
#'   value = hiv_rate,
#'   time = year,
#'   group = "region"
#' )
#'
#' # Summarize models
#' summary_jp(mods, digits = 1, dec = ".")
#'
#' @export

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
      # Time
      time <- mod$time

      # Joinpoints
      jp <- length(mod$joinpoints)

      # Breaks
      breaks <- sort(c(
        min(time, na.rm = TRUE),
        mod$joinpoints,
        max(time, na.rm = TRUE)
      ))

      # Period
      period <- paste(
        round(head(breaks, -1)),
        round(tail(breaks, -1)),
        sep = "-"
      )

      # ---- APC ----

      tab <- get_apc(
        list(mod),
        digits = digits,
        dec = dec
      ) |>
        dplyr::as_tibble() |>

        # Add columns
        dplyr::mutate(
          group = group,
          jp = jp,
          period = period,
          .before = 1
        ) |>

        # ---- Add AAPC ----

        dplyr::mutate(
          AAPC = dplyr::if_else(
            dplyr::row_number() == 1,
            get_aapc(
              list(mod),
              digits = digits,
              show_ci = FALSE,
              dec = dec
            )$AAPC,
            NA_character_
          )
        ) |>

        # ---- Separate grouping variables ----

        tidyr::separate_wider_delim(
          cols = group,
          names = c("group", "subgroup"),
          delim = "_",
          cols_remove = TRUE,
          too_few = "align_start"
        )

      # ---- Remove unnecessary columns ----

      if (all(is.na(tab$subgroup))) {
        tab |>
          dplyr::select(-subgroup, -model, -segment)
      } else {
        tab |>
          dplyr::select(-model, -segment)
      }
    }
  )
}
