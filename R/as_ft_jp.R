#' Summary Flextable for Joinpoint Regression Models
#'
#' Creates a flextable object from the output of \code{summary_jp()}, displaying
#' the number of joinpoints, time periods, Annual Percent Change (APC) with
#' 95% confidence intervals, and Average Annual Percent Change (AAPC) with
#' statistical significance.
#'
#' @param tab A tibble returned by \code{summary_jp()}.
#' @param digits Integer. Number of decimal places used to display the results.
#' @param lan Character. Output language: "en" (English) or "es" (Spanish).
#'
#' @return
#' A `flextable` object containing summary statistics for the joinpoint
#' regression models.
#'
#' @author Tamara Ricardo
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(
#'   data = hiv_data,
#'   value = hiv_rate,
#'   time = year,
#'   group = c("region", "sex")
#' )
#'
#' # Generate a flextable summary
#' tab <- summary_jp(mods, digits = 1)
#' jp_to_ft(tab)
#'
#' # Change table language
#' jp_to_ft(tab, lan = "es")
#' @export

jp_to_ft <- function(
  tab,
  digits = 1,
  lan = c("en", "es")
) {
  # ---- Language settings ----
  lan <- match.arg(lan)

  # ---- language settings ----
  if (lan == "es") {
    tab <- tab |>
      dplyr::rename(
        Grupo = group,
        Periodo = period,
        JP = jp
      ) |>
      tidyr::unite(c(CI_low, CI_upp), col = "IC", sep = "; ") |>
      dplyr::mutate(IC = dplyr::if_else(IC == "NA; NA", NA_character_, IC)) |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(APC, IC, AAPC),
          .fns = ~ stringr::str_replace_all(.x, "\\.", "\\,")
        )
      )

    if ("subgroup" %in% names(tab)) {
      tab <- tab |>
        dplyr::rename(Subgrupo = subgroup)
    }

    footnote_txt <- paste0(
      "* p < 0,05\n",
      "JP: cantidad de joinpoints; APC: cambio porcentual anual; ",
      "IC: intervalo de confianza al 95%; AAPC: cambio porcentual anual promedio (IC 95%)."
    )
  } else {
    tab <- tab |>
      dplyr::rename(
        Group = group,
        Period = period,
        JP = jp
      ) |>
      tidyr::unite(c(CI_low, CI_upp), col = "CI", sep = "; ") |>

      dplyr::mutate(IC = dplyr::if_else(IC == "NA; NA", NA_character_, IC))

    if ("subgroup" %in% names(tab)) {
      tab <- tab |>
        dplyr::rename(Subgroup = subgroup)
    }

    footnote_txt <- paste0(
      "* p < 0.05\n",
      "JP: number of joinpoints; APC: annual percent change; ",
      "CI: 95% confidence interval; AAPC: average annual percent change (95% CI)."
    )
  }

  # ---- Flextable object ----
  tab |>
    flextable::flextable() |>
    flextable::bold(part = "header") |>
    flextable::merge_v(j = 1) |>
    flextable::colformat_char(j = ~ . - AAPC, na_str = "-") |>
    flextable::add_body_row(
      top = FALSE,
      values = list(footnote_txt),
      colwidths = ncol(tab)
    ) |>
    flextable::hline_bottom(border = officer::fp_border(width = 0))
}
