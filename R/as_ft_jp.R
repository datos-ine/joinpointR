#' Summary Flextable For Joinpoint Regression Models
#'
#' Generates a flextable object displaying the number of joinpoints, time breaks, APC and its 95% confidence
#' interval, and AAPC and its statistical significance from a summary_jp() object.
#'
#' @param tab A tibble generated with summary_jp().
#' @param digits Number of decimal places to display (integer).
#' @param lan Language of output: "en" (English) or "es" (Spanish).
#' @return A flextable object with summary statistics.
#' @author Tamara Ricardo
#' @export
#' @examples
#' library(dplyr)
#' # Load example data
#' data("hiv_data")
#'
#' names(hiv_data)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = hiv_data, value = "hiv_rate", time = "year", group = c("region","sex"))
#'
#' # Generate summary tables
#' tab <- summary_jp(mods, digits = 1)
#'
#' # Format tables
#' as_ft_jp(tab)

as_ft_jp <- function(
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
      tidyr::unite(c(CI_low, CI_upp), col = "CI", sep = "; ")

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
