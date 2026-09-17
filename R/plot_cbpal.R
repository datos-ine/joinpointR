#' Display colorblind-friendly palettes
#'
#' Plots a list of the available colorblind-friendly palettes used for
#' `gg_jpoint()` plots. Allows to select by category, fairness and color
#' brightness. Internally calls `cols4all`.
#'
#' @param type Character. Type of palette to display: categorical (`"cat"`),
#' sequential (`"seq"`) or diverging (`"div"`). Defaults to `"seq"`.
#' @param colors Character. Defines the type of colors present in the palette:
#' `"vivid"` selects palettes with bright colors, `"pastel"` selects palettes with
#'  pastel colors, `"fair"` selects palettes with a mix of vivid and pastel colors,
#' `"all"` selects all the available palettes. Defaults to `"all"`.
#' @param n Numeric. Number of colors to display (Min = 3, Max = 13). Defaults to 3.
#'
#' @return
#' A `ggplot` object showing available palettes.
#' @export
plot_cbpal <- function(
  type = c("seq", "cat", "div"),
  colors = c("all", "vivid", "fair", "pastel"),
  n = 3
) {
  # ============================================================
  # ---- Set defaults ----
  # ============================================================
  pal_type <- match.arg(type)

  pal_colors <- match.arg(colors)

  # ============================================================
  # ---- Validations ----
  # ============================================================
  # ---- Validate number of colors ----
  if (n < 3 || n > 13) {
    stop(
      "The selected number of colors must be between 3 and 13.",
      call. = FALSE
    )
  }

  # ============================================================
  # ---- Get palette data ----
  # ============================================================
  dat <- get_cbpal(
    type = pal_type,
    colors = pal_colors,
    n = n
  )

  # ============================================================
  # ---- Return ----
  # ============================================================
  dat |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = position,
        y = name,
        fill = color
      )
    ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
