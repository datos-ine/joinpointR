#' Display colorblind-friendly palettes
#'
#' Plots a list of the available colorblind-friendly palettes used for
#' `gg_jpoint()` plots. Allows to select by category, fairness and color brightness. Internally calls `cols4all`.
#'
#' @param type Character. Type of palette to display: categorical (`"cat"`),
#' sequential (`"seq"`), diverging (`"div"`) or `"all"`. Defaults to `"all"`.
#' @param series Character. Palette collection. Defaults to `"scico"`.
#' @param fair Character. The palette contains an equal amount of vivid and
#' pastel colors. Defaults to `"No"`.
#' @param n Numeric. Number of colors to display (Min = 3, Max = 13). Defaults to 7.
#'
#' @return
#' A `ggplot` object showing available palettes.
#' @export
plot_cbpal <- function(
  type = c("all", "cat", "div", "seq"),
  fair = c("No", "Yes"),
  n = 8
) {
  # ---- Validate number of colors ----
  if (n < 3 || n > 13) {
    stop(
      "The selected number of colors for colorblind-friendly palettes must be between 3 and 13.",
      call. = FALSE
    )
  }

  # ---- Set default ----
  pal_type <- match.arg(type)

  # ---- Generate palette list ----
  pal_list <- get_cbpal() |>
    # Filter by type
    dplyr::filter(
      if (pal_type == "all") TRUE else type == pal_type
    ) |>
    # # Filter by fairness
    dplyr::filter_out(fair == !!fair) |>
    # Select palette names
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::pull(name)

  # ---- Generate palette data ----
  pal_data <- purrr::map_dfr(
    pal_list,
    \(pal) {
      tibble::tibble(
        name = pal,
        color = cols4all::c4a(
          palette = pal,
          n = n,
          verbose = FALSE
        ),
        position = seq_len(n)
      )
    }
  )

  # ---- Plot ----
  ggplot2::ggplot(
    pal_data,
    ggplot2::aes(
      x = position,
      y = name,
      fill = color
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(add = 0.5)
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
}
