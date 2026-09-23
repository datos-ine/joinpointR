#' Display colorblind-friendly palettes
#'
#' Plots a list of available colorblind-friendly palettes.
#' Allows filtering by palette type, series, or name.
#' @param type Character. Selects palettes based on type: \code{"cat"} for
#'  categorical, \code{"div"} for diverging, or \code{"seq"} for sequential.
#'  Defaults to \code{"all"}.
#' @param series Character vector. Selects palettes based on their series name.
#'  Defaults to \code{NULL}.
#' @param name Character vector. Selects one or more specific palettes by name.
#'  Defaults to \code{NULL}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object showing the selected palettes.
#'
#' @details
#' Available palette series:
#'
#' \code{brewer}, \code{carto}, \code{cols4all},  \code{gmt}, \code{hcl},
#' \code{kovesi}, \code{matplotlib}, \code{met}, \code{meteo}, \code{misc},
#'  \code{ocean}, \code{parks}, \code{powerbi}, \code{scico}, \code{seaborn},
#' \code{stevens}, \code{tableau}, \code{tol}.
#'
#' @examples
#' # Display all available palettes
#' plot_cbpal()
#'
#' # Display specific palettes by name
#' plot_cbpal(name = c("viridis", "magma"))
#'
#' # Display a specific palette series
#' plot_cbpal(series = "seaborn")
#'
#' @export

plot_cbpal <- function(
  type = c("all", "cat", "seq", "div"),
  series = NULL,
  name = NULL
) {
  # ============================================================
  # ---- Set defaults ----
  # ============================================================
  pal_type <- match.arg(type)

  # ============================================================
  # ---- Get palette data ----
  # ============================================================
  data(cbpal_list) 
  
  cbpal_list <- cbpal_list|>
    tidyr::pivot_longer(
      cols = x1:x7,
      names_to = "pos",
      values_to = "color"
    ) |>

    dplyr::mutate(pos = readr::parse_number(pos))

  # ============================================================
  # ---- Prepare data ----
  # ============================================================
  # ---- Filter by type ----
  if (pal_type != "all") {
    cbpal_list<- cbpal_list |>
      dplyr::filter(type == pal_type)
  }

  # ---- Filter by series ----
  if (!is.null(series)) {
    cbpal_list<- cbpal_list |>
      dplyr::filter(series %in% {{ series }})
  }

  # ---- Filter by name ----
  if (!is.null(name)) {
    cbpal_list<- cbpal_list |>
      dplyr::filter(name %in% {{ name }})
  }
  # ============================================================
  # ---- Return ----
  # ============================================================
  cbpal_list|>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = pos,
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

