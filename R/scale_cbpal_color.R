#' Colorblind-friendly scales
#'
#' @description
#' Scales for applying colorblind-friendly palettes to ggplot2 aesthetics.
#'
#' @param palette Character string specifying the palette.
#'
#' @param reverse Logical; if `TRUE`, reverses the palette.
#'
#' @param discrete Logical; should the scale be discrete?
#'
#' @param ... Other arguments passed to the underlying ggplot2 scale.
#'
#' @return A \code{ggplot2} scale.
#'
#' @examples
#' library(ggplot2)
#' data(iris)
#'
#' # Use a discrete variable for color
#' iris |>
#' ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#' geom_point() +
#' scale_cbpal_color()
#'
#' # Use a continuous variable for color
#' iris |>
#' ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
#' geom_point() +
#' scale_cbpal_color(discrete = FALSE)
#'
#' # Use a discrete variable for fill
#' iris |>
#' ggplot(aes(x = Sepal.Length, fill = Species)) +
#' geom_bar() +
#' scale_cbpal_fill()
#'
#' @name scale_cbpal
#' @aliases scale_cbpal_fill scale_cbpal_color
#' @export
#'
scale_cbpal_color <- function(
  palette = "viridis",
  reverse = FALSE,
  discrete = TRUE,
  ...
) {
  # --- Call the colorblind-friendly palette ---
  pal <- cbpal(palette = palette, reverse = reverse)

  # --- Pick between discrete or continuous palette ---
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal(256),
      ...
    )
  }
}


#' @rdname scale_cbpal
#' @export
#' 
scale_cbpal_fill <- function(
  palette = "viridis",
  reverse = FALSE,
  discrete = TRUE,
  ...
) {
  # --- Call the colorblind-friendly palette ---
  pal <- cbpal(palette = palette, reverse = reverse)

  # --- Pick between discrete or continuous palette ---
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colours = pal(256),
      ...
    )
  }
}


#' Selects a colorblind-friendly palette
#' @keywords internal
#' 
cbpal <- function(
  palette = "viridis",
  reverse = FALSE
) {
  # --- Load list of available palettes ---
  data(cbpal_list)

  # --- Filter data ---
  pal <- cbpal_list |>
    dplyr::filter(name == palette) |>
    tidyr::pivot_longer(
      cols = x1:x7,
      names_to = "pos",
      values_to = "color"
    ) |>
    dplyr::pull(color)

  # --- Reverse colors ---
  if (reverse) {
    pal <- rev(pal)
  }

  # --- Return ---
  colorRampPalette(colors = pal)
}
