#' Colorblind-friendly scales
#'
#' @description
#' Scales for applying colorblind-friendly palettes to ggplot2 aesthetics.
#'
#' @param palette Character. String specifying the palette name. Defaults to
#' \code{"viridis"}.
#'
#' @param reverse Logical. If \code{TRUE}, reverses the order of colors in the
#' palette. Defaults to \code{FALSE}.
#'
#' @param discrete Logical. Should the scale be discrete?
#'
#' @param aesthetics Character vector of aesthetics to apply the scale to.
#'
#' @param ... Additional arguments passed to \code{ggplot2} scale constructor.
#'
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
#' @aliases scale_cbpal scale_cbpal_fill scale_cbpal_color scale_cbpal_colour
#' @export
#'
scale_cbpal <- function(
  palette = NULL,
  reverse = FALSE,
  discrete = TRUE,
  aesthetics = c("fill", "color"),
  ...
) {
  # ---- Default settings ----
  aesthetics <- match.arg(
    aesthetics,
    choices = c("fill", "color", "colour"),
    several.ok = TRUE
  )

  # ---- Aditional arguments ----
  dots <- rlang::list2(...)

  # ---- Create palettes ----
  cbpal <- function(
    palette = NULL,
    reverse = FALSE
  ) {
    # --- Default  palette ---
    palette <- if (!is.null(palette)) {
      match.arg(palette, choices = dplyr::pull(cbpal_list, .data$name))
    } else {
      "viridis"
    }

    # --- Filter data ---
    pal <- cbpal_list |>
      dplyr::filter(.data$name == palette) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("x"),
        names_to = "pos",
        values_to = "color"
      ) |>
      dplyr::pull(.data$color)

    # --- Reverse colors ---
    if (reverse) {
      pal <- rev(pal)
    }

    # --- Return ---
    return(grDevices::colorRampPalette(colors = pal))
  }

  # ---- Create palette ----
  pal <- cbpal(palette = palette, reverse = reverse)

  # ---- Pick between discrete and continuous scale ----
  if (discrete) {
    rlang::exec(
      ggplot2::discrete_scale,
      aesthetics = aesthetics,
      scale_name = "cbpal",
      palette = pal,
      !!!dots
    )
  } else {
    rlang::exec(
      ggplot2::scale_fill_gradientn,
      aesthetics = aesthetics,
      colours = pal(256),
      !!!dots
    )
  }
}

#' @rdname scale_cbpal
#' @export
scale_cbpal_fill <- function(aesthetics = "fill", ...) {
  scale_cbpal(aesthetics = aesthetics, ...)
}

#' @rdname scale_cbpal
#' @export
scale_cbpal_colour <- function(aesthetics = "color", ...) {
  scale_cbpal(aesthetics = aesthetics, ...)
}

#' @rdname scale_cbpal
#' @export
scale_cbpal_color <- scale_cbpal_colour
