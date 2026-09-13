#' Plot Joinpoint Regression Models
#'
#' Creates a ggplot showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#'
#' @param mods A list of joinpoint regression models returned by \code{model_jp()}.
#' @param obs Logical. If `TRUE`, observed data points are displayed.
#' @param jp Logical. If `TRUE`, joinpoints are displayed as vertical dashed lines.
#' @param facets Character. Determines the facet layout: `"wrap"` for faceting by group,
#'  `"grid"` for faceting by group and subgroup, or `"grid2"` for faceting by subgroup
#' and group. When grouping variables are absent shows a single panel plot.
#' @param ncol Numeric. Number of columns to display when `facets = "wrap"`.
#' @param psize Numeric. Size of the observed data points.
#' @param ptr Numeric. Transparency level of the observed data points (0-1).
#' @param cb Logical. If `TRUE`, a colorblind-friendly palette is used.
#' @param cbpal Character. Name of the colorblind-friendly palette to use.
#'   See Details.
#'
#' @return
#' A `ggplot` object showing observed values, fitted joinpoint regression
#' lines, and optional joinpoints.
#'
#' @details
#' Available colorblind-friendly palettes from the `cols4all` package include:
#'
#' Diverging palettes:
#' \itemize{
#'   \item `"managua"`
#'   \item `"plasma"`
#'   \item `"roma"`
#'   \item `"vanimo"`
#'   \item `"viridis"`
#' }
#'
#' Sequential palettes:
#' \itemize{
#'   \item `"algae"`
#'   \item `"arches2"`
#'   \item `"blue_fluoride"`
#'   \item `"glasgow"`
#'   \item `"tokyo"`
#' }
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Filter data
#' hiv_data <- hiv_data |>
#' dplyr::filter(admin %in% c("CABA", "Catamarca", "Chaco", "Chubut"))
#'
#' # Fit the joinpoint models
#' mod <- model_jp(
#'   data = hiv_data ,
#'   value = hiv_rate,
#'   time = year,
#'   group = c("admin", "sex"),
#'   k = 2
#' )
#'
#' # Plot results
#' gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = "wrap")
#'
#' # Facet by group and subgroup
#' gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = "grid")
#'
#' # Facet by subgroup and group
#' gg_jpoint(mods, jp = FALSE, facets = "grid2")
#'
#' # Use a different colorblind-friendly palette
#' gg_jpoint(
#'   mods,
#'   obs = TRUE,
#'   jp = TRUE,
#'   facets = "grid",
#'   cb = TRUE,
#'   cbpal = "managua"
#' )
#'
#' # Use default ggplot2 palette (can be changed using `scale_color_`)
#' gg_jpoint(
#' mods,
#' cb = FALSE
#' )
#' @export

gg_jpoint <- function(
  mods,
  obs = TRUE,
  psize = 2.5,
  ptr = 0.75,
  jp = TRUE,
  facets = c("wrap", "grid", "grid2"),
  ncol = 4,
  cb = TRUE,
  cbpal = c(
    # Sequential
    "viridis",
    "managua",
    "plasma",
    "roma",
    "vanimo",
    # Diverging
    "algae",
    "arches2",
    "glasgow",
    "tokyo",
    "blue_fluoride"
  )
) {
  # ---- Default values ----
  if (cb) {
    cbpal <- match.arg(cbpal)
  }

  if (cb) {
  cbcol <- cols4all::c4a(
    palette = cbpal,
    n = 1
  )[1]
}
  
  n_mods <- length(mods)

  # ---- Validate facets ----
  if (n_mods > 1) {
    facets <- match.arg(facets)
  } else {
    facets <- "none"
    message("Facets were ignored because there were no grouping variable(s).")
  }

  # ---- Validate facet columns ----
  if (n_mods > 1 && facets != "wrap") {
    message(
      "Number of columns will be ignored when facets 'grid' or 'grid2'."
    )
  }

  # ---- Validate hide data points ----
  if (!obs) {
    message("Data points will not be displayed.")
  }

  # ---- Validate hide joinpoints ----
  if (!jp) {
    message("Joinpoint(s) position(s) will not be displayed.")
  }

  # ---- Validate disable colorblind-friendly palette ----
  if (!cb) {
    message("Colorblind-friendly palettes disabled.")
  }

  # ---- Generate subgroups ----
  get_groups <- function(.x) {
    .x |>
      tidyr::separate_wider_delim(
        cols = group_var,
        names = c("group", "subgroup"),
        delim = "_",
        cols_remove = FALSE,
        too_few = "align_start"
      ) |>
      dplyr::mutate(
        group_var = stringr::str_replace(
          group_var,
          "_",
          ": "
        )
      )
  }

  # ---- Generate dataset for base plot ----
  data <- purrr::map_dfr(
    mods,
    \(x) {
      mod <- x$model

      tibble::tibble(
        time = mod$model$time,
        obs = mod$model$y,
        fit = mod$fitted.values
      )
    },
    .id = if (n_mods > 1) "group_var" else NULL
  )

  if (n_mods > 1) {
    data <- data |> get_groups()
  }

  # ---- Generate dataset for joinpoint positions ----
  jp_data <- if (n_mods > 1) {
    purrr::imap_dfr(
      mods,
      \(mod, group) {
        if (length(mod$joinpoints) > 0) {
          tibble::tibble(
            group_var = group,
            jp = mod$joinpoints
          )
        } else {
          tibble::tibble(
            group_var = character(),
            jp = numeric()
          )
        }
      }
    ) |>
      get_groups()
  } else {
    if (length(mods[[1]]$joinpoints) > 0) {
      tibble::tibble(
        jp = mods[[1]]$joinpoints
      )
    } else {
      tibble::tibble(
        jp = numeric()
      )
    }
  }

  # ---- Generate base plot layout ----
  if (facets != "none") {
    g <- data |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = time,
          y = obs,
          color = if (facets != "grid2") {
            group
          } else {
            subgroup
          }
        )
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = fit),
        linewidth = 1
      ) +
      ggplot2::labs(
        y = "log(rate)",
        x = NULL,
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle = 90)
      )
  } else {
    g <- data |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = time,
          y = obs
        )
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = fit),
        linewidth = 1,
        color = if (cb) cbcol else NULL
      ) +
      ggplot2::labs(
        y = "log(rate)",
        x = NULL,
        color = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom"
      )
  }

  # ---- Add facets ----
  g <- switch(
    facets,

    grid = g + ggplot2::facet_grid(group ~ subgroup),

    grid2 = g + ggplot2::facet_grid(subgroup ~ group),

    wrap = g +
      ggplot2::facet_wrap(
        ~group_var,
        ncol = ncol
      ),

    none = g
  )

  # ---- Add data points ----
  if (obs) {
    g <- g +
      ggplot2::geom_point(
        size = psize,
        alpha = ptr,
        color = if (facets == "none" && cb) cbcol else NULL
      )
  } 

  # ---- Add joinpoints ----
  if (jp && nrow(jp_data) > 0) {
    g <- g +
      ggplot2::geom_vline(
        data = jp_data,
        ggplot2::aes(xintercept = jp),
        color = "darkgrey",
        linewidth = 2,
        alpha = 0.4
      )
  }

  # ---- Show plot ----
  if (cb && facets != "none") {
  g +
    ggplot2::scale_color_manual(
      values = cols4all::c4a(
        palette = cbpal,
        n = dplyr::n_distinct(data$group)
      )
    )
} else {
  g
}
}
