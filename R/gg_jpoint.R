#' Plot Joinpoint Regression Models
#'
#' Creates a ggplot showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#'
#' @param mods List of joinpoint regression models (output of model_jp()).
#' @param obs Displays observed data points (logical).
#' @param jp Displays joinpoints as vertical dashed lines (logical).
#' @param facets Whether to show the plots stacked ("none"),
#' facetted by group ("wrap") or by group and subgroup ("grid").
#' @param psize Desired size for the data points (numeric).
#' @param ptr Transparency of the data points (numeric).
#' @param cb Whether to use a colorblind-friendly palette (logical, defaults to TRUE)
#' @param cbpal Name of a colorblind-friendly palette (character, defaults to "viridis"). See details.
#' @return
#' A ggplot object showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#' @details
#' Available palettes are exported from cols4all:
#'
#' Diverging:
#' \itemize{
#'   \item "managua"
#'   \item "plasma"
#'   \item "roma"
#'   \item "vanimo"
#'   \item "viridis"
#' }
#'
#' Sequential:
#' \itemize{
#'   \item "algae"
#'   \item "arches2"
#'   \item "glasgow"
#'   \item "tokyo"
#'   \item "blue_fluoride"
#' }
#'
#' @export
#' @examples
#' # Load example data
#' data("hiv_data")
#'
#' names(hiv_data)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = hiv_data, value = "hiv_rate", time = "year", group = c("region", "sex"), k = 2, test = TRUE)
#'
#' # Plot results
#' gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = "wrap")
#'
#' # Facets by group and subgroup
#' gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = "grid")
#'
#' # No facets and hidden joinpoints
#' gg_jpoint(mods, jp = FALSE, facets = "none")
#'
#' # Change the color palette
#' gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = "grid", cb = TRUE, cbpal = "managua")

gg_jpoint <- function(
  mods,
  obs = TRUE,
  psize = 2.5,
  ptr = 0.75,
  jp = TRUE,
  facets = c("wrap", "grid", "none"),
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
  cbpal <- if (cb) {
    match.arg(cbpal)
  }

  facets <- match.arg(facets)

  # ---- Generate data ----
  data <- purrr::map_df(
    mods,
    ~ tibble::tibble(
      time = .x$model$.jp_time,
      obs = .x$model$.jp_log_value,
      fit = stats::predict(.x)
    ),
    .id = "group"
  ) |>

    # Create subgroups
    tidyr::separate_wider_delim(
      cols = group,
      names = c("group1", "group2"),
      delim = "_",
      cols_remove = FALSE,
      too_few = "align_start"
    ) |>

    # Format group label
    dplyr::mutate(group = stringr::str_replace(group, "_", ": "))

  # ---- Joinpoints ----
  jp_df <- purrr::imap_dfr(
    mods,
    \(mod, grp) {
      if (inherits(mod, "segmented") && !is.null(mod$psi)) {
        tibble::tibble(
          group = grp,
          jp = mod$psi[, "Est."]
        )
      } else {
        tibble::tibble(
          group = character(),
          jp = numeric()
        )
      }
    }
  ) |>

    # Create subgroups
    tidyr::separate_wider_delim(
      cols = group,
      names = c("group1", "group2"),
      delim = "_",
      cols_remove = FALSE,
      too_few = "align_start"
    ) |>

    # Format group label
    dplyr::mutate(group = stringr::str_replace(group, "_", ": "))

  # ---- Base plot layout ----
  g <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = time, y = obs, color = group1)
  ) +
    ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = 1) +

    ggplot2::labs(y = "log(rate)", x = NULL, color = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  ## ---- Add observed points ----
  if (obs) {
    g <- g +
      ggplot2::geom_point(size = psize, alpha = ptr)
  }

  ## ---- Add joinpoints ----
  if (jp && nrow(jp_df) > 0) {
    g <- g +
      ggplot2::geom_vline(
        data = jp_df,
        ggplot2::aes(xintercept = jp),
        color = "darkgrey",
        lwd = 2,
        alpha = 0.5
      )
  }

  ## ---- Add facets ----
  if (facets == "grid") {
    g <- g +
      ggplot2::facet_grid(group1 ~ group2)
  } else if (facets == "wrap") {
    g <- g +
      ggplot2::facet_wrap(
        ~group
      )
  }

  # ---- Show the final plot ----
  if (cb) {
    g +
      ggplot2::scale_color_manual(
        values = cols4all::c4a(
          palette = cbpal,
          n = dplyr::n_distinct(data$group1)
        )
      )
  } else {
    g
  }
}
