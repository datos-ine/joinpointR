#' Plot Joinpoint Regression Models
#'
#' Creates a ggplot showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#'
#' @param mods A list of models returned by \code{model_jp_grid()} or
#' \code{model_jp_step()}.
#' @param geom Character. Determines the geom displayer: `"line"` for regression line,
#' `"linepoint"` for regression line and data points.
#' @param jp Character. Whether to display joinpoint(s) position as a vertical line (`"line"`),
#' a shaded area (`"area"`) or hide them (`"hide"`).
#' @param facets Character. Determines the facet layout: `"wrap"` for faceting by group,
#'  `"grid"` for faceting by group and subgroup, or `"grid2"` for faceting by subgroup
#' and group. When grouping variables are absent shows a single panel plot.
#' @param facet_cols Numeric. Number of columns to display when `facets = "wrap"`.
#' @param psize Numeric. Size of the observed data points. Defaults to 2.5 points.
#' @param lwd Numeric. Size of the regression line. Defaults to 1 point.
#' @param alpha Numeric. Controls the transparency of the data points and joinpoint lines.
#' @param cbpal Character. Name of the colorblind-friendly palette to use. Defaults to `"viridis"`.
#' @param cbpal_n Numeric. Maximum number of colors to display in the palette.
#' Defaults to 5.
#'
#' @return
#' A `ggplot` object showing observed values, fitted joinpoint regression
#' lines, and optional joinpoints.
#'
#' @details
#' Available colorblind-friendly palettes can be checked using `plot_cbpal()`.
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Create reduced dataset
#' data <- hiv_data |>
#'   dplyr::filter(dplyr::between(admin, "ARG", "Chubut")) |>
#'   droplevels()
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(
#'   data = data ,
#'   rate = hiv_rate,
#'   time = year,
#'   group = c("admin", "sex"),
#'   k = 2
#' )
#'
#' # Plot results with default settings
#' gg_jpoint(mods = mods, geom = "line", jp = "line", facets = "wrap")
#'
#' # Change the geom and facetting style
#' gg_jpoint(mods = mods, geom = "linepoint", jp = "line", facets = "grid")
#'
#' # Change the joinpoint and facetting style
#' gg_jpoint(mods = mods, geom = "linepoint", jp = "area", facets = "grid2")
#'
#' # Use a different color palette
#' gg_jpoint(mods = mods, cbpal = "managua")
#'
#' @export

gg_jpoint <- function(
  mods,
  geom = c("line", "linepoint"),
  jp = c("line", "area", "hide"),
  facets = c("wrap", "grid", "grid2"),
  facet_cols = 4,
  psize = 2.5,
  lwd = 1,
  alpha = 0.75,
  cbpal = "viridis",
  cbpal_n = 7
) {
  # ---- Default values ----
  ## ---- Geoms ----
  geom <- match.arg(geom)

  ## --- Joinpoint settings ----
  jp <- match.arg(jp)

  ## ---- Facets ----
  facets <- match.arg(facets)

  ## ---- Number of models ----
  n_mods <- length(mods)

  # ---- Validate facets ----
  if (n_mods > 1) {
    facets <- match.arg(facets)
  } else {
    if (facets != "none") {
      message(
        "Argument 'facets' was ignored because only one model was supplied."
      )
    }

    facets <- "none"
  }

  # ---- Validate facet columns ----
  if (n_mods > 1 && facets != "wrap") {
    message(
      "Argument 'facet_cols' is ignored when facets = 'grid' or 'grid2'."
    )
  }

  # ---- Validate hide joinpoints ----
  if (jp == "hide") {
    message("Joinpoint(s) position(s) will not be displayed.")
  }

  # ---- Validate palette colors ----
  if (cbpal_n > 13) {
    warning(
      "The number of colors selected exceeds the maximum number of colors allowed. Number of colors will be set to 13."
    )
  }

  # ============================================================
  # ---- Generate data ----
  # ============================================================
  # --- Generate grouping variables ---
  groups <- function(x) {
    x |>
      dplyr::mutate(
        group = if (n_mods > 1) {
          stringr::str_remove(group_var, "_.*")
        } else {
          NA_character_
        },
        subgroup = if (n_mods > 1) {
          stringr::str_remove(group_var, ".*_")
        } else {
          NA_character
        },
        group_var = if (n_mods > 1) stringr::str_replace(group_var, "_", ": ")
      )
  }

  ## ---- Fitted values ----
  data <- purrr::map_df(
    mods,
    function(x, id) {
      tibble::tibble(
        time = x$time,
        log_rate = x$log_rate,
        fit = stats::fitted(x$model)
      )
    },
    .id = "group_var"
  ) |>

    # --- Grouping variables ---
    groups()

  ## ---- Joinpoint positions ----
  data_jp <- purrr::map_df(
    mods,
    function(x, id) {
      tibble::tibble(
        jp = x$joinpoints
      )
    },
    .id = "group_var"
  ) |>

    # --- Grouping variables ---
    groups()

  # ============================================================
  # ---- Generate palettes ----
  # ============================================================
  cbpal_list <- get_cbpal()$name

  # ---- Validate palette name ----
  if (!cbpal %in% cbpal_list) {
    stop(
      sprintf(
        "Palette '%s' is not available among the colorblind-friendly palettes.",
        cbpal
      ),
      call. = FALSE
    )
  }

  ## ---- Select colorblind-friendly palettes ----
  cbpal <- match.arg(cbpal, choices = cbpal_list)

  # =============================================================
  # ---- Base plot layout -----
  #  ============================================================
  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = time, y = log_rate)
  ) +

    # Labels
    ggplot2::labs(
      x = NULL,
      y = "log(rate)",
      color = NULL,
      fill = NULL
    ) +

    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90)
    )

  # =============================================================
  # ---- Add facets -----
  #  ============================================================
  if (n_mods > 1) {
    g <- switch(
      facets,
      grid = g + ggplot2::facet_grid(group ~ subgroup),
      grid2 = g + ggplot2::facet_grid(subgroup ~ group),
      wrap = g + ggplot2::facet_wrap(~group_var, ncol = facet_cols),
      g
    )
  }

  # =============================================================
  # ---- Add joinpoints -----
  #  ============================================================
  if (jp == "line" && nrow(data_jp) > 0) {
    g <- g +
      ggplot2::geom_vline(
        data = data_jp,
        mapping = ggplot2::aes(xintercept = jp),
        color = "darkgrey",
        lwd = 1,
        alpha = alpha
      )
  } else if (jp == "area" && nrow(data_jp) > 0) {
    # ---- Create dataset ----
    data_tp <- purrr::map_dfr(
      seq_along(mods),
      function(i) {
        mod <- mods[[i]]

        br <- c(
          min(mod$time, na.rm = TRUE),
          mod$joinpoints,
          max(mod$time, na.rm = TRUE)
        )

        if (length(br) < 2) {
          return(tibble::tibble())
        } else {
          tibble::tibble(
            group_var = names(mods)[i] %||% as.character(i),
            xmin = br[-length(br)],
            xmax = br[-1],
            period = paste0("Period ", seq_len(length(br) - 1))
          )
        }
      }
    ) |>
      groups()

    # ---- Add polygons to the plot ----
    if (nrow(data_tp) > 0) {
      g <- g +
        ggplot2::geom_rect(
          data = data_tp,
          mapping = ggplot2::aes(
            xmin = xmin,
            xmax = xmax,
            ymin = -Inf,
            ymax = Inf,
            fill = period
          ),
          alpha = 0.25,
          inherit.aes = FALSE
        ) +

        ggplot2::geom_vline(
          data = data_jp,
          mapping = ggplot2::aes(xintercept = jp),
          lty = "dashed",
          alpha = alpha
        ) +

        ggplot2::scale_fill_manual(
          values = cols4all::c4a(
            palette = cbpal,
            n = dplyr::n_distinct(data_tp$period)
          )
        )
    }
  }

  # =============================================================
  # ---- Add geometries -----
  #  ============================================================
  if (geom == "line") {
    if (jp != "area") {
      g <- g +
        ggplot2::geom_line(
          mapping = ggplot2::aes(
            y = fit,
            color = if (facets == "grid2") subgroup else group
          ),
          lwd = lwd
        )
    } else {
      g <- g +
        ggplot2::geom_line(
          mapping = ggplot2::aes(
            y = fit,
          ),
          lwd = lwd
        )
    }
  } else if (geom == "linepoint") {
    if (jp != "area") {
      g <- g +
        ggplot2::geom_line(
          mapping = ggplot2::aes(
            y = fit,
            color = if (facets == "grid2") subgroup else group
          ),
          lwd = lwd
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            color = if (facets == "grid2") subgroup else group
          ),
          size = psize,
          alpha = alpha
        )
    } else {
      g <- g +
        ggplot2::geom_line(
          mapping = ggplot2::aes(y = fit, lwd = lwd)
        ) +
        ggplot2::geom_point(
          size = psize,
          alpha = alpha
        )
    }
  }

  # =============================================================
  # ---- Show plot -----
  #  ============================================================
  if (facets != "none") {
    g +
      ggplot2::scale_color_manual(
        values = cols4all::c4a(
          palette = cbpal,
          n = if (cbpal_n <= 13) cbpal_n else 13
        )
      )
  } else {
    g
  }
}
