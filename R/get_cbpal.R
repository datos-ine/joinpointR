#' Gets a list of colorblind-friendly palettes
#' @param colors Character. Allows to select the palettes based on color
#' brightness: 'vivid' select palettes with bright colors, 'pastel' selects
#' palettes with pastel colors, 'fair' selects palettes with a balance between
#' bright and pastel colors. Defaults to 'all'.
#' @param n Integer. Specifies the number of colors to select. Ranges between 3
#'  and 13 colors. Defaults to 5.
#' @keywords  internal
#'
get_cbpal <- function(
  colors = c("all", "vivid", "fair", "pastel"),
  n = 5
) {
  # ============================================================
  # ---- Install dependencies ----
  # ============================================================
  if (!require("cols4all")) {
    install.packages("cols4all")
  }

  # ============================================================
  # ---- Set defaults ----
  # ============================================================
  pal_colors <- match.arg(colors)

  # ============================================================
  # ---- Retrieve available palettes ----
  # ============================================================
  pal_list <- purrr::map_df(
    c("cat", "seq", "div"),
    \(type) {
      # --- Get HTML table ---
      cols4all::c4a_table(
        type = type,
        filters = "cbf"
      ) |>
        # --- HTML to dataframe ---
        as.character() |>
        rvest::read_html() |>
        rvest::html_element("table") |>
        rvest::html_table() |>
        # --- Clean column names ---
        janitor::clean_names() |>
        # --- Exclude monochromatic palettes ---
        dplyr::filter_out(hues == "🖌") |>

        # --- Modify factor levels ---
        dplyr::mutate(
          dplyr::across(
            .cols = c(fair, vivid),
            .fns = ~ factor(.x, labels = c(rep("No", 2), "Yes"))
          )
        )
    },
    .id = "type"
  )

  # ----  Filter by color type ----
  if (pal_colors == "fair") {
    pal_list <- pal_list |>
      dplyr::filter(fair == "Yes")
  } else if (pal_colors == "vivid") {
    pal_list <- pal_list |>
      dplyr::filter(fair == "No" & vivid == "Yes")
  } else if (pal_colors == "pastel") {
    pal_list <- pal_list |>
      dplyr::filter(vivid == "No")
  } else {
    pal_list
  }

  # ---- Retrieve color names ----
  pal_list <- pal_list |>
    # --- Arrange by name ---
    dplyr::arrange(name) |>

    # --- Remove duplicates ---
    dplyr::distinct(name, .keep_all = TRUE) |>

    dplyr::pull(name)

  # ============================================================
  # ---- Generate palette data ----
  # ============================================================
  pal_data <- purrr::map(
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
  ) |>
    purrr::list_rbind()

  # ============================================================
  # ---- Return ----
  # ============================================================
  pal_data
}
