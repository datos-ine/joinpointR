#' Fit Joinpoint Regression Models Using Grid Search
#'
#' @description
#' Fits log-linear joinpoint regression models by grid search and selects
#' the best model according to the Bayesian Information Criterion (BIC).
#'
#' @param data A dataset containing the rates, time points, and, optionally,
#' grouping variables.
#' @param rate Name of the variable containing the rates.
#' @param time Name of the variable containing the time points.
#' @param group Character vector specifying the name(s) of the variable(s)
#' used to group the data. A maximum of two grouping variables is allowed.
#' Defaults to \code{NULL}.
#' @param jp Integer specifying the maximum number of joinpoints to test.
#' Must be between 0 and 7 (See details).
#' @param min.dist Integer specifying the minimum number of time points
#' required between consecutive joinpoints and between each endpoint of
#' the time series and the nearest joinpoint. Defaults to 2.
#'@param method Method used to select the best fit model. One of \code{"bic"},
#' \code{"bic3"}, \code{"wbic"}. Defaults to \code{"bic"} (see Details).
#'
#' @return A named list containing one element for each group. Each element contains:
#'  \itemize{
#' \item \code{fit}: An \code{lm} object corresponding to the selected
#' joinpoint model.
#' \item \code{joinpoints}: Numeric vector containing the estimated
#'  joinpoint positions of the selected model.
#' \item \code{time} Vector of time points used to fit the model.
#' \item \code{log_rate} Vector of log-transformed rates used as the
#'  response variable.
#' \item \code{BIC} BIC value of the selected model.
#' \item \code{model_sel}: A tibble containing the number and positions of
#'  joinpoints, SSE, and BIC for all candidate models evaluated.
#' }
#'
#' @details
#' The maximum recommended number of joinpoints is determined from the
#' number of time points in the series, following the criteria described
#' by Kim et al. (2000):
#'
#' \itemize{
#'   \item 0--6 time points: 0 joinpoints.
#'   \item 7--11 time points: 1 joinpoint.
#'   \item 12--16 time points: 2 joinpoints.
#'   \item 17--21 time points: 3 joinpoints.
#'   \item 22--26 time points: 4 joinpoints.
#'   \item 27--31 time points: 5 joinpoints.
#'   \item 32--36 time points: 6 joinpoints.
#'   \item 37+ time points: 7 joinpoints.
#' }
#'
#' The Bayesian Information Criterion (BIC) is calculated as:
#'
#' \deqn{
#' BIC = \log(MSE) + \frac{2(n_{jp}+1)\log(n)}{n}
#' }
#'
#' where \code{MSE} is the mean squared error of the fitted model,
#' \code{n_jp} is the number of joinpoints, and \code{n} is the number
#' of observations used to fit the model.
#'
#' When grouping variables are specified, all groups must contain the same
#' number of time points.
#'
#' @examples
#' # Load data
#' data(hiv_data)
#'
#' # Create a reduced dataset
#' data <- hiv_data |>
#' dplyr::filter(
#' dplyr::between(admin, "ARG", "Chubut"))
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year, group = c("admin", "sex"))
#'
#' # Filter dataset
#' data_arg <- hiv_data |>
#' dplyr::filter(admin == "ARG" & sex == "Female")
#'
#' mod1 <- model_jp(data = data_arg, rate = hiv_rate, time = year)
#'
#' mod1[[1]]$BIC
#'
#' @export

model_jp_grid <- function(
    data,
    rate,
    time,
    group = NULL,
    jp = 2,
    min.dist = 2,
    method = c("bic", "bic3", "wbic")
) {
    # ==========================================================
    # ---- Define variables ----
    # ==========================================================
    rate <- rlang::as_name(rlang::ensym(rate))

    time <- rlang::as_name(rlang::ensym(time))

    method <- match.arg(method)

    # ==========================================================
    # ---- Prepare data ----
    # ==========================================================
    jp_data <- clean_jp_data(
        data = data,
        rate = rate,
        time = time,
        group = group
    )

    data <- jp_data$group_data

    mod_names <- if (!is.null(group)) jp_data$group_levels else "Model"

    # ==========================================================
    # ---- Validations ----
    # ==========================================================
    # ---- Maximum number of joinpoints ----
    if (jp > 7) {
        stop(
            "This function allows to test a maximum of 7 joinpoints.",
            call. = FALSE
        )
    }

    # ---- Recommended number of joinpoints ----
    if (jp > jp_data$k) {
        warning(
            paste0(
                "The recommended number of joinpoints to test for your ",
                jp_data$time_points,
                " time points is ",
                jp_data$k,
                " (check Details)."
            ),
            call. = FALSE
        )
    }

    # ---- Method ---
    if (method == "bic3") {
        message("Model selection was performed based on the BIC3.")
    } else if (method == "wbic") {
        message("Model selection was performed based on the WBIC.")
    } else {
        message("Model selection was performed based on the BIC.")
    }

    # ==========================================================
    # ---- Fit models ----
    # ==========================================================
    fit_group <- function(data, group_name) {
        # --------------------------------------------------------
        # ---- Define x and y ----
        # --------------------------------------------------------
        x <- data$time
        y <- data$log_rate

        # --------------------------------------------------------
        # ---- Fit linear joinpoint model ----
        # --------------------------------------------------------
        fit_jp <- function(x, y, joinpoints) {
            # ---- Create model data ----
            model_data <- dplyr::bind_cols(
                tibble::tibble(x, y),
                purrr::map(
                    seq_along(joinpoints),
                    function(i) {
                        pmax(0, x - joinpoints[i])
                    }
                ) |>
                    rlang::set_names(
                        paste0("U.", seq_along(joinpoints))
                    )
            )

            # ---- Fit linear model ----
            model <- stats::lm(
                y ~ .,
                data = model_data
            )

            # ------------------------------------------------------
            # ---- Store data used by the model ----
            # ------------------------------------------------------
            model$.jp_time <- x
            model$.jp_log_rate <- y
            model$.jp_joinpoints <- joinpoints
            model$.jp_fitted_values <- stats::fitted(model)

            model
        }

        # --------------------------------------------------------
        # ---- Validate joinpoints ----
        # --------------------------------------------------------
        valid_jp <- function(jp_values) {
            if (length(jp_values) == 0) {
                return(TRUE)
            }

            idx <- match(
                jp_values,
                x
            )

            if (anyNA(idx)) {
                return(FALSE)
            }

            idx <- sort(idx)

            left <- idx[1] - 1

            middle <- if (length(idx) > 1) {
                diff(idx) - 1
            } else {
                numeric(0)
            }

            right <- length(x) - idx[length(idx)]

            all(
                left >= min.dist,
                middle >= min.dist,
                right >= min.dist
            )
        }

        # ========================================================
        # ---- Calculate Joinpoint BIC ----
        # ========================================================
        bic_jp <- function(mod, n_jp, r2_max = 0) {
            # --- Residuals and observations  ---
            mse <- mean(stats::residuals(mod)^2)
            n <- stats::nobs(mod)

            # --- Estimate BIC ---
            if (method == "bic3") {
                log(mse) + ((3 * n_jp + 2) / n) * log(n)
            } else if (method == "wbic") {
                n_parm_wbic <- (2 + r2_max) * n_jp + 2
                log(mse) + (n_parm_wbic / n) * log(n)
            } else {
                # Default: BIC
                log(mse) + ((2 * n_jp + 2) / n) * log(n)
            }
        }

        # ========================================================
        # ---- Model results ----
        # ========================================================
        results <- list()

        # ========================================================
        # ---- Model with 0 joinpoints ----
        # ========================================================
        joinpoints <- numeric(0)

        mod <- fit_jp(
            x = x,
            y = y,
            joinpoints = joinpoints
        )

        r2 <- summary(mod)$r.squared

        results[[length(results) + 1]] <- tibble::tibble(
            n_jp = 0,
            jp1 = NA_real_,
            jp2 = NA_real_,
            jp3 = NA_real_,
            jp4 = NA_real_,
            jp5 = NA_real_,
            jp6 = NA_real_,
            jp7 = NA_real_,
            SSE = sum(stats::residuals(mod)^2),
            BIC = bic_jp(mod, 0, r2)
        )

        # ========================================================
        # ---- Model with 1 to k joinpoints ----
        # ========================================================
        if (jp >= 1) {
            for (n_jp in seq_len(jp)) {
                # ----------------------------------------------------
                # Generate all combinations
                # ----------------------------------------------------
                jp_grid <- utils::combn(
                    sort(unique(x)),
                    n_jp,
                    simplify = FALSE
                ) |>
                    purrr::keep(
                        valid_jp
                    )

                # ----------------------------------------------------
                # Fit every candidate
                # ----------------------------------------------------
                if (length(jp_grid) > 0) {
                    for (jp_values in jp_grid) {
                        mod <- fit_jp(
                            x = x,
                            y = y,
                            joinpoints = jp_values
                        )

                        jp_values_out <- rep(
                            NA_real_,
                            7
                        )

                        jp_values_out[
                            seq_along(jp_values)
                        ] <- jp_values

                        results[[length(results) + 1]] <-
                            tibble::tibble(
                                n_jp = n_jp,
                                jp1 = jp_values_out[1],
                                jp2 = jp_values_out[2],
                                jp3 = jp_values_out[3],
                                jp4 = jp_values_out[4],
                                jp5 = jp_values_out[5],
                                jp6 = jp_values_out[6],
                                jp7 = jp_values_out[7],
                                SSE = sum(stats::residuals(mod)^2),
                                BIC = bic_jp(mod, n_jp, r2)
                            )
                    }
                }
            }
        }

        # ========================================================
        # ---- Full list of models ----
        # ========================================================
        results <- dplyr::bind_rows(results) |>
            dplyr::arrange(BIC)

        # ========================================================
        # ---- Select best model ----
        # ========================================================
        best <- results |>
            dplyr::slice_min(
                BIC,
                n = 1,
                with_ties = FALSE
            )

        # ========================================================
        # ---- Selected joinpoints ----
        # ========================================================
        joinpoints <- best |>
            dplyr::select(
                dplyr::starts_with("jp")
            ) |>
            unlist(
                use.names = FALSE
            ) |>
            stats::na.omit() |>
            as.numeric()

        # ========================================================
        # ---- Final model ----
        # ========================================================
        model <- fit_jp(
            x = x,
            y = y,
            joinpoints = joinpoints
        )

        # ========================================================
        # ---- Results of the best model ----
        # ========================================================
        list(
            fit = model,
            joinpoints = joinpoints,
            time = x,
            log_rate = y,
            BIC = best$BIC,
            model_sel = results
        )
    }

    # ==========================================================
    # ---- Fit models by group ----
    # ==========================================================
    mods <- purrr::imap(
        data,
        fit_group
    )

    mods <- mods |>
        purrr::set_names(nm = mod_names)

    # ==========================================================
    # ---- Message ----
    # ==========================================================
    purrr::iwalk(
        mods,
        ~ {
            jp <- .x$joinpoints

            message(paste0(
                "Model: ",
                .y,
                "| Joinpoints: ",
                if (length(jp) > 0) {
                    paste(jp, collapse = ", ")
                } else {
                    "None detected."
                }
            ))
        }
    )

    # ==========================================================
    # ---- Return ----
    # ==========================================================
    structure(mods, class = "model_jp")
}


# ---- Use model_jp ----
#' @export
model_jp <- function(
    data,
    ...
) {
    model_jp_grid(
        data,
        ...
    )
}
