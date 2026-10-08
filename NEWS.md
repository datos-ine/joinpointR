# joinpointR 1.0.0

* JoinpointR 2.0 is here!!!
## Improvements

* `joinpointR` is now independent from `segmented`!
* `model_jp()`, also known as `model_jp_grid()`, was reformulated to fit segmented linear regression models and select the best fit model using the grid-search method and the Bayesian Information Criterion (BIC).
* `get_summary()`, `get_apc()`, and `get_aapc()` now allow presenting results as `flextable` objects.
* `gg_jpoint()` was optimized, adding new geometries and coloring schemes, as well as greater flexibility to modify default settings.

## New features

* `bic_jp()`: Displays the BIC, penalized BIC (BIC3), and weighted BIC (WBIC) of a model or a list of models fitted with `model_jp()`.
* `plot_cbpal()`: Displays the available colorblind-friendly palettes.
* `scale_cbpal()`, `scale_cbpal_color()`, `scale_cbpal_colour()`, and `scale_cbpal_fill()`: Allows changing the default color scheme to a predefined colorblind-friendly palette.
