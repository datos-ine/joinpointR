# Changelog

## joinpointR 0.6.2

CRAN release: 2026-05-28

### Improvements

- Added the `step` argument to [`model_jp()`](../reference/model_jp.md).
- Added support for multiple grouping variables in
  [`model_jp()`](../reference/model_jp.md). Internally, the function
  creates a grouping variable based on their interaction.

### Bug fixes

- Fixed issues when merging grouping variables.
- Fixed output issues in [`get_apc()`](../reference/get_apc.md).

## joinpointR 0.5.0

CRAN release: 2026-05-02

### Improvements

- Added [`gg_jpoint()`](../reference/gg_jpoint.md).
- Improved language handling in
  [`summary_jp()`](../reference/summary_jp.md).
