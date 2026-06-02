# Average Annual Percent Change (AAPC)

Estimates the Average Annual Percent Change (AAPC) and its 95%
confidence interval. Optionally displays statistical significance using
significance stars.

## Usage

``` r
get_aapc(mod, digits = 1, show_ci = TRUE, dec = ".")
```

## Arguments

- mod:

  Joinpoint regression model (segmented object) or linear regression
  model (lm object).

- digits:

  Number of decimal places to display (integer).

- show_ci:

  Logical; if TRUE, displays the 95% confidence interval. If FALSE,
  displays significance stars.

- dec:

  Character. Decimal separator to use (e.g., "." or ",").

## Value

A character string with the AAPC and either its 95% confidence interval
or significance stars.

## Author

Tamara Ricardo

## Examples

``` r
# Load example data
data("plant", package = "segmented")

names(plant)
#> [1] "y"     "time"  "group"

# Fit the joinpoint models
mods <- model_jp(data = plant, value = "y", time = "time", group = "group", k = 2, test = TRUE)
#> No. of breakpoints: 2 .. 
#> Warning: The best BIC value at the boundary. Increase 'Kmax'?
#> 
#> BIC to detect no. of breakpoints:
#>          0          1          2 
#>  -10.65677  -86.81612 -107.18311 
#> 
#> No. of selected breakpoints: 2  
#> No. of breakpoints: 2 .. 
#> Warning: The best BIC value at the boundary. Increase 'Kmax'?
#> 
#> BIC to detect no. of breakpoints:
#>         0         1         2 
#>  30.10898 -57.99211 -73.23987 
#> 
#> No. of selected breakpoints: 2  
#> No. of breakpoints: 2 .. 
#> Warning: The best BIC value at the boundary. Increase 'Kmax'?
#> 
#> BIC to detect no. of breakpoints:
#>          0          1          2 
#>   1.713582 -66.784879 -73.378431 
#> 
#> No. of selected breakpoints: 2  

# AAPC of the first model
get_aapc(mods$RKW, digits = 1, show_ci = TRUE, dec = ".")
#> [1] "0.1% (0.1%; 0.1%)"
```
