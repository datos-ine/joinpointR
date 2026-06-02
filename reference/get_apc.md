# Annual Percent Change by Segment (APC)

Calculates the Annual Percent Change (APC) for each segment and its 95%
confidence interval.

## Usage

``` r
get_apc(mod, digits = 1, time = "time", dec = ".")
```

## Arguments

- mod:

  Joinpoint regression model (segmented object).

- digits:

  Number of decimal places to display (integer).

- time:

  Time variable used in the model (character).

- dec:

  Character. Decimal separator to use (e.g., "." or ",").

## Value

A character vector with APC and 95% CI for each segment.

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

# Obtain APC (95% CI)
get_apc(mods$RKW, digits = 1, time = "time", dec = ".")
#>          APC          CI
#> slope1  0.5%  0.4%; 0.5%
#> slope2  0.1%  0.1%; 0.2%
#> slope3 -0.1% -0.1%; 0.0%
```
