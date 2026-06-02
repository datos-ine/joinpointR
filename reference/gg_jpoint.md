# Plot Joinpoint Regression Models

Creates a ggplot showing observed values, fitted joinpoint regression
lines, and optional joinpoints.

## Usage

``` r
gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = FALSE)
```

## Arguments

- mods:

  List of joinpoint regression models (output of model_jp()).

- obs:

  Logical. If TRUE, displays observed data points.

- jp:

  Logical. If TRUE, displays joinpoints as vertical dashed lines.

- facets:

  Logical. If TRUE, displays one panel per group using facets.

## Value

A ggplot object.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
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

# Plot results
gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = FALSE)


# Facets by group
gg_jpoint(mods, obs = TRUE, jp = TRUE, facets = TRUE)
```
