# Summary Table For Joinpoint Regression Models

Generates a table displaying the number of joinpoints, time breaks, APC
and its 95% confidence interval, and AAPC and its statistical
significance from a list of joinpoint models.

## Usage

``` r
summary_jp(
  mods,
  digits = 1,
  var1 = "group",
  var2 = "subgroup",
  ft = FALSE,
  lan = c("en", "es")
)
```

## Arguments

- mods:

  List of joinpoint regression models (model_jp() output).

- digits:

  Number of decimal places to display (integer).

- var1:

  Character. Name of the grouping variable.

- var2:

  Character. Name of the subgrouping variable (optional).

- ft:

  Logical. If TRUE returns a flextable object, if FALSE returns a
  tibble.

- lan:

  Language of output: "en" (English) or "es" (Spanish).

## Value

A tibble or a flextable object.

## Author

Tamara Ricardo

## Examples

``` r
library(dplyr)
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

summary_jp(mods, digits = 1, var1 = "group", ft = FALSE, lan = "en")
#> # A tibble: 9 × 6
#>   group    JP Period    APC CI         AAPC  
#>   <chr> <dbl> <chr>   <dbl> <chr>      <chr> 
#> 1 RKW       2 142-280   0.5 0.4; 0.5   0.1% *
#> 2 RKW      NA 280-441   0.1 0.1; 0.2   NA    
#> 3 RKW      NA 441-710  -0.1 -0.1; 0    NA    
#> 4 RWC       2 142-315   0.9 0.8; 1     0.3% *
#> 5 RWC      NA 315-571   0.2 0.1; 0.2   NA    
#> 6 RWC      NA 571-750   0   -0.1; 0.1  NA    
#> 7 RKV       2 142-321   0.3 0.2; 0.4   0.0% *
#> 8 RKV      NA 321-532  -0.1 -0.2; -0.1 NA    
#> 9 RKV      NA 532-710  -0.3 -0.3; -0.2 NA    
```
