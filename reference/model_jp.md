# Joinpoint Regression Models by Groups

Fits segmented linear regression models by groups for age-standardized
rates using joinpoint regression. Models can be fitted using either a
stepwise selection procedure based on the Bayesian Information Criterion
(BIC) or a fixed number of joinpoints. Internally calls
[`segmented::selgmented()`](https://rdrr.io/pkg/segmented/man/selgmented.html)
or
[`segmented::segmented()`](https://rdrr.io/pkg/segmented/man/segmented.html)
and applies a log transformation to the response variable.

## Usage

``` r
model_jp(data, value, time, group, k = 2, step = TRUE, test = TRUE)
```

## Arguments

- data:

  Data frame containing age-standardized rates.

- value:

  Name of the response variable (character).

- time:

  Name of the time variable (character).

- group:

  Name of one or more grouping variables (character vector).

- k:

  Maximum number of joinpoints to be estimated (integer).

- step:

  Use stepwise procedure to select the number of joinpoints (logical).

- test:

  whether to test for differences in slope t-values during the stepwise
  selection procedure. Only used when `step = TRUE`.

## Value

A list of models by group.

## Author

Tamara Ricardo

## Examples

``` r
# Generate example data
library(dplyr)
df <- mtcars |>
mutate(
  year = seq(2000, length.out = n(), by = 1),
  group = factor(paste("cyl", cyl, sep = "_")),
  rate = mpg
) |>
select(year, group, rate)

# Check group levels
levels(df$group)
#> [1] "cyl_4" "cyl_6" "cyl_8"

# Fit models
mods <- model_jp(data = df, value = "rate", time = "year", group = "group",
 k = 2, step = TRUE, test = TRUE)
#> No. of breakpoints: 2 .. 
#> 
#> BIC to detect no. of breakpoints:
#>         0         1         2 
#> -2.347654 -2.027073  2.417460 
#> 
#> No. of selected breakpoints:  0  
#> No. of breakpoints: 2 .. 
#> 
#> BIC to detect no. of breakpoints:
#>         0         1         2 
#> -12.44537 -15.72914 -14.72914 
#> 
#> No. of selected breakpoints: 0  (1 breakpoint(s) removed due to small slope diff)
#> No. of breakpoints: 2 .. 
#> 
#> BIC to detect no. of breakpoints:
#>         0         1         2 
#> -1.227750 -0.614267  3.606649 
#> 
#> No. of selected breakpoints:  0  

# Show the output of the first model
mods$cyl_6
#> 
#> Call:
#> stats::lm(formula = formula, data = .x)
#> 
#> Coefficients:
#> (Intercept)         year  
#>   -3.820829     0.003514  
#> 
summary(mods$cyl_6)
#> 
#> Call:
#> stats::lm(formula = formula, data = .x)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.25210 -0.09738 -0.03658  0.12882  0.25009 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)
#> (Intercept) -3.820829  12.032563  -0.318    0.758
#> year         0.003514   0.005962   0.589    0.570
#> 
#> Residual standard error: 0.1734 on 9 degrees of freedom
#> Multiple R-squared:  0.03716,    Adjusted R-squared:  -0.06982 
#> F-statistic: 0.3473 on 1 and 9 DF,  p-value: 0.5701
#> 
```
