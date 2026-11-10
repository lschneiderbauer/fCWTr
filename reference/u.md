# Augment physical unit

A wrapper around
[`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)
mostly to have a short name to create a dimensionful quantity.

## Usage

``` r
u(x, unit = "1")
```

## Arguments

- x:

  A numeric vector to be augmented with a unit.

- unit:

  A character indicating the unit. See
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  and
  [`units::valid_udunits_prefixes()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for possible units.

## See also

[`units::set_units()`](https://r-quantities.github.io/units/reference/units.html)

## Examples

``` r
u(10, "km") # 10 kilometer
#> 10 [km]
u(10, "mm") # 10 mm
#> 10 [mm]

# comparison taking into account the unit
u(100, "m") > u(1, "km")
#> [1] FALSE
```
