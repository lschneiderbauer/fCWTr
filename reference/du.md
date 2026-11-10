# Drop physical unit

Drop units from a dimensionful quantity and retrieve a dimensionless
number. The dimensionless number is defined by the ratio of `x` and one
unit `u(1, unit)`.

## Usage

``` r
du(x, unit = "1")
```

## Arguments

- x:

  A dimensionful numeric vector (S3 class "units"), e.g. generated with
  [`u()`](https://lschneiderbauer.github.io/fCWTr/reference/u.md) or
  subsequent calculations.

- unit:

  A character indicating the unit. See
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  and
  [`units::valid_udunits_prefixes()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for possible units.

## Value

A dimensionless number defined by the ratio of `x` and one unit
`u(1, unit)`.

## See also

[`units::drop_units()`](https://r-quantities.github.io/units/reference/drop_units.html)

## Examples

``` r
 x <- u(1, "km")

 # retrieve a dimensionless number
 # from x in units of "meter"
 du(x, "m")
#> [1] 1000

 # drop the unit of a dimensionaless "unit" quantity
 du(x / x)
#> [1] 1
```
