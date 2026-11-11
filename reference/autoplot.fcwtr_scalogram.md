# Create a ggplot object resembling a scalogram

Create a ggplot object resembling a scalogram

## Usage

``` r
# S3 method for class 'fcwtr_scalogram'
autoplot(
  object,
  n = 1000,
  time_unit = NULL,
  freq_unit = "Hz",
  transf = identity,
  ...
)
```

## Arguments

- object:

  A "fcwtr_scalogram" object resulting from
  [`fcwt()`](https://lschneiderbauer.github.io/fCWTr/reference/fcwt.md).

- n:

  The plotting function reduces the time resolution by averaging to
  generate a reasonable graphics format. `n` is the number of time steps
  that are plotted. Defaults to `n = 1000`.

- time_unit:

  A time unit that is used for the x-axis scale, e.g. `"s"` or `"ms"`.
  By default (`NULL`), a suitable unit (s, ms, us, or ns) is chosen
  depending on the length of the scalogram. See
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  and
  [`units::valid_udunits_prefixes()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for valid expressions.

- freq_unit:

  A frequency unit that is used for the y-axis scale. Defaults to "Hz" -
  "Hertz". See
  [`units::valid_udunits()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  and
  [`units::valid_udunits_prefixes()`](https://r-quantities.github.io/units/reference/valid_udunits.html)
  for valid expressions.

- transf:

  A function, taking a vector and returning a vector, that is used to
  transform the scalogram values before plotting.

- ...:

  Additional arguments are ignored.

## Value

A ggplot object.

## Examples

``` r
library(ggplot2)

res <-
  fcwt(
    sin((1:4410) * 2 * pi * 440 / 44100),
    x_sample_freq = u(44100, "Hz"),
    freq_begin = u(50, "Hz"),
    freq_end = u(1000, "Hz"),
    n_freqs = 10,
    sigma = 5
  )

autoplot(res, time_unit = "ms")
```
