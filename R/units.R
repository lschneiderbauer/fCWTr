#' Augment physical unit
#'
#' A wrapper around [units::set_units()] mostly to have a short name to
#' create a dimensionful quantity.
#'
#' @param x A numeric vector to be augmented with a unit.
#' @param unit
#'  A character indicating the unit. See [units::valid_udunits()] and
#'  [units::valid_udunits_prefixes()] for possible units.
#'
#' @seealso [units::set_units()]
#' @examples
#' u(10, "km") # 10 kilometer
#' u(10, "mm") # 10 mm
#'
#' # comparison taking into account the unit
#' u(100, "m") > u(1, "km")
#'
#' @export
u <- function(x, unit = "1") {
  stopifnot(is.numeric(x))
  stopifnot(is.character(unit))
  # x must be a pure number
  stopifnot("Argument must a unitless number" = !inherits(x, "units"))

  # mode required because we pass a string instead of an expression
  units::set_units(x, unit, mode = "standard")
}

#' Drop physical unit
#'
#' Drop units from a dimensionful quantity and retrieve a dimensionless number.
#' The dimensionless number is defined by the ratio of `x` and one unit `u(1, unit)`.
#'
#' @param x
#'  A dimensionful numeric vector (S3 class "units"), e.g. generated with [u()] or
#'  subsequent calculations.
#' @inheritParams u
#' @return
#'  A dimensionless number defined by the ratio of `x` and one unit `u(1, unit)`.
#'
#' @examples
#'  x <- u(1, "km")
#'
#'  # retrieve a dimensionless number
#'  # from x in units of "meter"
#'  du(x, "m")
#'
#'  # drop the unit of a dimensionaless "unit" quantity
#'  du(x / x)
#'
#' @seealso [units::drop_units()]
#' @export
du <- function(x, unit = "1") {
  stopifnot(inherits(x, "units"))

  if (has_comp_unit(x, unit)) {
    units::drop_units(conv(x, unit))
  } else {
    stop("Units are not convertible.")
  }
}

#' Drop unit if dimensionless
#' @noRd
ddu <- function(x) {
  if (has_comp_unit(x, "1")) {
    units::drop_units(x)
  } else {
    x
  }
}

#' Does x have a unit compatible with `unit`?
#' @noRd
has_comp_unit <- function(x, unit) {
  stopifnot(
    "The first argument needs to of s3 class `unit`" = inherits(x, "units")
  )

  units::ud_are_convertible(units(x), units(u(1, unit)))
}

as_freq <- function(x) {
  stopifnot(is.numeric(x))

  if (inherits(x, "units")) {
    stopifnot(has_comp_unit(x, "Hz") )

    x
  } else {
    u(x, "Hz")
  }
}

as_sec <- function(x) {
  stopifnot(is.numeric(x))

  if (inherits(x, "units")) {
    stopifnot(has_comp_unit(x, "s") )

    x
  } else {
    u(x, "s")
  }
}

conv <- function(x, unit) {
  stopifnot(has_comp_unit(x, unit))

  units(x) <- unit
  x
}

#' returns a pure number from a frequency value
#' in Hz.
#' @noRd
hz <- function(x) {
  units::drop_units(conv(x, "Hz"))
}

sec <- function(x) {
  units::drop_units(conv(x, "s"))
}
