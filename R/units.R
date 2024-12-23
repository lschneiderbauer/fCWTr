#' Bestow units to a number
#'
#' A wrapper around `units::set_unit()` mostly to have a short name to
#' create a dimensionful number.
#'
#' @param x A numeric value to be bestowed with a unit.
#' @param unit
#'  A character vector indicating the unit. See `units::valid_udunits()` for
#'  possible units.
#'
#' @seealso `units::set_units()`
#' @export
#' @examples
#' u(10, "km") # 10 kilometer
#' u(10, "mm") # 10 mm
#'
#' # comparison taking into account the unit
#' u(100, "m") > u(1, "km")
#'
u <- function(x, unit) {
  stopifnot(is.numeric(x))
  stopifnot(is.character(unit))
  # x must be a pure number
  stopifnot("Argument must a unitless number" = !inherits(x, "units"))

  # mode required because we pass a string instead of an expression
  units::set_units(x, unit, mode = "standard")
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
