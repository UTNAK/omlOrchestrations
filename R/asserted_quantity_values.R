#' Asserted Quantity Values
#'
#' @param values The `results$bindings` of a SPARQL JSON result. Each binding
#'   has `quantity`, `value` and `unit`.
#'
#' @returns a data frame of quatity values with the following named elements:
#' - `quantity` IRI of the quantity
#' - `value` The value of the quantity. This is a list column (`I(list(...))`):
#'   each element holds one R value, which may be a scalar, a vector or `NULL`.
#'   The SPARQL literal is converted to a double here, once, with a correctly
#'   rounded parser, so no digits are lost.
#' - `unit` The unit of the quantity
#' @export
#'
#' @examples
#' \dontrun{
#' vdf <- asserted_quantity_values(values)
#' }
#'
asserted_quantity_values <- function(values) {
  data.frame(
    quantity = unname(vapply(values, function(x) x$quantity$value, character(1))),
    value = I(unname(lapply(values, function(x) parse_number(x$value$value)))),
    unit = unname(vapply(values, function(x) x$unit$value, character(1))),
    stringsAsFactors = FALSE
  )
}

# Convert the lexical form of a number to a double without losing digits.
#
# `as.numeric()` uses R's own parser, which is not correctly rounded on
# platforms without a long double (e.g. Apple Silicon): it can be off by one
# ulp, even for short strings. jsonlite uses a correctly rounded parser.
# Strings that are not JSON numbers (".5", "INF", ...) fall back to
# `as.numeric()`.
parse_number <- function(s) {
  vapply(s, function(x) {
    r <- tryCatch(jsonlite::parse_json(x), error = function(e) NULL)
    if (is.numeric(r) && length(r) == 1) as.double(r) else as.numeric(x)
  }, numeric(1), USE.NAMES = FALSE)
}

# Make the `value` column of a vdf a list column.
#
# A list column keeps each value as an R object (full double precision,
# vectors, NULL). Older vdfs with a character or numeric column are converted
# so that callers who build a vdf by hand keep working.
as_value_column <- function(value) {
  if (is.list(value)) {
    I(unclass(value))
  } else if (is.character(value)) {
    I(lapply(value, parse_number))
  } else {
    I(as.list(value))
  }
}
