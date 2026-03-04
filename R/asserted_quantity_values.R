#' Asserted Quantity Values
#'
#' @param values
#'
#' @returns a data frame of quatity values with the following named elements:
#' - `quantity` IRI of the quantity
#' - `value` The value of the quantity
#' - `unit` The unit of the quantity
#' @export
#'
#' @examples
#' vdf <- asserted_quantity_values(values)
#'
asserted_quantity_values <- function(values) {
  data.frame(
    quantity =  unlist(Map(f = function(x) x$quantity$value, values)),
    value =  unlist(Map(f = function(x) list(value = x$value$value), values)),
    unit =  unlist(Map(f = function(x) list(unit = x$unit$value), values)),
    stringsAsFactors = FALSE
  )
}
