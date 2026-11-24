#' Calculate Quantity
#'
#' @param quantity A data frame of the quantity.
#' @param cdata A data frame of the calls.
#' @param vdata A data frame of the values. The `value` column should be a
#'   list column, as made by [asserted_quantity_values()]. A character or
#'   numeric column is converted to a list column first.
#'
#' @returns `vdata` with one row added for `quantity`. Its `value` is the
#'   `value` the call returned, kept as an R object (not converted to text),
#'   so a scalar, a vector or `NULL` passes to the next call unchanged.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- calculate_quantity(quantity, cdata, vdata)
#' }
#'
calculate_quantity <- function(quantity, cdata, vdata) {
  vdata$value <- as_value_column(vdata$value)
  call_info <- cdata[cdata$quantity == quantity, ]
  call <- call_info[1, "call"]
  call_info_values <- merge(x = call_info, y = vdata, by.x = "arg_quantity", by.y = "quantity")
  arg_list <- Map(f = function(n, v, u) list(value = v, unit = u),
                  call_info_values$arg_name, call_info_values$value, call_info_values$unit)
  r <- do.call(call, arg_list)
  unit <- if (is.null(r$unit)) NA_character_ else r$unit
  rbind(vdata, data.frame(quantity = quantity, value = I(list(r$value)), unit = unit,
                          stringsAsFactors = FALSE))
}
