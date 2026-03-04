#' Calculate Quantity
#'
#' @param quantity A data frame of the quantity.
#' @param cdata A data frame of the calls.
#' @param vdata A data frame of the values.
#'
#' @returns A data frame
#'
#' @export
#'
#' @examples
#' df <- calculate_quantity(quantity, cdata, vdata)
#'
calculate_quantity <- function(quantity, cdata, vdata) {
  call_info <- cdata[cdata$quantity == quantity, ]
  call = call_info[1, "call"]
  call_info_values <- merge(x = call_info, y = vdata, by.x = "arg_quantity", by.y = "quantity")
  arg_list <- Map(f = function(n, v, u) list(value = as.numeric(v), unit = u),
                  call_info_values$arg_name, call_info_values$value, call_info_values$unit)
  r <- do.call(call, arg_list)
  rbind(vdata, c(quantity = quantity, value = r$value, unit = r$unit))
}

