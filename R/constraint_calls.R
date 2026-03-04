#' Constraint Calls
#'
#' @param calls
#'
#' @returns a data frame of constraint calls with the following named elements:
#' - `quantity` Mapping to the output of the target function
#' - `call` The name of the function in R
#' - `arg_name` Input of the function
#' - `arg_quantity` Mapping to the input of the target function
#' @export
#'
#' @examples
#' vdf <- asserted_quantity_values(calls)
#'
constraint_calls <- function(calls) {
  dfrow <- function(x) data.frame(
    quantity = x$quantity$value,
    call = x$call$value,
    arg_name = x$arg_name$value,
    arg_quantity = x$arg_quantity$value,
    # encoding_scheme = if (is.null(es <- x$encoding_scheme)) NA else es$value,
    stringsAsFactors = FALSE
  )
  Reduce(f = rbind, x = Map(f = dfrow, calls)) |>
    dplyr::arrange(quantity, call, arg_name)

}
