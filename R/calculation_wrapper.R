#' Calculation Wrapper
#'
#' @param ograph An igraph graph of the orchestration graph.
#' @param cdf A data frame of the calls.
#' @param vdf A data frame of the values, as made by
#'   [asserted_quantity_values()].
#'
#' @returns `vdf` with one row added for each calculated quantity. The `value`
#'   column is a list column: `result$value[[i]]` is the R value of row `i`
#'   (a scalar, a vector or `NULL`). When every value is a scalar,
#'   `unlist(result$value)` gives a numeric vector.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- calculation_wrapper(ograph, vdf, cdf)
#' }
#'
calculation_wrapper <- function(ograph, vdf, cdf) {
  vdf$value <- as_value_column(vdf$value)
  update_values <- function(v, t, sl) {
    if (length(sl) == 0) v else calculate_quantity(quantity = t, cdata = cdf, vdata = v)
  }
  validate_ds <- function(t, d) TRUE
  validate_tree <- function(t) igraph::is_dag(t)
  rollupTree::rollup(ograph, vdf, update_values, validate_ds, validate_tree)
}

