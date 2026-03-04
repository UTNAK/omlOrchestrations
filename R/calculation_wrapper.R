#' Calculation Wrapper
#'
#' @param ograph An igraph graph of the orchestration graph.
#' @param cdf A data frame of the calls.
#' @param vdf A data frame of the values.
#'
#' @returns A data frame
#'
#' @export
#'
#' @examples
#' result <- calculation_wrapper(ograph, vdf, cdf)
#'
calculation_wrapper <- function(ograph, vdf, cdf) {
  update_values <- function(v, t, sl) {
    if (length(sl) == 0) v else calculate_quantity(quantity = t, cdata = cdf, vdata = v)
  }
  validate_ds <- function(t, d) TRUE
  validate_tree <- function(t) igraph::is_dag(t)
  rollupTree::rollup(ograph, vdf, update_values, validate_ds, validate_tree)
}

