#' Calculation Order
#'
#' @param ograph An igraph graph.
#'
#' @returns A data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' o <- calculation_order(ograph)
#' }
#'
calculation_order <- function(ograph) {
  data.frame(quantity = names(igraph::topo_sort(ograph)), order = 1:length(igraph::V(ograph)))
}
