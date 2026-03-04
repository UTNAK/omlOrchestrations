#' Orchestration Graph
#'
#' @param edges
#'
#' @returns An igraph graph.
#'
#' @export
#'
#' @examples
#' ograph <- orchestration_graph(edges)
#'
orchestration_graph <- function(edges) {
  elist <- Reduce(f = c, x = Map(f = function(x) c(x$child$value, x$parent$value), edges), init = c())
  ograph <- igraph::graph_from_edgelist(matrix(elist, ncol = 2, byrow = TRUE))
}
