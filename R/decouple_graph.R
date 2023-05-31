#' Convert a definition into a list of nodes
#'
#' @param definition A pathway definition in string format
#'
#' @return A list with all the subgraphs
#' @export
#'
#' @examples
#' decouple_graph("a (b c)")

decouple_graph <- function(definition) {

  processed_definition <- definition
  graph <- list()

  while (stringr::str_detect(processed_definition, "\\(")) {
    subgraph_definition <- extract_first_parenthesis(processed_definition)

    subgraph_name <- stringr::str_glue("subgraph_{length(graph)}")

    graph[[subgraph_name]] <-
      subgraph_definition %>%
      stringr::str_remove_all("[\\(\\)]")

    processed_definition <- gsub(
      pattern = subgraph_definition,
      replacement = subgraph_name,
      x = processed_definition,
      fixed = TRUE
    )
  }
  graph[["root"]] <- processed_definition

  return(graph)
}
