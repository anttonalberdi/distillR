#' Compute the shortest path cost for a given set of annotations
#'
#' @param annotation_vector A vector of annotations to compute the shortest
#' path between the source and sink metabolite
#'
#' @return A tibble with the following columns:
#' - `pathway_id`: The pathway identifier
#' - `length_shortest_path`: The length of the shortest path
#' - `cost`: The number of genes missing in the shortest path
#' @export
#'
#' @examples
#' compute_shortest_paths(c("S09X", "6.1.1.4"))
compute_shortest_paths <- function(annotation_vector) {
  annotation_vector_clean <- c("root", "source", annotation_vector) %>% unique()

  gift_df %>%
    tidyr::separate(
      col = from,
      into = c("from_code", "from_annotation", "from_level"),
      sep = "_",
      remove = FALSE
    ) %>%
    tidyr::separate(
      col = to,
      into = c("to_code", "to_annotation", "to_level"),
      sep = "_",
      remove = FALSE
    ) %>%
    dplyr::select(-from_code, -from_level, -to_code, -to_level) %>%
    dplyr::mutate(
      cost = dplyr::if_else(
        condition =
          (from_annotation %in% annotation_vector_clean) &
          (to_annotation %in% annotation_vector_clean),
        true = 0,
        false = 1
      )
    ) %>%
    dplyr::select(pathway_id, from, to, cost) %>%
    tidyr::nest(graph_df = c(from, to, cost)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      graph = cppRouting::makegraph(graph_df, directed = TRUE) %>% list(),
      source = stringr::str_glue("{pathway_id}_root_source"),
      sink = stringr::str_glue("{pathway_id}_root_sink"),
      shortest_path = cppRouting::get_path_pair(
        Graph = graph, from = source, to = sink
      ),
      # remove root and sink effect
      length_shortest_path = length(shortest_path) - 1,
      cost = cppRouting::get_distance_pair(
        Graph = graph, from = source, to = sink
      ),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(pathway_id, length_shortest_path, cost) %>%
    dplyr::arrange(pathway_id, length_shortest_path, cost)
}
