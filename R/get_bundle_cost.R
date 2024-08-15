get_bundle_cost <- function(annotation_vector) {

  annotation_vector <- c("root", "source", annotation_vector) %>% unique()  # nolint

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
          (from_annotation %in% annotation_vector) &
          (to_annotation %in% annotation_vector),
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
      # completeness = 1 - cost / length_shortest_path  # nolint
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(pathway_id, length_shortest_path, cost)
}
