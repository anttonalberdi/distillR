get_bundle_cost <- function(annotation_vec) {
  annnotation_vec <- c("root", "source", annotation_vec) %>% unique()

  distillR::gift_df %>%
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
          (from_annotation %in% annotation_vec) &
          (to_annotation %in% annotation_vec),
        true = 0,
        false = 1
      )
    ) %>%
    dplyr::select(Code_bundle, from, to, cost) %>%
    tidyr::nest(graph_df = c(from, to, cost)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      graph = cppRouting::makegraph(graph_df, directed = TRUE) %>% list(),
      source = stringr::str_glue("{Code_bundle}_root_source"),
      sink = stringr::str_glue("{Code_bundle}_root_sink"),
      shortest_path = cppRouting::get_path_pair(Graph = graph, from = source, to = sink),
      length_shortest_path = length(shortest_path) - 1,  # remove root and sink effect
      cost = cppRouting::get_distance_pair(Graph = graph, from = source, to = sink),
      # completeness = 1 - cost / length_shortest_path
    ) %>%
    dplyr::select(Code_bundle, length_shortest_path, cost)
}
