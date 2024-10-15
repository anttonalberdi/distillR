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

  # annotation_vector <- c(
  #   "K00601", "K00602", "K00764", "K01492", "K01587", "K01588", "K01589",
  #   "K01756", "K01923", "K01933", "K01945", "K01952", "K06863", "K08289",
  #   "K11176", "K11787", "K11788", "K11808", "K13713", "K23264", "K23265",
  #   "K23269", "K23270"
  # )

  annotation_vector_clean <- c("root", "source", annotation_vector) |> unique()

  gift_graph |>
    # add costs
    dplyr::mutate(
      cost = dplyr::if_else(
        condition =
          (from_annotation %in% annotation_vector_clean) &
          (to_annotation %in% annotation_vector_clean),
        true = 0,
        false = 1
      )
    ) |>
    # compute shortest_paths
    dplyr::select(pathway_id, from, to, cost) |>
    tidyr::nest(graph_df = c(from, to, cost)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      graph = cppRouting::makegraph(graph_df, directed = TRUE) |> list(),
      source = stringr::str_glue("{pathway_id}_root_source"),
      sink = stringr::str_glue("{pathway_id}_root_sink"),
      shortest_path = cppRouting::get_path_pair(
        Graph = graph,
        from = source,
        to = sink
      ),
      # remove root and sink effect
      length_shortest_path = length(shortest_path) - 1,
      cost = cppRouting::get_distance_pair(
        Graph = graph, from = source, to = sink
      ),
    ) |>
    dplyr::ungroup() |>
    # clean up
    dplyr::select(pathway_id, length_shortest_path, cost) |>
    dplyr::arrange(pathway_id, length_shortest_path, cost)
}
