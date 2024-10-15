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

  gift_graph_with_costs <-
    gift_graph |>
    dplyr::mutate(
      cost = dplyr::if_else(
        condition =
          (from_annotation %in% annotation_vector_clean) &
            (to_annotation %in% annotation_vector_clean),
        true = 0,
        false = 1
      )
    ) |>
    dplyr::select(from, to, cost) |>
    cppRouting::makegraph(directed = TRUE)

  pathway_ids <- gift_graph |>
    dplyr::pull(pathway_id) |>
    unique()

  sources <- stringr::str_glue("{pathway_ids}_root_source")
  sinks <- stringr::str_glue("{pathway_ids}_root_sink")

  shortest_paths <-
    gift_graph_with_costs |>
    cppRouting::get_path_pair(
      from = sources,
      to = sinks
    )

  shortest_path_lengths <-
    shortest_paths |>
    purrr::map_int(~ length(.x) - 1)

  costs <-
    gift_graph_with_costs |>
    cppRouting::get_distance_pair(
      from = sources,
      to = sinks
    )

  tibble::tibble(
    pathway_id = pathway_ids,
    length_shortest_path = shortest_path_lengths |> as.integer(),
    cost = costs |> as.integer()
  )
}
