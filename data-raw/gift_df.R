#' Extract the deepest-leftest parenthesis
#'
#' @param definition A definition of a pathway
#'
#' @return The first parenthesis found
#' @export
#'
#' @examples
#' extract_first_parenthesis("a (b (c d)) (e f)")
extract_first_parenthesis <- function(definition) {
  split_definition <-
    stringr::str_split(definition, "", simplify = FALSE)[[1]]
  definition_length <- length(split_definition)

  start <- 0
  end <- 0

  for (i in seq_len(definition_length)) {
    character <- split_definition[i]
    if (character == "(") {
      start <- i
    } else if (character == ")") {
      end <- i
      return(stringr::str_sub(definition, start, end))
    }
  }
}



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
    subgraph_definition <-
      extract_first_parenthesis(processed_definition)

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


#' Transform the plus sign into a space inside a definition
#'
#' @param definition A pathway definition in string format
#'
#' @return A pathway definition in string format
#' @export
#'
#' @examples
#' plus_to_space("a+b")
plus_to_space <- function(definition) {
  definition %>%
    stringr::str_replace_all("\\+", " ")
}



#' Make the node names unique inside a decoupled graph
#'
#' @param decoupled_graph A decoupled graph from `decouple_graph`
#'
#' @return A decoupled graph with unique node names
#' @export
#'
#' @examples
#' "a (b c)" %>%
#'   decouple_graph() %>%
#'   dereplicate_graph()
dereplicate_graph <- function(decoupled_graph) {
  decoupled_graph %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(subgraph_definition = "subgraph_definition") %>%
    tibble::column_to_rownames("subgraph_definition") %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("subgraph_name") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      level = row_number() - 1,
      subgraph_definition_new =
        subgraph_definition %>%
          stringr::str_replace_all("([\\w\\.]+)", stringr::str_glue("\\1_{level}")) %>%
          stringr::str_replace_all("(subgraph_\\d+)_\\d+", "\\1") # undo subgraph
    ) %>%
    dplyr::select(subgraph_name, subgraph_definition_new) %>%
    tibble::deframe() %>%
    as.list()
}


#' Process a subgraph definition with a comma: fan out from source to nodes,
#' fan in from nodes to sink
#'
#' @param subgraph_definition A subgraph definition
#' @param subgraph_id The subgraph id
#'
#' @return A data frame with the edges
#' @export
#'
#' @examples
#' process_comma_subdefinition("b,c", "subgraph_1")
process_comma_subdefinition <-
  function(subgraph_definition, subgraph_id) {
    nodes <-
      subgraph_definition %>%
      stringr::str_split(",") %>%
      unlist()

    edges <-
      bind_rows(
        tibble(
          from = stringr::str_glue("{subgraph_id}_source"),
          to = nodes
        ),
        tibble(
          from = nodes,
          to = stringr::str_glue("{subgraph_id}_sink")
        )
      ) %>%
      mutate(
        from = if_else(
          stringr::str_detect(from, "^subgraph_\\d+$"),
          stringr::str_glue("{from}_sink"),
          from
        ),
        to = if_else(
          stringr::str_detect(to, "^subgraph_\\d+$"),
          stringr::str_glue("{to}_source"),
          to
        )
      )

    return(edges)
  }


#' Process a subgraph definition with a space: source, node1, ... nodeN, sink
#'
#' @param subgraph_definition A subgraph definition
#' @param subgraph_id The subgraph id
#'
#' @return A data frame with the edges
#' @export
#'
#' @examples
#' process_space_subdefinition("b c", "subgraph_1")
process_space_subdefinition <-
  function(subgraph_definition, subgraph_id) {
    nodes <-
      subgraph_definition %>%
      stringr::str_split(" ") %>%
      unlist()

    edges <-
      tibble(
        from = stringr::str_glue("{subgraph_id}_source"),
        to = nodes[1]
      ) %>%
      bind_rows(
        tibble(from = nodes) %>%
          mutate(
            to = lead(from, default = stringr::str_glue("{subgraph_id}_sink"))
          )
      ) %>%
      mutate(
        from = if_else(
          stringr::str_detect(from, "^subgraph_\\d+$"),
          stringr::str_glue("{from}_sink"),
          from
        ),
        to = if_else(
          stringr::str_detect(to, "^subgraph_\\d+$"),
          stringr::str_glue("{to}_source"),
          to
        )
      )

    return(edges)
  }


#' Process a single node subdefinition: source, node, sink
#'
#' @param subgraph_definition Subgraph definition
#' @param subgraph_id Subgraph id
#'
#' @return A data frame with the edges
#' @export
#'
#' @examples
#' process_single_node_subdefinition("b", "subgraph_1")
process_single_node_subdefinition <- function(subgraph_definition, subgraph_id) {
  nodes <- subgraph_definition
  edges <- tibble(
    from = c(stringr::str_glue("{subgraph_id}_source"), subgraph_definition),
    to = c(subgraph_definition, stringr::str_glue("{subgraph_id}_sink"))
  )
  return(edges)
}



#' Convert dereplicated graph to adjacency list
#'
#' @param dereplicated_graph A dereplicated graph from `dereplicate_graph`
#'
#' @return A list of data frames with the edges
#' @export
#'
#' @examples
#' "a (b,c) (c+d)" %>%
#'   plus_to_space() %>%
#'   distillR::decouple_graph() %>%
#'   dereplicate_graph() %>%
#'   dereplicated_graph_to_adjacency_list()
dereplicated_graph_to_adjacency_list <-
  function(dereplicated_graph) {
    list_of_edge_dfs <- list()

    for (subgraph_id in names(dereplicated_graph)) {
      subgraph_definition <- dereplicated_graph[[subgraph_id]]

      if (stringr::str_detect(subgraph_definition, ",")) {
        edges <-
          process_comma_subdefinition(subgraph_definition, subgraph_id)
      } else if (stringr::str_detect(subgraph_definition, " ")) {
        edges <-
          process_space_subdefinition(subgraph_definition, subgraph_id)
      } else { # single node subgraph
        edges <- process_single_node_subdefinition(subgraph_definition, subgraph_id)
      }

      list_of_edge_dfs[[subgraph_id]] <- edges
    }

    list_of_edge_dfs
  }



#' Remove intermediate sources and sinks
#'
#' @param edge_df A data frame with the edges
#'
#' @return A data frame with the edges
#' @export
#'
#' @examples
#' "a (b,c) (c+d)" %>%
#'   plus_to_space() %>%
#'   decouple_graph() %>%
#'   dereplicate_graph() %>%
#'   dereplicated_graph_to_adjacency_list() %>%
#'   dplyr::bind_rows() %>%
#'   trim_intermediate_sources_and_sinks_df()
trim_intermediate_sources_and_sinks_df <- function(edge_df) {
  nodes_to_delete <-
    c(
      edge_df %>% filter(str_detect(from, "^subgraph_")) %>% pull(from),
      edge_df %>% filter(str_detect(to, "^subgraph_")) %>% pull(to)
    ) %>%
    unique()

  edge_df_final <- edge_df

  for (node_to_delete in nodes_to_delete) {
    predecessors <-
      edge_df_final %>%
      filter(to == node_to_delete) %>%
      pull(from)

    successors <-
      edge_df_final %>%
      filter(from == node_to_delete) %>%
      pull(to)

    new_edges <-
      expand.grid(
        from = predecessors,
        to = successors,
        stringsAsFactors = FALSE
      )

    edge_df_final <-
      edge_df_final %>%
      filter(!(from == node_to_delete | to == node_to_delete)) %>%
      bind_rows(new_edges)
  }

  return(edge_df_final)
}


#' Append tag to edge dataframe
#'
#' @param df Dataframe with the edges (from, to)
#' @param gift_id tag to append to the beginning of each node
#'
#' @return dataframe with the nodes renamed
#' @export
#'
#' @examples
#' tibble::tibble(
#'   from = c("root_source", "a"),
#'   to = c("a", "sink")
#' ) %>%
#'   append_gift_id_to_df("tag")
append_gift_id_to_df <- function(df, gift_id) {
  df %>%
    mutate(
      from = stringr::str_glue("{gift_id}_{from}"),
      to = stringr::str_glue("{gift_id}_{to}")
    )
}


#' Pipeline to go from definition to edge list, with nodes properly renamed
#'
#' @param definition string with the pathway definition
#' @param gift_id  the identity of the GIFT
#'
#' @return dataframe with the edges (from, to) of the pathway
#' @export
#'
#' @examples
#' definition_to_edge_df("a (b,c) (c+d)", "mygift")
definition_to_edge_df <- function(definition, gift_id) {
  definition %>%
    plus_to_space() %>%
    decouple_graph() %>%
    dereplicate_graph() %>%
    dereplicated_graph_to_adjacency_list() %>%
    dplyr::bind_rows() %>%
    trim_intermediate_sources_and_sinks_df() %>%
    append_gift_id_to_df(gift_id)
}



#' Build the GIFT database as a dataframe
#'
#' @return data frame
#' @export
#'
#' @examples
#' build_gift_df()
build_gift_df <- function() {
  load("data-raw/GIFT_db.rda")

  giftdb <-
    GIFT_db %>%
    mutate( # Fixes
      Definition = if_else(
        Code_bundle == "B060213",
        "(2.6.1.1,2.6.1.5,2.6.1.27,2.6.1.57) 1.13.11.27 1.13.11.5 5.2.1.2 3.7.1.2",
        Definition
      ),
      Definition = if_else(
        Code_bundle == "B010301",
        "K13800,K13809,K09903",
        Definition
      )
    )

  gift_df <-
    giftdb %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(
      edge_df = definition_to_edge_df(Definition, Code_bundle) %>% list()
    ) %>%
    unnest(edge_df) %>%
    select(-Definition)

  return(gift_df)
}


gift_df <- build_gift_df()
save(gift_df, file = "data/gift_df.rda", compress = "xz")
