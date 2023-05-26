# nolint start
#' Calculates the quantitative GIFTs from a gene bundle and a gene count table
#'
#' @param definition Definition string of a given gene bundle
#' @param gene_count_table Table containing quantitative data of genes (e.g.,
#' expression or abundance)
#' @return A list of quantitative GIFT values
#' @examples
#' compute_qGIFT(definition, gene_count_table)
#' @export

# UNDER DEVELOPMENT
# compute_qgift <- function(definition, gene_count_table) {
#   # If using EC codes, modify them to allow calculations
#   if (grepl(".", definition, fixed = TRUE)) {
#     gene_count_table <- gene_count_table %>%
#       rowwise() %>%
#       mutate(
#         Annotation =
#           list(map_chr(
#             Annotation,
#             function(x) str_replace_all(x, "\\.", "_")
#           ))
#       ) %>%
#       ungroup()
#
#     definition <- gsub(".", "_", definition, fixed = TRUE)
#   }
#
#   # Declare definition table
#   definition_quantitative_list <-
#     as.list(rep(definition, ncol(gene_count_table) - 1))
#   names(definition_quantitative_list) <- colnames(gene_count_table)[-1]
#
#   # Decompose definition
#   def_decomp_list <- lapply(
#     definition_quantitative_list,
#     function(x) unlist(strsplit(x, "(?=[ ( ),+]+)", perl = TRUE))
#   )
#
#   # Set levels
#   def_level_list <- lapply(def_decomp_list, function(x) set_levels(x))
#
#   # Definition-level table
#   def_table_list <- list()
#   for (l in names(def_level_list)) {
#     def_table <- create_step_matrix(def_decomp_list[[l]], def_level_list[[l]])
#     def_table_list[[l]] <- def_table
#   }
#
#   # Calculate number of levels
#   levels_list <- lapply(def_table_list, function(x) colnames(x)[c(3:ncol(x))])
#
#   # Iterate calculation across levels and samples
#   for (level in rev(levels_list[[1]])) {
#     for (s in names(definition_quantitative_list)) {
#       definition_quantitative_list[[s]] <-
#         distillq_definition(
#           sample = s,
#           definition_expression = definition_quantitative_list[[s]],
#           def_table_list[[s]],
#           level = level,
#           gene_count_table = gene_count_table %>% select(Annotation, s)
#         )
#       if (level != "L0_group") {
#         def_decomp_list[[s]] <- unlist(strsplit(
#           x = definition_quantitative_list[[s]],
#           split = "(?=[ ( ),+]+)",
#           perl = TRUE
#         ))
#         def_level_list[[s]] <- set_levels(def_decomp_list[[s]])
#         def_table_list[[s]] <- create_step_matrix(
#           def_decomp = def_decomp_list[[s]],
#           def_level = def_level_list[[s]]
#         )
#       }
#     }
#   }
#   # Return value
#   return(definition_quantitative_list)
# }
# nolint end
