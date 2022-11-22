#' Calculates the gene expression/abundance-based MCI of a metabolic pathway
#'
#' @param definition Definition string of a given metabolic pathway/module
#' @param expression_table Table containing expression data per functon
#' @return A list of expression/abundance-based MCI values
#' @examples
#' compute_MCI_expression(definition,expression_table)
#' @export

#UNDER DEVELOPMENT
compute_MCI_expression <- function(definition,expression_table){

  #If using EC codes
  if (grepl(".", definition, fixed = TRUE)){
    rownames(expression_table) <- gsub(".","_",rownames(expression_table),fixed=TRUE)
    definition <- gsub(".","_",definition,fixed=TRUE)
  }

  #Declare definition table
  definition_expression_list <- as.list(rep(definition,ncol(expression_table)))
  names(definition_expression_list) <- colnames(expression_table)

  #Decompose definition
  def_decomp_list <- lapply(definition_expression_list, function(x) unlist(strsplit(x, "(?=[ ( ),+]+)", perl=TRUE)))

  #Set levels
  def_level_list <- lapply(def_decomp_list, function(x) set_levels(x))

  #Definition-level table
  def_table_list <- list()
  for (l in names(def_level_list)){
    def_table <- create_step_matrix(def_decomp_list[[l]],def_level_list[[l]])
    def_table_list[[l]] <- def_table
  }

  #Calculate number of levels
  levels_list <- lapply(def_table_list, function(x) colnames(x)[c(3:ncol(x))])

  #Iterate calculation across levels and samples
  for(level in rev(levels_list[[1]])){
    for(s in names(definition_expression_list)){
      definition_expression_list[[s]] <- distillate_definition_expression(sample=s,definition_expression_list[[s]], def_table_list[[s]], level, expression_table)
      if(level != "L0_group"){
        def_decomp_list[[s]] <- unlist(strsplit(definition_expression_list[[s]], "(?=[ ( ),+]+)", perl=TRUE))
        def_level_list[[s]] <- set_levels(def_decomp_list[[s]])
        def_table_list[[s]] <- create_step_matrix(def_decomp_list[[s]],def_level_list[[s]])
      }
    }
  }
  #Return value
  return(definition_expression_list)
}
