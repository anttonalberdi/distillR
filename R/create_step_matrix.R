#' Creates a matrix that specifies the hierarchical structure of the definition of a metabolic pathway/module
#'
#' @param def_decomp Vector of decomposed units of a definition string of a metabolic pathway/module (produced by decompose_definition())
#' @param def_level Vector of the hierarchical level of decomposed units of a definition string of a metabolic pathway/module (produced by set_levels())
#' @return A decomposed hierarchy matrix
#' @examples
#' create_step_matrix(definition_decomposed,definition_levels)
#' @export

create_step_matrix <- function(def_decomp,def_level){

  #Create table structure
  def_table <- as.data.frame(cbind(def_decomp,def_level))

  #Create level 0 structure
  L0_splits <- rownames(def_table[((def_decomp == " ") | (def_decomp == "+") | (def_decomp == ",")) & (def_level == 0),])
  L0_group <- c()
  group <- 1
  for (i in c(1:length(def_decomp))){
    if(i %in% L0_splits){group=group+1}
    L0_group <- c(L0_group,group)
  }
  def_table[,"L0_group"] <- L0_group

  #If more hierarchical levels exist, iterate across levels.
  max_level <- max(def_level)
  if(max_level > 0){
    levels <- c(1:max_level)
    #Next levels
    for(level in levels){
      splits <- rownames(def_table[((def_decomp == " ") | (def_decomp == "+") | (def_decomp == ",")) & (def_level <= level),])
      level_group <- c()
      group <- 1
      for (i in c(1:length(def_decomp))){
        if(i %in% splits){group=group+1}
        if(def_table[i,"def_level"]>=level){
          level_group <- c(level_group,group)
        }else{
          level_group <- c(level_group,NA)
          group = 1
        }
      }
      def_table[,paste0("L",level,"_group")] <- level_group
    }
  }

  #Polish matrix
  def_table[(def_table$def_decomp == "(") | (def_table$def_decomp == ")"),] <- NA

  #Print matrix
  return(def_table)
}
