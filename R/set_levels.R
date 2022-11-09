#' Calculates the hierarchical levels of the definition of a metabolic pathway/module
#'
#' @param def_decomp A decomposed definition string of a given metabolic function (produced by decompose_definition())
#' @return A vector of hierarchical levels per character in string
#' @examples
#' set_levels(definition_decomposed)
#' @export

set_levels <- function(def_decomp){
  def_level <- c()
  level=0
  for (i in c(1:length(def_decomp))){
    chr = def_decomp[i]
    if(chr == "("){level=level+1}
    if(chr == ")"){level=level-1}
    def_level <- c(def_level,level)
  }
  return(def_level)
}
