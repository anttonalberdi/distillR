decompose_definition <- function(definition){
  unlist(strsplit(definition, "(?=[ ( ),+]+)", perl = TRUE))
}
