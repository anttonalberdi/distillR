#' Calculates a GIFT from a gene bundle
#'
#' @param definition Definition string of a given gene bundle
#' @param present Vector of identifiers present in the genome
#' @return A GIFT value
#' @examples
#' compute_GIFT(definition, present)
#' compute_GIFT(
#'   "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))",
#'   c("K01580", "K00823", "K16871")
#' )
#' @export

compute_GIFT <- function(definition, present) {
  present <- sanitize_identifiers(present)
  definition <- sanitize_identifiers(definition)
  def_decomp <- decompose_definition(definition)
  def_level <- set_levels(def_decomp)
  def_table <- create_step_matrix(def_decomp, def_level)
  # List levels
  levels <- colnames(def_table)[c(3:ncol(def_table))]
  # Iterate calculation across levels
  for (level in rev(levels)) {
    definition <- distill_definition(definition, def_table, level, present)
    if (level != "L0_group") {
      def_decomp <- unlist(strsplit(definition, "(?=[ ( ),+]+)", perl = TRUE))
      def_level <- set_levels(def_decomp)
      def_table <- create_step_matrix(def_decomp, def_level)
    }
  }
  # Return value
  return(as.numeric(definition))
}
