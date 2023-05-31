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
  split_definition <- stringr::str_split(definition, "", simplify = FALSE)[[1]]
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
