#' Sweeps list names and row names in a list of matrices
#'
#' @param matrix_list A list of matrices (e.g. qGIFT_table yielded by distillq())
#' @return A list of matrices with sweeped list names and row names
#' @examples
#' sweep_matrix_list(matrix_list)
#' @export

sweep_matrix_list <- function(matrix_list) {
# Declare vectors
  rows <- rownames(matrix_list[[1]])
  columns <- colnames(matrix_list[[1]])
  tables <- names(matrix_list)

# Create new empty list of matrices
  newlist <- replicate(length(rows), matrix(NA, length(tables), length(columns)), simplify = F)
  names(newlist) <- rows

# Populate empty list of matrices
  for (row in rows) {
    for (table in tables) {
      colnames(newlist[[row]]) <- columns
      rownames(newlist[[row]]) <- tables
      newlist[[row]][table, ] <- matrix_list[[table]][row, ]
    }
  }

# Return object
  return(newlist)
}
