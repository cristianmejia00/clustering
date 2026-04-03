# Function to convert a matrix to a dataframe of pairwise comparisons
matrix_to_pairwise <- function(mat) {
  # Check if the input is a matrix
  if (!is.matrix(mat)) {
    stop("Input must be a matrix")
  }
  
  # Get the dimensions of the matrix
  n <- nrow(mat)
  
  # Create empty vectors to store the row names, column names, and values
  row_names <- character()
  col_names <- character()
  values <- numeric()
  
  # Loop through all pairs of elements
  for (i in 1:n) {
    for (j in 1:n) {
      # Skip the diagonal elements if desired (comment out if you want to include them)
      if (i == j) next
      if (mat[i,j] == 0) next
      
      # Add the current pair to our vectors
      row_names <- c(row_names, rownames(mat)[i] %||% as.character(i))
      col_names <- c(col_names, colnames(mat)[j] %||% as.character(j))
      values <- c(values, mat[i, j])
    }
  }
  
  # Create the dataframe
  result <- data.frame(
    row = row_names,
    col = col_names,
    value = values,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Helper function in case rownames/colnames are NULL
`%||%` <- function(x, y) if (is.null(x)) y else x


dataset <- dataset %>% filter(dataset$X_N %in% V(g1)$name)
vcount(g1)
g1 <- g_valid
