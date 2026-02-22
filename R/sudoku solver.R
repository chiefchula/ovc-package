solve_sudoku <- function(file_path) {

  # Read Sudoku from file
  sudoku <- as.matrix(read.table(file_path))

  if (!all(dim(sudoku) == c(9, 9))) {
    stop("Sudoku must be a 9x9 grid.")
  }

  # Function to check if number is valid
  is_valid <- function(board, row, col, num) {

    # Check row
    if (num %in% board[row, ]) return(FALSE)

    # Check column
    if (num %in% board[, col]) return(FALSE)

    # Check 3x3 box
    start_row <- 3 * ((row - 1) %/% 3) + 1
    start_col <- 3 * ((col - 1) %/% 3) + 1

    box <- board[start_row:(start_row + 2),
                 start_col:(start_col + 2)]

    if (num %in% box) return(FALSE)

    return(TRUE)
  }

  # Backtracking solver
  solve <- function(board) {

    for (row in 1:9) {
      for (col in 1:9) {

        if (board[row, col] == 0) {

          for (num in 1:9) {

            if (is_valid(board, row, col, num)) {

              board[row, col] <- num

              result <- solve(board)
              if (!is.null(result)) {
                return(result)
              }

              board[row, col] <- 0
            }
          }

          return(NULL)
        }
      }
    }

    return(board)
  }

  solution <- solve(sudoku)

  if (is.null(solution)) {
    stop("No solution exists.")
  }

  return(solution)
}
