#' Scatter Plot with Lattice
#'
#' @description Creates a scatter plot using lattice for two numeric variables.
#' @export
create_scatterplot_lattice <- function(data, x_col, y_col) {
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  if (!(x_col %in% colnames(data)) || !(y_col %in% colnames(data))) {
    stop("Specified columns must exist in the data frame.")
  }
  lattice::xyplot(data[[y_col]] ~ data[[x_col]], data = data,
                  main = "Scatter Plot",
                  xlab = x_col,
                  ylab = y_col
  )
}

