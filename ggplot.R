#' Scatter Plot with ggplot2
#'
#' @description Creates a scatter plot using ggplot2 for two numeric variables.
#' @export
create_scatterplot_ggplot <- function(data, x_col, y_col) {
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  if (!(x_col %in% colnames(data)) || !(y_col %in% colnames(data))) {
    stop("Specified columns must exist in the data frame.")
  }
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(color = "black") +
    theme_minimal() +
    labs(
      title = "Scatter Plot",
      x = x_col,
      y = y_col
    )
}


