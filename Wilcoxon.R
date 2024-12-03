#' Perform Wilcoxon Rank-Sum Test
#'
#' @description This function performs the Wilcoxon rank-sum test on two numeric vectors.
#' @examples
#' group1 <- c(1.2, 2.3, 1.8)
#' group2 <- c(3.1, 3.5, 3.2)
#' perform_wilcox_test(group1, group2)
#' @export
perform_wilcox_test <- function(group1, group2) {
  if (!is.numeric(group1) || !is.numeric(group2)) {
    stop("Both inputs must be numeric vectors.")
  }
  test_result <- wilcox.test(group1, group2)
  return(list(statistic = test_result$statistic, p_value = test_result$p.value))
}
