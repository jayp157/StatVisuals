#' Create an S3 Object for Data Summary
#'
#' @description This function creates an S3 object that summarizes a given dataset.
#' @export
create_summary <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  summary_list <- list(
    summary_stats = summary(data),
    num_rows = nrow(data),
    num_cols = ncol(data)
  )
  structure(summary_list, class = "summary_data")
}

#' Define S4 Class for Biomarker Analysis
#'
#' @description This S4 class stores biomarker data and analysis results.
#' @export
setClass(
  "BiomarkerAnalysis",
  slots = list(
    data = "data.frame",
    results = "list"
  )
)

