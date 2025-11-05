#' Load Example Data
#'
#' @description Loads the example dataset from the package's extdata.
#' @return A data frame containing the example data.
#' @export
#'
load_example_data <- function() {
  xlsx_path <- system.file("extdata", "All_CPJB_L11.xlsx", package = "DatingforSingleGrains")
  readxl::read_xlsx(xlsx_path)
}
