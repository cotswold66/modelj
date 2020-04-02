#' Method for printing summary table
#'
#' @param ...
#' @param object modelj object
#'
#' @return
#' @export
#'
summary.modelj <- function(object, ...){
  x <- purrr::pluck(object, "summary")
  x <- knitr::kable(x, digits = c(0, 2, 2, 2, 2, 0, 4, 4 ,2, 0 ,2, 0))
  x <- kableExtra::kable_styling(x)
  x <- kableExtra::add_header_above(x, c(" " = 1, "Contrasts Statistics" = 7, "Power Calc" = 4))
  x
}
