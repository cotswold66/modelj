#' Method for printing summary table
#'
#' @param object modelj object
#' @param caption string containing table caption
#'
#' @return
#' @export
#'
summary.modelj <- function(object, caption = NULL){
  x <- purrr::pluck(object, "summary")
  x <- knitr::kable(
    x,
    booktabs = TRUE,
    linesep = "",
    caption = caption,
    digits = c(0, 2, 2, 2, 2, 0, 4, 4 ,2, 0 ,2, 0)
    )
  x <- kableExtra::kable_styling(
    x,
    latex_options = c("striped", "HOLD_posiiton"),
    full_width = FALSE
    )
  x <- kableExtra::add_header_above(
    x,
    c(" " = 1, "Contrasts Statistics" = 7, "Power Calc" = 4)
    )
  return(x)
}
