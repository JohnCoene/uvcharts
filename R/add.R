#' Add serie
#'
#' @param p a \code{uvcharts} object.
#' @param serie values to plot.
#'
#' @examples
#' mtcars %>%
#'   uv_bar(mpg) %>%
#'   uv_add(qsec) %>%
#'   uv_add(drat)
#'
#' @export
uv_add <- function(p, serie){

  p <- add_series(p, serie)

  p

}
