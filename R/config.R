#' Configure the chart
#'
#' @export
uv_config <- function(p, ...){

  opts <- list(...)

  p$x$config <- append(p$x$config, opts)

  p
}
