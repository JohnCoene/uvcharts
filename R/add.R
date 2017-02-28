#' Add bars
#'
#' @export
uv_serie <- function(p, serie){

  # fetch from env
  data <- get("data", envir = data_env)
  x <- get("x", envir = data_env)

  series_name <- deparse(substitute(serie)) # get series name
  serie <- eval(substitute(serie), data) # get series values
  p$x$graph$categories <- append(p$x$graph$categories, list(series_name))

  # prepare data
  dataset <- prep_dataset(x, serie)
  names(dataset) <- series_name

  p$x$graph$dataset <- append(p$x$graph$dataset, dataset)

  p

}
