ucharts <- function(data, x, type = "Line", width = NULL, height = NULL, elementId = NULL) {

  x <- eval(substitute(x, parent.frame()), data)

  assign("data", data, envir = data_env)
  assign("x", x, envir = data_env)

  # forward options using x
  x = list(
    type = type,
    config = list(
      meta = list(
        caption = ""
      )
    )
  )

  #attr(x, 'TOJSON_ARGS') <- list(auto_unbox = FALSE)

  # create widget
  htmlwidgets::createWidget(
    name = 'uvcharts',
    x,
    width = width,
    height = height,
    package = 'uvcharts',
    elementId = elementId
  )
}

prep_dataset <- function(x, values){
  dataset <- cbind.data.frame(as.character(x), values)
  names(dataset) <- c("name", "value")
  dataset <- list(apply(dataset, 1, as.list))
  return(dataset)
}
