ucharts <- function(data, x, type, width, height, elementId) {

  if(!is.data.frame(data)){
    stop("data must be a dataframe", call. = FALSE)
  }

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
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE,
                                             knitr.defaultWidth = '100%'),
    package = 'uvcharts',
    elementId = elementId
  )
}

prep_dataset <- function(x, values){
  dataset <- cbind.data.frame(as.character(x), values)
  names(dataset) <- c("name", "value")
  rownames(dataset) <- NULL
  dataset <- apply(dataset, 1, function(row) as.list(row[!is.na(row)]))

  # values as numeric
  dataset <- lapply(dataset, function(x){
    x[[2]] <- as.numeric(x[[2]])
    x
  })
  return(list(dataset))
}
