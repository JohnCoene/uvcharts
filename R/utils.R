ucharts <- function(data, x, type, width, height, elementId) {

  x.name <- deparse(substitute(x, parent.frame()))
  x <- eval(substitute(x, parent.frame()), data)

  if(is.numeric(x) || is.integer(x)){
    dplyr::arrange(data, x)
  }

  assign("data", data, envir = data_env)
  assign("x", x, envir = data_env)
  assign("x.name", x.name, envir = data_env)

  # forward options using x
  x = list(
    type = type,
    config = list(
      meta = list(
        caption = ""
      ),
      graph = list(
        responsive = TRUE
      ),
      dimension = list(
        height = 200
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
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = FALSE,
                                             viewer.defaultWidth = '100%',
                                             viewer.defaultHeight = '100%',
                                             knitr.defaultWidth = '100%'),
    package = 'uvcharts',
    elementId = elementId
  )
}

prep_dataset <- function(serie){

  x <- get("x", envir = data_env)

  dataset <- cbind.data.frame(as.character(x), serie)

  names(dataset) <- c("name", "value")
  dataset[is.na(dataset)] <- 0 # replace NA
  dataset <- apply(dataset, 1, function(row) as.list(row))

  names(dataset) <- NULL
  # # values as numeric
  dataset <- lapply(dataset, function(x){
    num <- tryCatch(as.numeric(paste0(x[[2]])), error = function(e) e)
    x[[2]] <- if(!is(num, "error")) num
    return(x)
  })
  return(list(dataset))
}


add_data_ <- function(p, data, series_name){
  col_name <- as.character(series_name)
  serie <- data[, col_name]
  p$x$graph$categories <- append(p$x$graph$categories, list(series_name))

  # prepare data
  dataset <- prep_dataset(serie)

  names(dataset) <- series_name

  p$x$graph$dataset <- append(p$x$graph$dataset, dataset)
  p
}

add_series <- function(p, serie){

  data <- get("data", envir = data_env)
  series_name <- deparse(substitute(serie, parent.frame()))

  if(dplyr::is.grouped_df(data)){

    if(length(dplyr::groups(data)) > 1) stop("uvcharts only supports one group.", call. = FALSE)

    # deparse groups
    g <- dplyr::groups(data)
    g <- unlist(lapply(g, deparse))

    x.name <- get("x.name", envir = data_env) # get name of x variable

    # select approriate columns
    data <- dplyr::select_(data, x.name, g, series_name)

    # spread groups - long to wide
    data <- tryCatch(tidyr::spread_(data, g, series_name),
                     error = function(e) e)

    if(is(data, "error")) stop("Multiple y values for single x.", call. = FALSE) # test

    data <- as.data.frame(data)

    assign("x", data[,1], envir = data_env) # override x

    # remove x column from selection
    data <- dplyr::select(data, 2:ncol(data))

    series_name <- names(data)

  }

  # loop
  for(i in 1:length(series_name)){
    p <- add_data_(p, data, series_name[i])
  }

  p
}
