#' Setup a uvchart
#'
#' Initiate a uvchart
#'
#' @param data data.frame of data to plot.
#' @param x \code{x} variable.
#' @param type type of chart to plot, see details.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId id of div element containing chart.
#'
#' @import htmlwidgets
#' @rdname uv_charts
#'
#' @export
uv_charts <- function(data, x, type = "Line", width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, type, width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_bar <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Bar", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_line <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Line", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_area <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Area", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_stackbar <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "StackedBar", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_stackarea <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "StackedArea", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_pie <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Pie", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_donut <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Donut", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_percentbar <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "PercentBar", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_percentarea <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "PercentArea", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_stepupbar <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "StepUpBar", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_polar <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "PolarArea", width, height, elementId)
}

#' @inheritParams uv_charts
#' @rdname uv_charts
#' @export
uv_waterfall <- function(data, x, width = "100%", height = "100%", elementId = NULL) {

  ucharts(data, x, "Waterfall", width, height, elementId)
}

#' Shiny bindings for uvcharts
#'
#' Output and render functions for using uvcharts within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a uvcharts
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name uvcharts-shiny
#'
#' @export
uvchartsOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'uvcharts', width, height, package = 'uvcharts')
}

#' @rdname uvcharts-shiny
#' @export
renderUvcharts <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, uvchartsOutput, env, quoted = TRUE)
}
