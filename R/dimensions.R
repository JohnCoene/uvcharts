#' Customise chart dimensions
#'
#' Customise the chart dimensions.
#'
#' @export
uv_dimensions <- function(p, width, height){

  opts <- list()
  opts$width <- if(!missing(width)) width
  opts$height <- if(!missing(height)) height

  p$x$config$dimensions <- append(p$x$config$dimensions, opts)

  p
}

#' Customise chart margin
#'
#' Customise the chart margin.
#'
#' @export
uv_margin <- function(p, top, bottom, left, right){

  opts <- list()
  opts$top <- if(!missing(top)) top
  opts$bottom <- if(!missing(bottom)) bottom
  opts$left <- if(!missing(left)) left
  opts$right <- if(!missing(right)) right

  p$x$config$margin <- append(p$x$config$margin, opts)

  p
}

#' Customise chart frame
#'
#' Customise the chart frame.
#'
#' @export
uv_frame <- function(p, bgcolor){

  opts <- list()
  opts$bgcolor <- if(!missing(bgcolor)) bgcolor

  p$x$config$frame <- append(p$x$config$frame, opts)

  p
}

#' Customise chart axis
#'
#' Customise the chart axis
#'
#' @export
uv_axis <- function(p, opacity, ticks, subticks, padding, minor, strokecolor, fontfamily,
                     fontsize, fontweight, showticks, showsubticks, showtext){

  opts <- list()
  opts$opacity <- if(!missing(opacity)) opacity
  opts$ticks <- if(!missing(ticks)) ticks
  opts$subticks <- if(!missing(subticks)) subticks
  opts$padding <- if(!missing(padding)) padding
  opts$minor <- if(!missing(minor)) minor
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$showticks <- if(!missing(showticks)) showticks
  opts$showsubticks <- if(!missing(showsubticks)) showsubticks
  opts$showtext <- if(!missing(showtext)) showtext

  p$x$config$axis <- append(p$x$config$axis, opts)

  p
}
