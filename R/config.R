#' Configure the chart
#'
#' Configure the chart.
#'
#' @param p a \code{uvcharts} object.
#' @param ... any argument to pass to \href{http://imaginea.github.io/uvCharts/documentation.html}{config}.
#'
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_config <- function(p, ...){

  opts <- list(...)

  p$x$config <- append(p$x$config, opts)

  p
}

#' Customise chart dimensions
#'
#' Customise the chart dimensions.
#'
#' @param p a \code{uvcharts} object.
#' @param width,height any CSS size.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
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
#' @param p a \code{uvcharts} object.
#' @param top,bottom,left,right any CSS size.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
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
#' @param p a \code{uvcharts} object.
#' @param bgcolor any Color Code or Color String or \code{none}.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
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
#' Customise the chart axis.
#'
#' @param p a \code{uvcharts} object.
#' @param opacity opacity of the axis lines drawn for reference, defaults to \code{1}.
#' @param ticks Number of major ticks on the measure axis.
#' @param subticks Number of minor ticks on the measure axis.
#' @param padding any CSS size.
#' @param minor length of the minor ticks towards the label.
#' @param strokecolor color of axis lines, any Color Code or Color String or \code{none}. defaults to \code{#000000}.
#' @param fontfamily any font, defaults to \code{Arial}.
#' @param fontsize font size. Defaults to \code{11}.
#' @param fontweight font weight, defaults to \code{bold}.
#' @param showticks show ticks flag, \code{TRUE} or \code{FALSE}, defaults to the former.
#' @param showsubticks show subticks flag, \code{TRUE} or \code{FALSE}, defaults to the former.
#' @param showtext show text flag, \code{TRUE} or \code{FALSE}, defaults to the former.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
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

#' Customise chart labels
#'
#' Customise the chart labels.
#'
#' @param p a \code{uvcharts} object.
#' @param strokecolor any Color Code or Color String or \code{none}. defaults to \code{#000000}.
#' @param fontfamily any font, defaults to \code{Arial}.
#' @param fontsize font size. Defaults to \code{11}.
#' @param fontweight font weight, defaults to \code{bold}.
#' @param showlabel show labels flag, \code{TRUE} or \code{FALSE}, defaults to the former.
#' @param precision precision on the labels in case of decimal values. Defaults to \code{2}.
#' @param prefix any prefix which needs to be added to the label.
#' @param suffix any suffix which needs to be added to the label.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_label <- function(p, fontfamily, fontsize, fontweight, strokecolor, showlabel, precision,
                    prefix, suffix){

  opts <- list()
  opts$precision <- if(!missing(precision)) precision
  opts$prefix <- if(!missing(prefix)) prefix
  opts$suffix <- if(!missing(suffix)) suffix
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$showlabel <- if(!missing(showlabel)) showlabel

  p$x$config$label <- append(p$x$config$label, opts)

  p
}

#' Customise chart scale
#'
#' Customise the chart scale.
#'
#' @param p a \code{uvcharts} object.
#' @param type scale used to represent values on the chart, see details for valid values.
#' @param ordinality defines the gap between 2 consecutive labels, defaults to \code{0.2}.
#'
#' @details
#' Valid values for \code{type} argument, defaults to \code{linear}:
#'
#' \itemize{
#'   \item{\code{linear}}
#'   \item{\code{log}}
#'   \item{\code{pow}}
#'   \item{\code{sqrt}}
#' }
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_scale <- function(p, type, ordinality){

  opts <- list()
  opts$type <- if(!missing(type)) type
  opts$ordinality <- if(!missing(ordinality)) ordinality

  p$x$config$scale <- append(p$x$config$scale, opts)

  p
}

#' Customise chart tooltip
#'
#' Customise the chart tooltip.
#'
#' @param p a \code{uvcharts} object.
#' @param show flag to enable or disable tooltip functionality, defaults to \code{TRUE}.
#' @param format defines custom format for tooltip, character string. defaults to \code{\%c [\%l] : \%v}
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_tooltip <- function(p, show, format){

  opts <- list()
  opts$show <- if(!missing(show)) show
  opts$format <- if(!missing(format)) format

  p$x$config$tooltip <- append(p$x$config$tooltip, opts)

  p
}

#' Customise chart caption
#'
#' Customise the chart caption.
#'
#' @param p a \code{uvcharts} object.
#' @param strokecolor any Color Code or Color String or \code{none}. defaults to \code{#000000}.
#' @param fontfamily any font, defaults to \code{Arial}.
#' @param fontsize font size. Defaults to \code{11}.
#' @param fontweight font weight, defaults to \code{bold}.
#' @param textdecoration text decoration applied on the caption, defaults to \code{none}.
#' @param cursor precision on the labels in case of decimal values. Defaults to \code{pointer}.
#'
#' @rdname uv_caption
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_caption <- function(p, fontfamily, fontsize, fontweight, textdecoration, strokecolor,
                       cursor){

  opts <- list()
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$textdecoration <- if(!missing(textdecoration)) textdecoration
  opts$cursor <- if(!missing(cursor)) cursor

  p$x$config$caption <- append(p$x$config$caption, opts)

  p
}

#' Customise chart subcaption
#'
#' Customise the chart subcaption.
#'
#' @inheritParams uv_caption
#' @rdname uv_caption
#'
#' @export
uv_subcaption <- function(p, fontfamily, fontsize, fontweight, textdecoration, strokecolor,
                       cursor){

  opts <- list()
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$textdecoration <- if(!missing(textdecoration)) textdecoration
  opts$cursor <- if(!missing(cursor)) cursor

  p$x$config$subcaption <- append(p$x$config$subcaption, opts)

  p
}

#' Customise chart meta properties
#'
#' Customise the chart meta properties.
#'
#' @param p a \code{uvcharts} object.
#' @param caption caption for the graph.
#' @param subcaption subcaption of the graph.
#' @param hlabel label for the horizontal axis.
#' @param vlabel label for the vertical axis.
#' @param hsulabel sub-label for the horizontal axis.
#' @param vsulabel sub-label for the vertical axis.
#' @param isDownloadable whether or not to display the download link for the chart, defaults to \code{FALSE}.
#' @param downloadLabel text to be displayed in the download link, defaults to \code{Download}.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_meta <- function(p, caption, subcaption, hlabel, vlabel, hsulabel, vsulabel, isDownloadable,
                          downloadLabel){

  opts <- list()
  p$x$config$meta$caption <- if(!missing(caption)) caption
  opts$subcaption <- if(!missing(subcaption)) subcaption
  opts$hlabel <- if(!missing(hlabel)) hlabel
  opts$vlabel <- if(!missing(vlabel)) vlabel
  opts$hsulabel <- if(!missing(hsulabel)) hsulabel
  opts$vsulabel <- if(!missing(vsulabel)) vsulabel
  opts$isDownloadable <- if(!missing(isDownloadable)) isDownloadable
  opts$downloadLabel <- if(!missing(downloadLabel)) downloadLabel

  p$x$config$meta <- append(p$x$config$meta, opts)

  p
}

#' Customise chart caption
#'
#' Customise the chart caption.
#'
#' @param p a \code{uvcharts} object.
#' @param palette palette name, see details for valid values. Defaults to \code{Brink}.
#' @param bgcolor background color used for the entire chart include the margin spaces.
#' @param orientation orientation of the chart for a different layout based on requirements. Can take
#' \code{Horizontal} or \code{Vertical}, defaults to the former.
#' @param custom.palette override the palette with your own color palette by providing a non empty array
#' of color codes. They are used in order for corresponding category in the categories array of the graph
#' definition. Pass colors as \code{list}.
#' @param opacity opacity of the entire chart to make it blend into any background of your choice. Defaults to \code{1}.
#'
#' @details
#' Valid values for \code{palette} argument.
#'
#' \itemize{
#'   \item{\code{Plain}}
#'   \item{\code{Simple}}
#'   \item{\code{Olive}}
#'   \item{\code{Candid}}
#'   \item{\code{Sulphide}}
#'   \item{\code{Soft}}
#'   \item{\code{Lint}}
#' }
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_graph <- function(p, palette, bgcolor, orientation, custom.palette, opacity){

  opts <- list()
  opts$palette <- if(!missing(palette)) palette
  opts$bgcolor <- if(!missing(bgcolor)) bgcolor
  opts$orientation <- if(!missing(orientation)) orientation
  opts$custompalette <- if(!missing(custom.palette)) custom.palette
  opts$opacity <- if(!missing(opacity)) opacity

  p$x$config$graph <- append(p$x$config$graph, opts)

  p
}


#' Customise chart bar chart
#'
#' Customise the chart bars.
#'
#' @param p a \code{uvcharts} object.
#' @param strokecolor any Color Code or Color String or \code{none}. defaults to \code{#000000}.
#' @param fontfamily any font, defaults to \code{Arial}.
#' @param fontsize font size. Defaults to \code{11}.
#' @param fontweight font weight, defaults to \code{bold}.
#' @param textcolor text color used for the numbers, defaults to \code{#000000}.
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_baropt <- function(p, strokecolor, fontsize, fontweight, fontfamily, textcolor){

  opts <- list()
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$textcolor <- if(!missing(textcolor)) textcolor

  p$x$config$bar <- append(p$x$config$bar, opts)

  p
}

#' Customise chart lines
#'
#' Customise the properties affecting Bar, Stacked Bar and Step Up Bar Chart.
#'
#' @param p a \code{uvcharts} object.
#' @param interpolation line interpolation. Defaults to \code{linear} see details for valid values.
#'
#' @details
#' Valid values for \code{interpolation} argument:
#' \itemize{
#'   \item{\code{linear}}
#'   \item{\code{basis}}
#'   \item{\code{cardinal}}
#'   \item{\code{monotone}}
#' }
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_lineopt <- function(p, interpolation){

  opts <- list()
  opts$interpolation <- if(!missing(interpolation)) interpolation

  p$x$config$line <- append(p$x$config$line, opts)

  p
}

#' Customise chart lines
#'
#' Customise the properties specific to line chart.
#'
#' @param p a \code{uvcharts} object.
#' @param interpolation line interpolation. Defaults to \code{linear} see details for valid values.
#' @param opacity oOpacity in the case of simple Area chart, defaults to \code{0.2}.
#' @param offset offset, defaults to \code{zero}. See details for valid values.
#'
#' @details
#' Valid values for \code{interpolation} argument:
#' \itemize{
#'   \item{\code{linear}}
#'   \item{\code{basis}}
#'   \item{\code{cardinal}}
#'   \item{\code{monotone}}
#' }
#'
#' Valid values for \code{offset} argument:
#' \itemize{
#'   \item{\code{zero}}
#'   \item{\code{wiggle}}
#'   \item{\code{silhouette}}
#'   \item{\code{expand}}
#' }
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_areaeopt <- function(p, interpolation, opacity, offset){

  opts <- list()
  opts$interpolation <- if(!missing(interpolation)) interpolation
  opts$opacity <- if(!missing(opacity)) opacity
  opts$offset <- if(!missing(offset)) offset

  p$x$config$area <- append(p$x$config$area, opts)

  p
}

#' Customise chart lines
#'
#' Customise the properties specific to Pie Chart.
#'
#' @param p a \code{uvcharts} object.
#' @param strokecolor any Color Code or Color String or \code{none}. defaults to \code{#000000}.
#' @param fontfamily any font, defaults to \code{Arial}.
#' @param fontsize font size. Defaults to \code{11}.
#' @param fontweight font weight, defaults to \code{bold}.
#' @param fontvariant font variant used on the text, defaults to \code{small-caps}, can also take \code{normal}.
#' @param fontfill text color on the measure, defaults to \code{#000000}.
#' @param strokewidth stroke width on each arc. Defaults to \code{1}.
#' @param factor radius of the cropped part in terms of the bigger pie. Defaults to \code{0.4}.
#'
#' @rdname uv_pie_opt
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_pieopt <- function(p, fontfamily, fontsize, fontweight, fontvariant, fontfill,
                        strokecolor, strokewidth){

  opts <- list()
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$fontvariant <- if(!missing(fontvariant)) fontvariant
  opts$fontfill <- if(!missing(fontfill)) fontfill
  opts$strokewidth <- if(!missing(strokewidth)) strokewidth

  p$x$config$pie <- append(p$x$config$pie, opts)

  p
}

#' Customise chart lines
#'
#' Customise the properties specific to Donut Chart.
#'
#' @inheritParams uv_pie_opt
#' @rdname uv_pie_opt
#'
#' @seealso \href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
#'
#' @export
uv_donutopt <- function(p, fontfamily, fontsize, fontweight, fontvariant, fontfill,
                        strokecolor, strokewidth, factor){

  opts <- list()
  opts$strokecolor <- if(!missing(strokecolor)) strokecolor
  opts$fontfamily <- if(!missing(fontfamily)) fontfamily
  opts$fontsize <- if(!missing(fontsize)) fontsize
  opts$fontweight <- if(!missing(fontweight)) fontweight
  opts$fontvariant <- if(!missing(fontvariant)) fontvariant
  opts$fontfill <- if(!missing(fontfill)) fontfill
  opts$strokewidth <- if(!missing(strokewidth)) strokewidth
  opts$factor <- if(!missing(factor)) factor

  p$x$config$donut <- append(p$x$config$donut, opts)

  p
}
