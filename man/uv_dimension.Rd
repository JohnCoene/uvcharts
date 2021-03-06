% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{uv_dimension}
\alias{uv_dimension}
\title{Customise chart dimensions}
\usage{
uv_dimension(p, width, height = 200, override = FALSE)
}
\arguments{
\item{p}{a \code{uvcharts} object.}

\item{width, height}{any CSS size.}

\item{override}{if \code{TRUE}, overrides the htmlwidgets div's height and width.}
}
\description{
Customise the chart dimensions.
}
\examples{
mtcars \%>\%
  uv_charts(qsec) \%>\%
  uv_add(mpg) \%>\%
  uv_dimension(width = 200)

}
\seealso{
\href{http://imaginea.github.io/uvCharts/documentation.html}{Official docs}
}

