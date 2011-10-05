\name{LSDpie}
\alias{LSDpie}
\alias{demo.LSDpie}
\title{LSD Pie}
\description{Piecharts at arbitrary position and radii.}
\usage{
LSDpie(props, x = 0, y = 0, radius = 1, cols = NULL, colpal = "pies", edges = 100, add = FALSE, xlim = NULL, ylim = NULL, main = "Piecharts", alpha = NULL, ...)
}
\arguments{
  \item{props}{\code{vector giving the relations of the pie pieces (need not to be normed)}}
  \item{x}{\code{x-position of the piechart}}
  \item{y}{\code{y-position of the piechart}}
  \item{radius}{\code{radius of the piechart}}
  \item{cols}{\code{vector of colors}}
  \item{colpal}{\code{colorpalette if cols is not specified}}
  \item{edges}{\code{number of edges the "circle" will have}}
  \item{add}{\code{should piechart be added to an existing plot}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{main}{\code{title of the plot}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.LSDpie()}
\keyword{hplot}
