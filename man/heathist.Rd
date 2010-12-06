\name{heathist}
\alias{heathist}
\alias{demo.heathist}
\title{Heat histogram}
\description{heat histogram with an additional color stripe based on a kernel density estimate}
\usage{
heathist(x, breaks = 20, main = "Heat-histogram", colpal = "greys", nobox = FALSE, revpal = FALSE, add.density = FALSE, col.density = "darkred", add.rug = TRUE, col.rug = "black", ncolors = 100, ...)
}
\arguments{
  \item{x}{\code{vector}}
  \item{breaks}{\code{breaks of the histogram}}
  \item{main}{\code{title of the plot}}
  \item{colpal}{\code{which colorpalettes should be chosen ( see disco() )}}
  \item{nobox}{\code{should the box be plotted}}
  \item{revpal}{\code{reverse the colorpalette}}
  \item{add.density}{\code{add density line}}
  \item{col.density}{\code{color of the density line}}
  \item{add.rug}{\code{add a rug (1-d plot of the data) below the histogram-bars}}
  \item{col.rug}{\code{color of the rug}}
  \item{ncolors}{\code{number of colors to use}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb, Romain Francois}
\seealso{\code{\link{heatbarplot}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.heathist()}
\keyword{hplot}
