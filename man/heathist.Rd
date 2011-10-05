\name{heathist}
\alias{heathist}
\alias{demo.heathist}
\title{Heat histogram}
\description{heat histogram with an additional color stripe based on a kernel density estimate}
\usage{
heathist(x, breaks = 20, main = "Heat-histogram", colpal = "greys", nobox = FALSE, revpal = FALSE, add.density = FALSE, col.density = "darkred", add.rug = TRUE, col.rug = "black", ncolors = 100, ...)
}
\arguments{
  \item{x}{vector}
  \item{breaks}{breaks of the histogram}
  \item{main}{title of the plot}
  \item{colpal}{which colorpalettes should be chosen ( see disco() )}
  \item{nobox}{should the box be plotted}
  \item{revpal}{reverse the colorpalette}
  \item{add.density}{add density line}
  \item{col.density}{color of the density line}
  \item{add.rug}{add a rug (1-d plot of the data) below the histogram-bars}
  \item{col.rug}{color of the rug}
  \item{ncolors}{number of colors to use}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb, Romain Francois}
\seealso{\code{\link{heatbarplot}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.heathist()}
\keyword{hplot}
