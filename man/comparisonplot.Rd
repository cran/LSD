\name{comparisonplot}
\alias{comparisonplot}
\alias{demo.comparisonplot}
\alias{cplot}
\alias{demo.cplot}
\title{Comparisonplot}
\description{function to compare two vectors extensively}
\usage{
comparisonplot(x, y, histbreaks = 30, adjust = 1, colpal = "heat", main = NULL, xlim = NULL, ylim = NULL, ab = FALSE, add.density = FALSE, col.density = "darkred", pimp = FALSE, ...)

cplot(x, y, histbreaks = 30, adjust = 1, colpal = "heat", main = NULL, xlim = NULL, ylim = NULL, ab = FALSE, add.density = FALSE, col.density = "darkred", pimp = FALSE, ...)
}
\arguments{
  \item{x}{\code{x-vector}}
  \item{y}{\code{y-vector}}
  \item{histbreaks}{\code{breaks of the histograms}}
  \item{adjust}{\code{scale the used bandwidth ( see demo.heatboxplot() )}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{main}{\code{maintitle of the plot}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{ab}{\code{should an abline be added to the plot}}
  \item{add.density}{\code{add density line}}
  \item{col.density}{\code{color of the density line}}
  \item{pimp}{\code{PIMP MY PLOT}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{heatscatter}},\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.cplot()}
\keyword{hplot}
