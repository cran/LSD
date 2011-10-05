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
  \item{x}{x-vector}
  \item{y}{y-vector}
  \item{histbreaks}{breaks of the histograms}
  \item{adjust}{scale the used bandwidth ( see demo.heatboxplot() )}
  \item{colpal}{which colorpalette should be chosen ( see disco() )}
  \item{main}{maintitle of the plot}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{ab}{should an abline be added to the plot}
  \item{add.density}{add density line}
  \item{col.density}{color of the density line}
  \item{pimp}{PIMP MY PLOT}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{heatscatter}},\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.cplot()}
\keyword{hplot}
