\name{plotmatrix}
\alias{plotmatrix}
\alias{demo.plotmatrix}
\title{Plot a matrix}
\description{Plot the rows of a matrix in colored lines.}
\usage{
plotmatrix(mat, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, main = NULL, type = "l", lwd = 2, at = NULL, xlabels = NULL, ltys = NULL, add = FALSE,cols = NULL, ...)
}
\arguments{
  \item{mat}{matrix}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{xlab}{xlabs, standard graphics parameter}
  \item{ylab}{ylabs, standard graphics parameter}
  \item{main}{title of the plot}
  \item{type}{type, standard plotting parameter}
  \item{lwd}{lwd, standard plotting parameter}
  \item{at}{at which x-positions the columns should occur}
  \item{xlabels}{text added as x-axis labels}
  \item{ltys}{vector of linetypes}
  \item{cols}{vector of colors}
  \item{add}{add to existing plot}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{makemovie}},\code{\link{LSD.demo.tour}}}
\examples{demo.plotmatrix()}
\keyword{hplot}
