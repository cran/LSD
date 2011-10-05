\name{plotmatrix}
\alias{plotmatrix}
\alias{demo.plotmatrix}
\title{Plot a matrix}
\description{Plot the rows of a matrix in colored lines.}
\usage{
plotmatrix(mat, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, main = NULL, type = "l", lwd = 2, at = NULL, xlabels = NULL, ltys = NULL, add = FALSE,cols = NULL, ...)
}
\arguments{
  \item{mat}{\code{matrix}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{xlabs, standard graphics parameter}}
  \item{ylab}{\code{ylabs, standard graphics parameter}}
  \item{main}{\code{title of the plot}}
  \item{type}{\code{type, standard plotting parameter}}
  \item{lwd}{\code{lwd, standard plotting parameter}}
  \item{at}{\code{at which x-positions the columns should occur}}
  \item{xlabels}{\code{text added as x-axis labels}}
  \item{ltys}{\code{vector of linetypes}}
  \item{cols}{\code{vector of colors}}
  \item{add}{\code{add to existing plot}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{makemovie}},\code{\link{LSD.demo.tour}}}
\examples{demo.plotmatrix()}
\keyword{hplot}
