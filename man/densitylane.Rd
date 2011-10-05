\name{densitylane}
\alias{densitylane}
\title{Densitylane}
\description{color stripe based on a kernel density estimate}
\usage{
densitylane(x, y, pos = 1, width = 0.4, colpal = "standard", horizontal = horizontal, alpha = NULL, ncol = 75)
}
\arguments{
  \item{x}{the density$x values of a density object}
  \item{y}{the density$y values of a density object}
  \item{pos}{position of the lane}
  \item{width}{width of the lane}
  \item{colpal}{which colorpalette should be chosen ( see disco() )}
  \item{horizontal}{should the density line be plotted horizontal}
  \item{alpha}{alpha value for color opacity}
  \item{ncol}{the number of colors to use}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatboxplot}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatboxplot()}
\keyword{aplot}
