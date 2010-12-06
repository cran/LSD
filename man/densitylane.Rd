\name{densitylane}
\alias{densitylane}
\title{Densitylane}
\description{color stripe based on a kernel density estimate}
\usage{
densitylane(x, y, pos = 1, width = 0.4, colpal = "standard", horizontal = horizontal, alpha = NULL, ncol = 75)
}
\arguments{
  \item{x}{\code{the density$x values of a density object}}
  \item{y}{\code{the density$y values of a density object}}
  \item{pos}{\code{position of the lane}}
  \item{width}{\code{width of the lane}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{horizontal}{\code{should the density line be plotted horizontal}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{ncol}{\code{the number of colors to use}}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatboxplot}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatboxplot()}
\keyword{aplot}
