\name{kde2dplot}
\alias{kde2dplot}
\title{Kde2d plot}
\description{Plot displays the estimation of the density of a 2d sample. The density itself is computed with kde2d in the package MASS.}
\usage{
kde2dplot(x, y, grid = 100, ncol = 30, nlevels = 10, main = "kde2d")
}
\arguments{
  \item{x}{x-vector}
  \item{y}{y-vector}
  \item{grid}{size of the grid ( see kde2d )}
  \item{ncol}{the number of colors to use}
  \item{nlevels}{number of levels}
  \item{main}{title of the plot}
}
\author{Romain Francois}
\seealso{\code{\link{heatscatter}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatscatter()}
\keyword{hplot}
