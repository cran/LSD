\name{makemovie}
\alias{makemovie}
\alias{demo.makemovie}
\title{Make movie}
\description{Interpolate rows of a matrix to make a fluent animation}
\usage{
makemovie(mat, timepoints = NULL, timestep = 1, motionline = NULL)
}
\arguments{
  \item{mat}{matrix}
  \item{timepoints}{the timepoints of the rows of the matrix}
  \item{timestep}{the timesteps between the existing timepoints}
  \item{motionline}{results from timepoints and timesteps by default}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{plotmatrix}},\code{\link{LSD.demo.tour}}}
\examples{demo.makemovie()}
\keyword{aplot}
