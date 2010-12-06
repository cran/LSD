\name{makemovie}
\alias{makemovie}
\alias{demo.makemovie}
\title{Make movie}
\description{Interpolate rows of a matrix to make a fluent animation}
\usage{
makemovie(mat, timepoints = NULL, timestep = 1, motionline = NULL)
}
\arguments{
  \item{mat}{\code{matrix}}
  \item{timepoints}{\code{the timepoints of the rows of the matrix}}
  \item{timestep}{\code{the timesteps between the existing timepoints}}
  \item{motionline}{\code{results from timepoints and timesteps by default}}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{plotmatrix}},\code{\link{LSD.demo.tour}}}
\examples{demo.makemovie()}
\keyword{aplot}
