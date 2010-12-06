\name{singlemsdplot}
\alias{singlemsdplot}
\title{Single Msdplot}
\description{Progression plot for two-dimensional data to be used by msdplot.}
\usage{
singlemsdplot(x, col = NULL, alpha = 50, bars = TRUE, at = NULL)
}
\arguments{
  \item{x}{\code{data as matrix}}
  \item{col}{\code{colors}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{bars}{\code{error bars}}
  \item{at}{\code{at which x-positions the columns should occur}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{msdplot}},\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.msdplot()}
\keyword{aplot}
