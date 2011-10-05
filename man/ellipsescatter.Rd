\name{ellipsescatter}
\alias{ellipsescatter}
\alias{demo.ellipsescatter}
\title{Ellipsescatter}
\description{Scatterplot with the ability to color certain groups of points and draw ellipses assuming gaussian distributions.}
\usage{
ellipsescatter(x, y, groups, colors = NULL, bgcol = "darkgrey", scalesd = 1, level = 0.75, location = "topright", ...)
}
\arguments{
  \item{x}{\code{x-vector}}
  \item{y}{\code{y-vector}}
  \item{groups}{\code{list of vectors to plot as groups}}
  \item{colors}{\code{colors corresponding to groups}}
  \item{bgcol}{\code{color of points}}
  \item{scalesd}{\code{scale parameter for standard deviations}}
  \item{level}{\code{the confidence level of a pairwise confidence region}}
  \item{location}{\code{location of the added legend}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{LSD.demo.tour}}}
\examples{demo.ellipsescatter()}
\keyword{hplot}
