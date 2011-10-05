\name{ellipsescatter}
\alias{ellipsescatter}
\alias{demo.ellipsescatter}
\title{Ellipsescatter}
\description{Scatterplot with the ability to color certain groups of points and draw ellipses assuming gaussian distributions.}
\usage{
ellipsescatter(x, y, groups, colors = NULL, bgcol = "darkgrey", scalesd = 1, level = 0.75, legend.cex = 1.5, location = "topright", ...)
}
\arguments{
  \item{x}{x-vector}
  \item{y}{y-vector}
  \item{groups}{list of vectors to plot as groups}
  \item{colors}{colors corresponding to groups}
  \item{bgcol}{color of points}
  \item{scalesd}{scale parameter for standard deviations}
  \item{level}{the confidence level of a pairwise confidence region}
  \item{legend.cex}{cex of the added legend}
  \item{location}{location of the added legend}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{LSD.demo.tour}}}
\examples{demo.ellipsescatter()}
\keyword{hplot}
