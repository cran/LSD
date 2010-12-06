\name{singleclusterplot}
\alias{singleclusterplot}
\title{Single clusterplot}
\description{Progression plot for two-dimensional data to be used by clusterplot.}
\usage{
singleclusterplot(clusmat, at = NULL, fromto = c(0.05, 0.95), colpal = "standardheat", nrcolors = 25, outer.col = "light grey", revpal = FALSE, alpha = NULL, quartiles.col = c("black", "grey", "grey"), add.quartiles = TRUE)
}
\arguments{
  \item{clusmat}{\code{matrix with numerical entries, quantiles of rows will define colors}}
  \item{at}{\code{at which x-positions the columns should occur}}
  \item{fromto}{\code{the range of quantiles that should be plotted}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{nrcolors}{\code{the number of colors to use}}
  \item{outer.col}{\code{color of the outlier lines}}
  \item{revpal}{\code{should the colorpalette be inverted}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{quartiles.col}{\code{color of quartile lines}}
  \item{add.quartiles}{\code{should the quartile lines be plotted}}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{clusterplot}},\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.clusterplot()}
\keyword{aplot}
