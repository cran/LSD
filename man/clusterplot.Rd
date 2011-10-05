\name{clusterplot}
\alias{clusterplot}
\alias{demo.clusterplot}
\title{Clusterplot}
\description{Progression plot for two-dimensional data in several clusters, briefly a boxplot over time}
\usage{
clusterplot(clusmat, label = NULL, at = NULL, main = "Clusterplot", xlim = NULL, ylim = NULL, xlab = "Columns", ylab = "Rows", xaxt = "s", xlabels = NULL, las = 1, fromto = c(0.05, 0.95), colpal = "standardheat", nrcolors = 25, outer.col = "light grey", quartiles.col = c("black", "grey", "grey"), add.quartiles = TRUE, separate = TRUE, revpal = FALSE, size = TRUE, alpha = NULL, smooth = FALSE,	df = 3, ...)
}
\arguments{
  \item{clusmat}{\code{matrix or list with numerical entries, quantiles of rows resp. vectors will define colors}}
  \item{label}{\code{if multiple clusters should be plotted: vector with labels, which assigns rows to clusters}}
  \item{at}{\code{at which x-positions the columns should occur}}
  \item{main}{\code{the title(s) of the plot(s)}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{x-axis legend, standard graphics parameter}}
  \item{ylab}{\code{y-axis legend, standard graphics parameter}}
  \item{xaxt}{\code{should an x axis be plotted at all? (="n" if not)}}
  \item{xlabels}{\code{text added as x-axis labels}}
  \item{las}{\code{las=1: horizontal text, las=2: vertical text (x-axis labels)}}
  \item{fromto}{\code{the range of quantiles that should be plotted}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{nrcolors}{\code{the number of colors to use}}
  \item{outer.col}{\code{color of the outlier lines}}
  \item{quartiles.col}{\code{color of quartile lines}}
  \item{add.quartiles}{\code{should the quartile lines be plotted}}
  \item{separate}{\code{should clusters be plotted in subsequent figures}}
  \item{revpal}{\code{should the colorpalette be inverted}}
  \item{size}{\code{should the size of each cluster be added to the title}}
  \item{smooth}{\code{should quantiles be smoothed}}
  \item{df}{\code{degrees of freedom}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{singleclusterplot}},\code{\link{colorpalette}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.clusterplot()}
\keyword{hplot}
