\name{clusterplot}
\alias{clusterplot}
\alias{demo.clusterplot}
\title{Clusterplot}
\description{Progression plot for two-dimensional data in several clusters, briefly a boxplot over time}
\usage{
clusterplot(clusmat, label = NULL, at = NULL, main = "Clusterplot", xlim = NULL, ylim = NULL, xlab = "Columns", ylab = "Rows", xaxt = "s", xlabels = NULL, las = 1, fromto = c(0.05, 0.95), colpal = "standardheat", nrcolors = 25, outer.col = "light grey", quartiles.col = c("black", "grey", "grey"), add.quartiles = TRUE, separate = TRUE, revpal = FALSE, size = TRUE, alpha = NULL, smooth = FALSE,	df = 3, ...)
}
\arguments{
  \item{clusmat}{matrix or list with numerical entries, quantiles of rows resp. vectors will define colors}
  \item{label}{if multiple clusters should be plotted: vector with labels, which assigns rows to clusters}
  \item{at}{at which x-positions the columns should occur}
  \item{main}{the title(s) of the plot(s)}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{xlab}{x-axis legend, standard graphics parameter}
  \item{ylab}{y-axis legend, standard graphics parameter}
  \item{xaxt}{should an x axis be plotted at all? (="n" if not)}
  \item{xlabels}{text added as x-axis labels}
  \item{las}{\code{las=1}: horizontal text, \code{las=2}: vertical text (x-axis labels)}
  \item{fromto}{the range of quantiles that should be plotted}
  \item{colpal}{colorpalettes assigned to sorted labels \code{label} ( see disco() )}
  \item{nrcolors}{the number of colors to use}
  \item{outer.col}{color of the outlier lines}
  \item{quartiles.col}{color of quartile lines}
  \item{add.quartiles}{should the quartile lines be plotted}
  \item{separate}{should clusters be plotted in subsequent figures}
  \item{revpal}{should the colorpalette be inverted}
  \item{size}{should the size of each cluster be added to the title}
  \item{smooth}{should quantiles be smoothed}
  \item{df}{degrees of freedom}
  \item{alpha}{alpha value for color opacity}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{singleclusterplot}},\code{\link{colorpalette}},\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.clusterplot()}
\keyword{hplot}
