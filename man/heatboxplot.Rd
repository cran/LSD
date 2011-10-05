\name{heatboxplot}
\alias{heatboxplot}
\alias{demo.heatboxplot}
\title{Heatboxplot}
\description{A boxplot with an additional color stripe based on a kernel density estimate}
\usage{
heatboxplot(x, horizontal = FALSE, add = FALSE, colpal = "standard", colpals = NULL, ncol = 75, lwd = 1.75, axes = TRUE, labels = NULL, alpha = NULL, xlim = NULL, ylim = NULL, xlab = NULL, ylab = "data values", main = "Heatboxplot", nolab = FALSE, outline = TRUE, boxonly = FALSE, adjust = 1, quant.from = 0.25, quant.to = 0.75, range = 1.5, border = "black", plot.boxplot = TRUE, add.quartiles = TRUE, add.box = FALSE, n.density = 1024, cexbox = 0.6, ...)
}
\arguments{
  \item{x}{data as vector, matrix, list or data.frame}
  \item{horizontal}{should all be plotted horizontal}
  \item{add}{should the boxplot be added to an existing plot}
  \item{colpal}{which colorpalette should be chosen ( see disco() ), only if colpals = NULL}
  \item{colpals}{which colorpalettes should be chosen ( see disco() )}
  \item{ncol}{the number of colors to use}
  \item{lwd}{linewidth to be used}
  \item{axes}{should the axes be plotted}
  \item{labels}{labels of the boxplots}
  \item{alpha}{alpha value for color opacity}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{xlab}{xlab, standard graphics parameter}
  \item{ylab}{ylab, standard graphics parameter}
  \item{main}{title of the plot}
  \item{nolab}{should title and ylab be suppressed}
  \item{outline}{should outliers be plotted}
  \item{boxonly}{should the density only be plotted in the box}
  \item{adjust}{scale the used bandwidth}
  \item{quant.from}{from which quantile should the density lane be plotted}
  \item{quant.to}{to which quantile should the density lane be plotted}
  \item{range}{this determines how far the plot whiskers extend out from the box}
  \item{border}{color of the box and whiskers}
  \item{plot.boxplot}{should boxplot be added}
  \item{add.quartiles}{box of the boxplot, if plot.boxplot = FALSE}
  \item{add.box}{box of the plot}
  \item{n.density}{the number of equally spaced points at which the density is to be estimated}
  \item{cexbox}{cex of the boxes}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{cplot}},\code{\link{densitylane}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatboxplot()}
\keyword{hplot}
