\name{heatboxplot}
\alias{heatboxplot}
\alias{demo.heatboxplot}
\title{Heatboxplot}
\description{A boxplot with an additional color stripe based on a kernel density estimate}
\usage{
heatboxplot(x, horizontal = FALSE, add = FALSE, colpal = "standard", colpals = NULL, ncol = 75, lwd = 1.75, axes = TRUE, labels = NULL, alpha = NULL, xlim = NULL, ylim = NULL, xlab = NULL, ylab = "data values", main = "Heatboxplot", nolab = FALSE, outline = TRUE, boxonly = FALSE, adjust = 1, quant.from = 0.25, quant.to = 0.75, range = 1.5, border = "black", plot.boxplot = TRUE, add.quartiles = TRUE, add.box = FALSE, n.density = 1024, cexbox = 0.6, ...)
}
\arguments{
  \item{x}{\code{data as vector, matrix, list or data.frame}}
  \item{horizontal}{\code{should all be plotted horizontal}}
  \item{add}{\code{should the boxplot be added to an existing plot}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() ), only if colpals = NULL}}
  \item{colpals}{\code{which colorpalettes should be chosen ( see disco() )}}
  \item{ncol}{\code{the number of colors to use}}
  \item{lwd}{\code{linewidth to be used}}
  \item{axes}{\code{should the axes be plotted}}
  \item{labels}{\code{labels of the boxplots}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{xlab, standard graphics parameter}}
  \item{ylab}{\code{ylab, standard graphics parameter}}
  \item{main}{\code{title of the plot}}
  \item{nolab}{\code{should title and ylab be suppressed}}
  \item{outline}{\code{should outliers be plotted}}
  \item{boxonly}{\code{should the density only be plotted in the box}}
  \item{adjust}{\code{scale the used bandwidth}}
  \item{quant.from}{\code{from which quantile should the density lane be plotted}}
  \item{quant.to}{\code{to which quantile should the density lane be plotted}}
  \item{range}{\code{this determines how far the plot whiskers extend out from the box}}
  \item{border}{\code{color of the box and whiskers}}
  \item{plot.boxplot}{\code{should boxplot be added}}
  \item{add.quartiles}{\code{box of the boxplot, if plot.boxplot = FALSE}}
  \item{add.box}{\code{box of the plot}}
  \item{n.density}{\code{the number of equally spaced points at which the density is to be estimated}}
  \item{cexbox}{\code{cex of the boxes}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{cplot}},\code{\link{densitylane}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatboxplot()}
\keyword{hplot}
