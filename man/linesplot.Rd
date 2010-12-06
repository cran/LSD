\name{linesplot}
\alias{lplot}
\alias{demo.lplot}
\alias{linesplot}
\alias{demo.linesplot}
\title{Linesplot}
\description{One-dimensional scatterplot with the option to add a boxplot}
\usage{
lplot(x, labels = NULL, col = "black", cols = NULL, alpha = 25, xlim = NULL, ylim = NULL, xlab = NULL, ylab = "data values", las = 1, outline = TRUE, cexbox = 0.6, addboxes = FALSE, border = "black", range = 1.5, lwd = 1.5, main = "Linesplot", ...)

linesplot(x, labels = NULL, col = "black", cols = NULL, alpha = 25, xlim = NULL, ylim = NULL, xlab = NULL, ylab = "data values", las = 1, outline = TRUE, cexbox = 0.6, addboxes = FALSE, border = "black", range = 1.5, lwd = 1.5, main = "Linesplot", ...)
}
\arguments{
  \item{x}{\code{data as vector, matrix, list or data.frame}}
  \item{labels}{\code{vector of labels}}
  \item{col}{\code{standard color}}
  \item{cols}{\code{vector of colors}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{xlab, standard graphics parameter}}
  \item{ylab}{\code{ylab, standard graphics parameter}}
  \item{las}{\code{las=1: horizontal text, las=2: vertical text (x-axis labels)}}
  \item{outline}{\code{should outliers be plotted}}
  \item{cexbox}{\code{cex of the boxes}}
  \item{addboxes}{\code{should boxplots be added}}
  \item{border}{\code{color of the box and whiskers}}
  \item{range}{\code{this determines how far the plot whiskers extend out from the box}}
  \item{lwd}{\code{linewidth of the box and whiskers}}
  \item{main}{\code{title of the plot}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.linesplot()}
\keyword{hplot}
