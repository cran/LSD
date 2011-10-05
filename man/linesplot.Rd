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
  \item{x}{data as vector, matrix, list or data.frame}
  \item{labels}{vector of labels}
  \item{col}{standard color}
  \item{cols}{vector of colors}
  \item{alpha}{alpha value for color opacity}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{xlab}{xlab, standard graphics parameter}
  \item{ylab}{ylab, standard graphics parameter}
  \item{las}{\code{las=1}: horizontal text, \code{las=2}: vertical text (x-axis labels)}
  \item{outline}{should outliers be plotted}
  \item{cexbox}{cex of the boxes}
  \item{addboxes}{should boxplots be added}
  \item{border}{color of the box and whiskers}
  \item{range}{this determines how far the plot whiskers extend out from the box}
  \item{lwd}{linewidth of the box and whiskers}
  \item{main}{title of the plot}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{heatboxplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.linesplot()}
\keyword{hplot}
