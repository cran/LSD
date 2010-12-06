\name{heatmaplot}
\alias{heatmaplot}
\alias{demo.heatmaplot}
\title{Heat MA plot}
\description{A colored MA plot based on a 2d density computed by kde2d.}
\usage{
heatmaplot(x, y, pch = 19, cexplot = 0.5, ncol = 30, grid = 100, colpal = "heat", xlim = NULL, ylim = NULL, xlab = "M-values", ylab = "A-values", main = "heatmaplot", cor = FALSE, alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", add.line = TRUE, color.line = "darkgrey", lwd = 2, greyscale = FALSE, ...)
}
\arguments{
  \item{x}{\code{x-vector}}
  \item{y}{\code{y-vector}}
  \item{pch}{\code{plotting character}}
  \item{cexplot}{\code{cex of the points}}
  \item{ncol}{\code{the number of colors to use}}
  \item{grid}{\code{size of the grid ( see kde2d )}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{xlab, standard graphics parameter}}
  \item{ylab}{\code{ylab, standard graphics parameter}}
  \item{main}{\code{the title of the plot}}
  \item{cor}{\code{should the (spearman) correlation be added to the title}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{add.contour}{\code{should the contour lines be added}}
  \item{nlevels}{\code{number of levels of the contour lines}}
  \item{color.contour}{\code{color of the contour lines}}
  \item{add.line}{\code{add horizontal line to the plot}}
  \item{color.line}{\code{color of the horizontal line}}
  \item{lwd}{\code{lwd, standard graphics parameter}}
  \item{greyscale}{\code{should the colorpalette be in greyscales}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatmapairs}},\code{\link{heatpairs}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatmaplot()}
\keyword{hplot}
