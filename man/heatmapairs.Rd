\name{heatmapairs}
\alias{heatmapairs}
\alias{demo.heatmapairs}
\title{Heat MA pairs}
\description{Colored pairwise MA plots based on 2d density estimates computed by kde2d.}
\usage{
heatmapairs(mat, main = "Heat MA pairs", xlim = NULL, ylim = NULL, labels = NULL, add.points = FALSE, group = NULL, color.group = "magenta", method = "spearman", colpal = "heat", pch = 19, cexplot = 0.5, ncol = 30, grid = 100, alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, add.line = TRUE, color.line = "darkgrey", lwd = 2, ...)
}
\arguments{
  \item{mat}{\code{matrix}}
  \item{main}{\code{the title of the plot}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{labels}{\code{labels to be shown on the diagonal}}
  \item{add.points}{\code{should a certain group of points be colored somehow}}
  \item{group}{\code{certain group of points that should be highlighted}}
  \item{color.group}{\code{color in which the certain group of points should be highlighted}}
  \item{method}{\code{correlation method}}
  \item{colpal}{\code{which colorcolpal should be chosen ( see disco() )}}
  \item{pch}{\code{plotting character}}
  \item{cexplot}{\code{cex of the points}}
  \item{ncol}{\code{the number of colors to use}}
  \item{grid}{\code{size of the grid ( see kde2d )}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{add.contour}{\code{should the contour lines be added}}
  \item{nlevels}{\code{number of levels of the contour lines}}
  \item{color.contour}{\code{color of the contour lines}}
  \item{greyscale}{\code{should the colorpalette be in greyscales}}
  \item{add.line}{\code{add horizontal line to the plot}}
  \item{color.line}{\code{color of the horizontal line}}
  \item{lwd}{\code{lwd, standard graphics parameter}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatmaplot}},\code{\link{heatpairs}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatmapoints()}
\keyword{hplot}
