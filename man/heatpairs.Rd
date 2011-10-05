\name{heatpairs}
\alias{heatpairs}
\alias{demo.heatpairs}
\title{Heat pairs}
\description{Colored pairwise scatterplots based on a 2d density esimates computed by kde2d.}
\usage{
heatpairs(mat, main = "Heatpairs", xlim = NULL, ylim = NULL, labels = NULL, add.points = FALSE, group = NULL, color.group = "magenta", method = "spearman", colpal = "heat", pch = 19, cexplot = 0.5, cor.cex = 2.5, ncol = 30, grid = 100, alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, ...)
}
\arguments{
  \item{mat}{matrix}
  \item{main}{the title of the plot}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{labels}{labels to be shown on the diagonal}
  \item{add.points}{should a certain group of points be colored somehow}
  \item{group}{certain group of points that should be highlighted}
  \item{color.group}{color in which the certain group of points should be highlighted}
  \item{method}{correlation method}
  \item{colpal}{which colorcolpal should be chosen ( see disco() )}
  \item{pch}{plotting character}
  \item{cexplot}{cex of the points}
  \item{cor.cex}{cex of the correlation}
  \item{ncol}{the number of colors to use}
  \item{grid}{size of the grid ( see kde2d )}
  \item{alpha}{alpha value for color opacity}
  \item{add.contour}{should the contour lines be added}
  \item{nlevels}{number of levels of the contour lines}
  \item{color.contour}{color of the contour lines}
  \item{greyscale}{should the colorpalette be in greyscales}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatscatter}},\code{\link{heatmapairs}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatpairs()}
\keyword{hplot}
