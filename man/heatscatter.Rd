\name{heatscatter}
\alias{heatscatter}
\alias{demo.heatscatter}
\title{Heat scatter}
\description{A colored scatterplot based on a 2d density computed by kde2d.}
\usage{
heatscatter(x, y, pch = 19, cexplot = 0.5, ncol = 30, grid = 100, colpal = "heat", xlim = NULL, ylim = NULL, main = NULL, cor = TRUE, method = "spearman", alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", greyscale = FALSE, ...)
}
\arguments{
  \item{x}{x-vector}
  \item{y}{y-vector}
  \item{pch}{plotting character}
  \item{cexplot}{cex of the points}
  \item{ncol}{the number of colors to use}
  \item{grid}{size of the grid ( see kde2d )}
  \item{colpal}{which colorpalette should be chosen ( see disco() )}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{main}{the title of the plot}
  \item{cor}{should the correlation be added to the title}
  \item{method}{correlation method}
  \item{alpha}{alpha value for color opacity}
  \item{add.contour}{should the contour lines be added}
  \item{nlevels}{number of levels of the contour lines}
  \item{color.contour}{color of the contour lines}
  \item{greyscale}{should the colorpalette be in greyscales}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatpairs}},\code{\link{heatmapairs}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatscatter()}
\keyword{hplot}
