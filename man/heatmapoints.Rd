\name{heatmapoints}
\alias{heatmapoints}
\alias{demo.heatmapoints}
\title{Heat MA points}
\description{A colored MA plot based on a 2d density computed by kde2d to be added to an existing plot.}
\usage{
heatmapoints(x, y, pch = 19, cexplot = 0.5, ncol = 30, grid = 100, colpal = "heat", xlim = NULL, ylim = NULL, alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", add.line = TRUE, color.line = "darkgrey", lwd = 2, greyscale = FALSE, ...)
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
  \item{alpha}{alpha value for color opacity}
  \item{add.contour}{should the contour lines be added}
  \item{nlevels}{number of levels of the contour lines}
  \item{color.contour}{color of the contour lines}
  \item{add.line}{add horizontal line to the plot}
  \item{color.line}{color of the horizontal line}
  \item{lwd}{lwd, standard graphics parameter}
  \item{greyscale}{should the colorpalette be in greyscales}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatmapairs}},\code{\link{heatmaplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatmapoints()}
\keyword{aplot}
