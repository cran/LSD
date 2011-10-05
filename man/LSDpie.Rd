\name{LSDpie}
\alias{LSDpie}
\alias{demo.LSDpie}
\title{LSD Pie}
\description{Piecharts at arbitrary position and radii.}
\usage{
LSDpie(props, x = 0, y = 0, radius = 1, cols = NULL, colpal = "pies", edges = 100, add = FALSE, xlim = NULL, ylim = NULL, main = "Piecharts", alpha = NULL, ...)
}
\arguments{
  \item{props}{vector giving the relations of the pie pieces (need not to be normed)}
  \item{x}{x-position of the piechart}
  \item{y}{y-position of the piechart}
  \item{radius}{radius of the piechart}
  \item{cols}{vector of colors}
  \item{colpal}{colorpalette if cols is not specified}
  \item{edges}{number of edges the "circle" will have}
  \item{add}{should piechart be added to an existing plot}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{main}{title of the plot}
  \item{alpha}{alpha value for color opacity}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Achim Tresch, Bjoern Schwalb}
\seealso{\code{\link{disco}},\code{\link{LSD.demo.tour}}}
\examples{demo.LSDpie()}
\keyword{hplot}
