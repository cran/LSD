\name{heatbarplot}
\alias{heatbarplot}
\title{Heatbarplot}
\description{mimics a histogram (with an additional color stripe based on a kernel density estimate) that can be displayed horizontal}
\usage{
heatbarplot(x, colpal = "black", revpal = FALSE, horiz = FALSE, ncolors = 100, ...)
}
\arguments{
  \item{x}{histogram object}
  \item{colpal}{which colorpalette should be chosen ( see disco() )}
  \item{revpal}{reverse the colorpalette}
  \item{horiz}{rotate 90 degrees}
  \item{ncolors}{number of colors to use}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{cplot}},\code{\link{heathist}},\code{\link{LSD.demo.tour}}}
\examples{demo.cplot()}
\keyword{hplot}
