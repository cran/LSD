\name{distinctcolors}
\alias{distinctcolors}
\alias{demo.distinctcolors}
\title{Distinctcolors}
\description{find a vector of n distinct colors by laying a grid into the RGB colorspace}
\usage{
distinctcolors(n = 20, bw = FALSE, show = FALSE, maxvalue = 255)
}
\arguments{
  \item{n}{number of colors to be used}
  \item{bw}{should black and white be removed}
  \item{show}{should colors be plotted}
  \item{maxvalue}{maximal color value (rgb scheme)}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{disco}},\code{\link{display.colorpalette}},\code{\link{LSD.demo.tour}}}
\examples{demo.distinctcolors()}
\keyword{color}
