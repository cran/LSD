\name{distinctcolors}
\alias{distinctcolors}
\alias{demo.distinctcolors}
\title{Distinctcolors}
\description{find a vector of n distinct colors by laying a grid into the RGB colorspace}
\usage{
distinctcolors(n = 20, bw = FALSE, graphics = TRUE, maxvalue = 255)
}
\arguments{
  \item{n}{\code{number of colors to be used}}
  \item{bw}{\code{should black and white be removed}}
  \item{graphics}{\code{should x11() be called}}
  \item{maxvalue}{\code{maximal color value (rgb scheme)}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{disco}},\code{\link{display.colorpalette}},\code{\link{LSD.demo.tour}}}
\examples{demo.distinctcolors()}
\keyword{color}
