% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSD.fusionplot.R
\name{fusionplot}
\alias{fusionplot}
\alias{LSD.fusionplot}
\title{Visualize two-dimensional data clusters}
\usage{
fusionplot(
  x,
  y,
  label = NULL,
  main = NULL,
  xlim = NULL,
  ylim = NULL,
  fromto = c(0.05, 0.95),
  colpal = "standardheat",
  simulate = FALSE,
  daltonize = FALSE,
  cvd = "p",
  nrcol = 25,
  outer.col = "lightgrey",
  quartiles.col = c("grey", "black", "grey"),
  add.quartiles = TRUE,
  separate = TRUE,
  rev = FALSE,
  size = TRUE,
  alpha = NULL,
  axes = TRUE,
  ...
)
}
\arguments{
\item{x}{a numeric vector.}

\item{y}{a numeric vector.}

\item{label}{a character vector assigning rows/elements of 'input' to clusters (if specified, multiple clusters can be depicted in different colors and/or subsequent plots).}

\item{main}{title(s) of the plot, standard graphics parameter.}

\item{xlim}{x limits, standard graphics parameter.}

\item{ylim}{y limits, standard graphics parameter.}

\item{fromto}{a numeric vector containing the range of quantiles (between 0 and 1) to be plotted.}

\item{colpal}{a character vector containing R built-in color names or a name of a \code{LSD} colorpalette as a character string (see disco() or \code{\link{disco}}).}

\item{simulate}{logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to simulate dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).}

\item{daltonize}{logical: if \code{TRUE} (\code{FALSE} by default), a converted colorpalette is used to enhance dichromat vision according to \url{http://www.daltonize.org} (see \code{\link{daltonize}}).}

\item{cvd}{character string implying the type of color vision deficiency ("p" for protanope, "d" for deuteranope or "t" for tritanope).}

\item{nrcol}{a non-negative integer specifying the number of colors to be used (defaults to 25, if not specified).}

\item{outer.col}{R built-in color to be used for outlier lines (lines outside of 'fromto').}

\item{quartiles.col}{a character vector containing three R built-in colors for quartile lines (c('0.25','0.5','0.75')).}

\item{add.quartiles}{logical: if \code{TRUE} (by default), lines are plotted corresponding to the quartiles.}

\item{separate}{if \code{TRUE} (by default), different clusters are depicted in subsequent plots.}

\item{rev}{logical: if \code{TRUE} (\code{FALSE} by default), a reversed colorpalette is used.}

\item{size}{logical: if \code{TRUE} (by default), the size of each cluster is added to the title of the respective plot.}

\item{alpha}{alpha value: a two-digit integer between 01 and 99 for color opacity, i.e. appearance of partial or full transparency (usage omitted by default).}

\item{axes}{logical: if \code{TRUE} (by default), a box and axes are added to the plot (if \code{FALSE}, custom specification of axes can be achieved via basic R graphics functions).}

\item{...}{additional parameters to be passed to points and plot.}
}
\description{
Depict a numeric matrix or list utilizing the underlying distribution quantiles of one dimension in a color encoded fashion.
}
\examples{
nr = 750
x = 1:nr/300
y = c(rnorm(nr)+sin(2*x)*3,rnorm(nr)+sin(2*x+pi/2)*3)
x = c(x,x)

labs = paste("cluster",c(rep(c(1,2),each = nr)))
colpals = c("oranges","pubu")
qcol = c("transparent","black","transparent")
fusionplot(x,y,labs,separate=FALSE,colpal=colpals,alpha=75,quartiles.col = qcol)
}
\seealso{
\code{\link{singlefusionplot}}, \code{\link{align}}, \code{\link{demotour}}, \code{\link{disco}}, \code{\link{colorpalette}}
}
\author{
Achim Tresch, Bjoern Schwalb
}
\keyword{cluster}
