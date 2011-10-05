\name{msdplot}
\alias{msdplot}
\alias{demo.msdplot}
\title{Msdplot}
\description{Progression plot for two-dimensional data in several clusters}
\usage{
msdplot(x, label = NULL, at = NULL, xlim = NULL, ylim = NULL, xlab = "timepoints", ylab = "values", main = "Msdplot", xaxt = "s", xlabels = NULL, las = 1, separate = TRUE, size = FALSE, col = NULL, bars = TRUE, alpha = 50, ...)
}
\arguments{
  \item{x}{\code{matrix with numerical entries, means and standard deviations of rows will define lines}}
  \item{label}{\code{vector with labels, if multiple clusters should be plotted}}
  \item{at}{\code{at which x-positions the columns should occur}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{xlab}{\code{xlab, standard graphics parameter}}
  \item{ylab}{\code{ylab, standard graphics parameter}}
  \item{main}{\code{title of the plot}}
  \item{xaxt}{\code{should an x axis be plotted at all? (="n" if not)}}
  \item{xlabels}{\code{text added as x-axis labels}}
  \item{las}{\code{las=1: horizontal text, las=2: vertical text (x-axis labels)}}
  \item{separate}{\code{should clusters be plotted separately}}
  \item{size}{\code{should the size of each cluster be added to the title}}
  \item{col}{\code{color of the plot}}
  \item{bars}{\code{error bars}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{singlemsdplot}},\code{\link{colorpalette}},\code{\link{disco}}}
\examples{demo.msdplot()}
\keyword{hplot}
