\name{msdplot}
\alias{msdplot}
\alias{demo.msdplot}
\title{Msdplot}
\description{Progression plot for two-dimensional data in several clusters}
\usage{
msdplot(x, label = NULL, at = NULL, xlim = NULL, ylim = NULL, xlab = "timepoints", ylab = "values", main = "Msdplot", xaxt = "s", xlabels = NULL, las = 1, separate = TRUE, size = FALSE, col = NULL, bars = TRUE, alpha = 50, ...)
}
\arguments{
  \item{x}{matrix or list with numerical entries, means and standard deviations of rows will define lines}
  \item{label}{vector with labels, if multiple clusters should be plotted}
  \item{at}{at which x-positions the columns should occur}
  \item{xlim}{xlimits, standard graphics parameter}
  \item{ylim}{ylimits, standard graphics parameter}
  \item{xlab}{xlab, standard graphics parameter}
  \item{ylab}{ylab, standard graphics parameter}
  \item{main}{title of the plot}
  \item{xaxt}{should an x axis be plotted at all? (="n" if not)}
  \item{xlabels}{text added as x-axis labels}
  \item{las}{\code{las=1}: horizontal text, \code{las=2}: vertical text (x-axis labels)}
  \item{separate}{should clusters be plotted separately}
  \item{size}{should the size of each cluster be added to the title}
  \item{col}{color of the plot}
  \item{bars}{error bars}
  \item{alpha}{alpha value for color opacity}
  \item{\dots}{additional parameters to be passed to points and plot}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{singlemsdplot}},\code{\link{colorpalette}},\code{\link{disco}}}
\examples{demo.msdplot()}
\keyword{hplot}
