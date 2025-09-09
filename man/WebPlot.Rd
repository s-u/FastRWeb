\name{WebPlot}
\alias{WebPlot}
\title{
  Graphics device for inclusion of plots in FastRWeb results.
}
\description{
  \code{WebPlot} opens a new graphics device (currently based on
  \code{Cairo}) and returns an object that can be used as a result of
  FastRWeb functions or in web output.
}
\usage{
WebPlot(width = 640, height = 480, type = "png", ...)
}
\arguments{
  \item{width}{width of the resulting plot (normally in pixels)}
  \item{height}{height of the resulting plot (normally in pixels)}
  \item{type}{type of the output}
  \item{\dots}{further arguments to be passed to \code{\link[Cairo]{Cairo}}}
}
\details{
  \code{WebPlot} generates a temporary file name that is accessible using
  the \code{"tmpfile"} command of \code{\link{WebResult}} and opens a
  new \code{\link[Cairo]{Cairo}} device with the specified parameteres. It
  returns a \code{WebPlot} object that can be either retured directly
  from the \code{run()} function (and thus resulting in one image) or
  used with the \code{\link{out}()} function to reference the image in
  an HTML page (see examples below).

  Note that \code{\link{as.WebResult}} coercion is used to finalize the
  result when returned directly and it will close the device, so
  \code{dev.off()} is optional and not needed in that case. Also
  \code{WebPlot} reserves the right to close any or all other
  active \code{WebPlot} devices - this ensures that \code{dev.off()} may
  not be neeed at all even when using multiple \code{WebPlot}s.
}
\value{
  \code{WebPlot} object.

  The structure of the \code{WebPlot} class is considered internal and
  should not be created directly. Current attributes include
  \code{file} (filename), \code{type} (output type), \code{mime} (MIME
  type), \code{width}, \code{height}.
}
%\references{
%}
\author{
Simon Urbanek
}
%\note{
%}
\seealso{
\code{\link{WebResult}}
}
\examples{
## example 1: single image
## if saved as "plot.png.R"
## it can be served as http://server/cgi-bin/R/plot.png
run <- function(n = 100, ...) {
  n <- as.integer(n)
  # create the WebPlot device
  p <- WebPlot(800, 600)
  # plot ...
  plot(rnorm(n), rnorm(n), pch=19, col="#ff000080")
  # return the WebPlot result
  p
}

## example 2: page containing multiple images
## if saved as "plotex.html.R"
## it can be served as http://server/cgi-bin/R/plotex.html
run <- function(...) {
  out("<h2>Simple example<h2>")
  data(iris) ## ideally, you'll use data from the Rserve session
  attach(iris)
  p <- WebPlot(600, 600)
  plot(Sepal.Length, Petal.Length, pch=19, col=Species)
  out(p)
  p <- WebPlot(350, 600)
  barplot(table(Species), col=seq.int(levels(Species)))
  out(p)
  done()
}
}
\keyword{interface}
