\name{done}
\alias{done}
\title{
  FastRWeb interface functions
}
\description{
  \code{done} collects the entire contents created so far using output
  functions (such as \code{\link{out}}) and creates a
  \code{\link{WebResult}} object that can be returned from the
  \code{run} function
}
\usage{
done(..., cmd = "html", type = "text/html; charset=utf-8")
}
\arguments{
  \item{...}{additional entries to appent to the output (or the actual content depending on the command)}
  \item{cmd}{interface command}
  \item{type}{content type}
}
\value{
  Returns a \code{\link{WebResult}} object that can be returned from the
  \code{run} function.
}
\details{
  Some objects can override the default command and type value. For
  example, if the document consits solely of a plot then the content
  type is taken from the plot and command changed to \code{"tmpfile"}.
}
\seealso{
  \code{\link{out}}, \code{\link{WebResult}}
}
%\examples{
%}
\keyword{interface}
