\name{done}
\alias{done}
\title{
  FastRWeb interface functions
}
\description{
  \code{done} collect the entire contents created so far and creates an object that can be returned from the \code{run} function
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
  Returns an object that can be returned from the \code{run} function.
}
\details{
  Some objects can override the default command and type value. For example, if the document consits solely of a plot then the content type is taken from the plot and command changed to \code{"tmpfile"}.
}
%\seealso{
%  \code{\link{.jcall}}, \code{\link{.jcast}}
%}
%\examples{
%}
\keyword{interface}
