\name{WebResult}
\alias{WebResult}
\alias{as.WebResult}
\title{
  Result object of a FastRWeb script
}
\description{
  \code{WebResult} is the class of the object that will be returned
  from the \code{run} function of a FastRWeb script back to the browser.

  Using a separate class allows automatic conversion of other objects
  into the necessary representation - all that is needed is a
  \code{as.WebResult} method for that particular object.

  \code{WebResult} function can be used to create such objects directly.

  \code{as.WebResult} coerces an object into a \code{WebResult}, it is a
  generic. This allows methods to be defined for \code{as.WebResult}
  which act as convertors transforming R objects into web results.
}
\usage{
WebResult(cmd = "html", payload = "", content.type = "text/html; charset=utf-8",
          headers = character(0))
as.WebResult(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmd}{string, command passed back to the FastRWeb
    interface. Currently supported commands are \code{"html"},
    \code{"file"}, \code{"tmpfile"} and \code{"raw"}. See details below.}
  \item{payload}{string, the body (contents) that will be sent back or
    file name, depending on the command}
  \item{content.type}{MIME content type specification as it will be
    returned to the browser}
  \item{headers}{string vector, optional additional headers to be sent
    to the browser. Must not contain CR or LF!}
  \item{x}{object to convert into \code{WebResult}}
  \item{\dots}{additional arguments passed to the method}
}
\details{
  There are four ways the results can be passed from R to the client
  (browser):

  \itemize{
    \item \code{"html"} is the default mode and it simply sends the
    result contained in \code{payload} to the browser as the body of the
    HTTP response.

    \item \code{"file"} sends the content of the file with the name
    specified in \code{payload} from the \code{web} subdirectory of the
    FastRWeb project root as the body of the HTTP response.

    \item \code{"tmpfile"} sends the content of the file with the name
    specified in \code{payload} from the \code{tmp} subdirectory of the
    FastRWeb project root as the body of the HTTP response and removes
    the file once it was delivered.

    \item \code{"raw"} does not generate any HTTP headers but assumes
    that \code{payload} defines the entire HTTP response including
    headers. The use of this command is discouraged in favor of
    \code{"html"} with headers, since the payload must be properly
    formatted, which can be difficult.
  }

  All modes except \code{"raw"} cause FastRWeb to generate HTTP headers
  based on the content and any custom headers that were added using
  \code{\link{add.header}} or the \code{headers} argument. Note that
  the latter two may NOT contain \code{Content-length:} and
  \code{Content-type:} entries as those are generated automatically
  based on the content and the \code{content.type} argument.
}
\value{
  Object of the class \code{WebResult}
}
%\references{
%}
\author{
Simon Urbanek
}
%\note{
%}
\seealso{
  \code{\link{add.header}}, \code{\link{done}}
}
%\examples{
%}
\keyword{interface}
