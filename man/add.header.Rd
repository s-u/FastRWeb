\name{add.header}
\alias{add.header}
\title{
  Add HTML headers to FastRWeb response.
}
\description{
  \code{add.header} appends additional headers to the HTML reponse when
  using \code{\link{WebResult}} with any other command than
  \code{"raw"}.

  This is useful for handling of cookies (see \code{getCookies()} in the
  sample \code{common.R} script), cache-behavior, implementing
  URL redirection etc.
}
\usage{
add.header(txt)
}
\arguments{
  \item{txt}{character vector of header entires. The string may NOT
  include any CR/LF characters, those will be automatically generated
  when the final response is constructed. Elements of the vector should
  represent lines. It is user's responsibility to ensure the entries are
  valid according to the HTTP standard. Also note that you should never
  add either \code{Content-type:} or \code{Content-length:} headers as
  those are always generated automatically from the
  \code{\link{WebResult}}.}
}
%\details{
%}
\value{
  Character vector of the resulting headers
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
## main.css.R: serve a static file (main.css) with cache control
run <- function(...) {
  # last for at most an hour
  add.header("Cache-Control: max-age=3600")
  WebResult("file", "main.css", "text/css")
}
}
\keyword{interface}
