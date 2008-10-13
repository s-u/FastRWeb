\name{requests}
\alias{arequest}
\title{
  FastRWeb asynchronous (AJAX) requests
}
\description{
  \code{arequests} creates an anchor object representing AJAX request to load elements of the document dynamically
}
\usage{
arequest(txt, target, where, ..., attr = "") 
}
\arguments{
  \item{txt}{text (or any HTML content) inside the anchor}
  \item{target}{URI to load}
  \item{where}{name of the element (usuall a \code{div} tag) load the new content into}
  \item{...}{additional parameters to the request}
  \item{attr}{additional attributes for the anchor}
}
\value{
  Returns an object that can be added to the HTML document.
}
%\details{
%}
%\seealso{
%  \code{\link{.jcall}}, \code{\link{.jcast}}
%}
%\examples{
%}
\keyword{interface}
