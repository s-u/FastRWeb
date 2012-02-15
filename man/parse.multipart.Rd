\name{parse.multipart}
\alias{parse.multipart}
\title{
  Parsing of POST request multi-part body.
}
\description{
  \code{parse.multipart} parses the result of a POST request that is in
  a multi-part encoding. This is typically the case when a form is
  submitted with \code{"enctype='multipart/form-data'"} property and
  \code{"file"} input types.
}
\usage{
parse.multipart(request = .GlobalEnv$request)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{request}{Request interface object as defined by the FastRWeb
    interface. \code{parse.multipart} will use \code{c.type},
    \code{c.length} and \code{body} elements of the object.}
}
%\details{
%}
\value{
  On success a named list of values in the form. Scalar values are
  passed literally as strings, files (multi-part chunks) are passed as
  lists with named elements \code{content_type}, \code{tempfile} (file
  containing the content), \code{filename} (name of the file as
  specified in the encoding,if present) and \code{head} (character
  vector of content headers).

  On failure \code{NULL} with a warning.
}
%\references{
%}
\author{
  The original parser code was written by Jeffrey Horner for the Rook package.
}
\note{
  The typical use is along the lines of:
  
  \code{if (grepl("^multipart", request$s.type)) pars <- parse.multipart()}
  
  The function uses warnings to communicate parsing issues. While
  debugging, it may be usedful to convert then to errors via
  \code{options(warn=2)} so they will be visigle on the client side.
}
%%  ~~further notes~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
