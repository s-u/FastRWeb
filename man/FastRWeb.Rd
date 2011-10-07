\name{FastRWeb}
\alias{FastRWeb}
\alias{FastRWeb-package}
\title{
  FastRWeb - infratructure to serve web pages with R scripts efficiently
}
\section{About FastRWeb}{
  \bold{FastRWeb} is not just a package, but an entire infrastructure
  allowing the use of R scripts to create web pages and graphics.

  The basic idea is that an URL of the form
  \code{http://server/cgi-bin/R/foo?bar=value} will be processed by FastRWeb
  such as to result in sourcing of the \code{foo.R} script and running
  the function \code{run(bar="value")} which is expected to be defined
  in that script. The results of a script can be anything from HTML
  pages to bitmap graphics or PDF document.

  \bold{FastRWeb} uses CGI or PHP as front-end and \code{Rserve} server
  as the back-end. For details see \cite{Urbanek, S. (2008)
  \emph{FastRWeb: Fast Interactive Web Framework for Data Mining Using R},
  IASC 2008}.
  
  The R code in the package itself provides R-side tools that
  facilitate the delivery of results to a browser - such as
  \code{\link{WebResult}}, \code{\link{WebPlot}}, \code{\link{out}},
  \code{\link{done}} - more in detail below.
}
\section{Installation}{
  
}
\seealso{
  \code{\link{WebResult}}, \code{\link{WebPlot}}, \code{\link{out}},
  \code{\link{done}}, \code{\link{add.header}}
}
%\examples{
%}
\keyword{interface}
