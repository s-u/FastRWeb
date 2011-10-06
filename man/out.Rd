\name{out}
\alias{out}
\alias{oprint}
\alias{otable}
\alias{ohead}
\title{
  FastRWeb HTML output functions
}
\description{
  \code{out} outputs the argument as-is (also works for objects that are intended for web output)

  \code{oprint} outputs the result of verbatim print

  \code{otable} constructs a table
}
\usage{
out(..., sep = "", eol = "\n")
oprint(..., sep = "\n")
otable(..., tab = "", tr = "", cs = "</td><td>")
ohead(..., level = 3)
}
\arguments{
  \item{...}{entries to output or print}
  \item{sep}{separator string}
  \item{eol}{end of line separator}
  \item{tab}{additional attributes for \code{table} HTML tag}
  \item{tr}{additional attibutes for table row (\code{tr}) HTML tag}
  \item{cs}{column separator}
  \item{level}{level of the header (1 is the topmost)}
}
\value{
  All functions returns the full document as constructed so far
}
%\details{
%}
%\seealso{
%  \code{\link{.jcall}}, \code{\link{.jcast}}
%}
\examples{
  run <- function(...) {
    out("<h2>My Table</h2>")
    d = data.frame(a = 1:3, b = c("foo", "bar", "foobar"))
    otable(d)
    oprint(str(d))
    done()
  }    
}
\keyword{interface}
