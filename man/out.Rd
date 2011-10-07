\name{out}
\alias{out}
\alias{oprint}
\alias{otable}
\alias{ohead}
\title{
  FastRWeb HTML output functions
}
\description{
  \code{out} outputs the argument as-is (also works for objects that are
  intended for web output)

  \code{oprint} outputs the result of verbatim \code{print} call

  \code{otable} constructs a table

  \code{ohead} creates a header
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
\details{
  The output functions enable the \code{run} function to build the
  result object gradually as opposed to returing just one
  \code{\link{WebResult}} object at the end.

  The output functions above manipulate an internal buffer that collects
  output and uses \code{\link{done}} to contruct the final
  \code{\link{WebResult}} object. It is analogous to using \code{print}
  to create output in R scripts as they proceed. However, due to the
  fact that \code{print} output is generally unsuitable as HTML output,
  the output function here process the output such that the result is a
  HTML document.

  NOTE: It is important to remember that the output is collected in a
  buffer, so in order to actually create the output, do not forget to use
  \code{return(done())} when leaving the \code{run} function to use that
  content!
}
\seealso{
  \code{\link{done}}, \code{\link{WebResult}}
}
\examples{
  run <- function(...) {
    out("<h2>My Table</h2>")
    d <- data.frame(a = 1:3, b = c("foo", "bar", "foobar"))
    otable(d)
    oprint(str(d))
    done()
  }    
}
\keyword{interface}
