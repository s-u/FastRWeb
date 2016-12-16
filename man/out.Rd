\name{out}
\alias{out}
\alias{oprint}
\alias{otable}
\alias{ohead}
\alias{oclear}
\title{
  FastRWeb HTML output functions
}
\description{
  \code{out} outputs the argument as-is (also works for objects that are
  intended for web output)

  \code{oprint} outputs the result of verbatim \code{print} call

  \code{otable} constructs a table

  \code{ohead} creates a header

  \code{oclear} clears (by discarding existing content) the output buffer and/or headers
}
\usage{
out(..., sep = "", eol = "\n")
oprint(..., sep = "\n", escape = TRUE)
otable(..., tab = "", tr = "", cs = "</td><td>", escape = TRUE)
ohead(..., level = 3, escape = TRUE)
oclear(output=TRUE, headers=FALSE)
}
\arguments{
  \item{...}{entries to output or print}
  \item{sep}{separator string}
  \item{eol}{end of line separator}
  \item{escape}{if \code{TRUE} special HTML characters are escaped in
  inner text (via `FastRWeb:::htmlEscape`), if \code{FALSE} the
  strings are passed without modification. It can also be a function
  taking exactly one argument that is expected to perform the escaping.}
  \item{tab}{additional attributes for \code{table} HTML tag}
  \item{tr}{additional attibutes for table row (\code{tr}) HTML tag}
  \item{cs}{column separator}
  \item{level}{level of the header (1 is the topmost)}
  \item{output}{logical, if \code{TRUE} then the output is cleared}
  \item{headers}{logical, if \code{TRUE} then the headers are cleared}
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
  HTML document. Special HTML characters `<`, `>` and `&` are escaped
  in the inner text (not in tags) if \code{escape=TRUE} in functions
  that provide that argument.

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
    ohead("My Table", level=2)
    d <- data.frame(a = 1:3, b = c("foo", "bar", "foobar"))
    otable(d)
    out("<p><b>Verbatim R output:</b><br>")
    oprint(str(d))
    done()
  }    
}
\keyword{interface}
