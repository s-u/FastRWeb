\name{oinput}
\alias{oselection}
\alias{oinput}
\alias{osubmit}
\title{
  Functions aiding in creating HTML form elements.
}
\description{
  \code{oinput} creates an input element (text input, button, checkbox,
  file, hidden value, image, password, radio button or reset/submit
  button)

  \code{oselection} creates a drop-down list of items

  \code{osubmit} is a convenience wrapper for
  \code{oinput(type='submit', ...)} to create a submit button
}
\usage{
oinput(name, value, size, type="text", checked=FALSE, ...)
osubmit(name="submit", ...)
oselection(name, text, values = text, sel.index, sel.value, size, ...)
}
\arguments{
  \item{name}{name of the element in the HTML form. This argument is
    mandatory and should be unique in the form.}
  \item{value}{optional, value that will be pre-populated in the text
    field and/or the caption of the button.}
  \item{size}{optional, size of the element. For text input the number of visible
    characters, for selection the number of visible items.}
  \item{type}{type of the element. Valid entries are \code{"text"},
    \code{"password"}, \code{"button"}, \code{"checkbox"},
    \code{"radio"}, \code{"file"}, \code{"hidden"}, \code{"image"},
    \code{"reset"} and \code{"submit"}.}
  \item{checked}{boolean, if set to \code{TRUE} then the \code{checked}
    attribute is set in the element (valid for checkboxes only).}
  \item{text}{character vector of the items that will be shown to the
    user.}
  \item{values}{values that will represent the \code{text} items in the
    form and thus submitted. Typically IDs are used here instead of the
    actual text to avoid issues with encoding and size.}
  \item{sel.index}{index (integer or a logical vector) specifying which
    value will be selected. If missing, none will be marked as selected.} 
  \item{sel.value}{value (one of the \code{values} elements) which will
    be selected. Only one of \code{sel.index} and \code{set.value} may
    be specified.}
  \item{\dots}{
    Additional HTML attributes and their values. The actual range of
    supported attibutes is browser- and element-specific. Some commonly
    supported attributes include \code{disabled} (must be boolean),
    \code{class}, \code{id}, \code{style}, \code{onChange},
    \code{onClick}, \code{onSelect}, \code{onFocus}, \code{onBlur}.
    It is possible to pass objects as long as they implement
    \code{as.character} method to generate valid values that can be used
    in the \code{item="value"} form, i.e. assuming double quotes around
    the value in HTML.
  }
}
%\details{
%}
\value{
  The functions are called for their side-effect (see
  \code{\link{out}}). They return the current HTML buffer.
}
%\references{
%}
\author{
  Simon Urbanek
}
\note{
  All form-level functions assume the existence of an enclosing
  form. The actual behavior (other than custom JavaScript callback
  attributes) is defined by the enclosing form.
}
\seealso{
  \code{\link{out}}, \code{\link{oprint}}, \code{\link{done}}
}
\examples{
run <- function(foo, fruit, ...) {
  fruits <- c("apples", "oranges", "pears")
  if (!missing(fruit))
    out("Thank you for choosing ", fruits[as.integer(fruit)],"!<p>")

  out("<form>")
  out("Foo:")
  oinput("foo", foo)
  out("<br>Select fruit:")
  oselection("fruit", fruits, seq.int(fruits), , fruit)
  out("<br>")
  osubmit()
  out("</form>")
  done()
}
}
\keyword{interface}
