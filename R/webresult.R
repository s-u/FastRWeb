as.WebResult <- function(x, ...) UseMethod("as.WebResult")

as.WebResult.WebResult <- function(x, ...) x

as.WebResult.default <- function(x, ...)
  structure(c("html", paste(as.character(x),collapse='\n'), "text/html; charset=utf-8"), class="WebResult")
