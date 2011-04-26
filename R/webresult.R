as.WebResult <- function(x, ...) UseMethod("as.WebResult")

as.WebResult.WebResult <- function(x, ...) x

as.WebResult.default <- function(x, ...)
  structure(c("html", paste(as.character(x),collapse='\n'), "text/html; charset=utf-8"), class="WebResult")

WebResult <- function(cmd="html", payload, content.type="text/html; charset=utf-8", headers="")
  structure(c(cmd, paste(as.character(payload), collapse="\n"), content.type, headers), class="WebResult")
