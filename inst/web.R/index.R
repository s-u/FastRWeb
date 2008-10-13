# just return the contents of the index.html file in the web directory
run <- function(...)
  paste(readLines(system.file("web/index.html", package="FastRWeb")))
