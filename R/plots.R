WebPlot <- function(width=640, height=480, type='png', ...) {
  file <- paste('tmp-',paste(sprintf('%x',as.integer(runif(4)*65536)),collapse=''),'.tmp',sep='')
  if (type %in% c('pdf', 'svg')) file <- paste(file, type, sep='.') ## some back-ends always append the extension, unfortuantely
  Cairo(width, height, type=type, file=file, ...)
  mime <- switch(type, png="image/png", pdf="application/pdf", jpg="image/jpeg", jpeg="image/jpeg", gif="image/gif", svg="image/svg+xml", "application/octet")
  structure(list(file=file,type=type,mime=mime,width=width,height=height), class="WebPlot")
}

as.WebResult.WebPlot <- function(x, ...) {
  dev.off()
  structure(c("tmpfile", x$file, x$mime), class="WebResult")
}

as.character.WebPlot <- function(x, ...) {
  dev.off()
  tag <- "img"
  sz <- file.info(x$file)$size
  r <- readBin(x$file, raw(), sz)
  unlink(x$file)
  if (isTRUE(x$type == 'pdf')) tag <- "embed"
  paste0("<", tag, " src='",base64enc::dataURI(r, x$mime),"' width=",x$width," height=",x$height,">")
}
