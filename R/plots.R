WebPlot <- function(width=640, height=480, type='png', inline=isTRUE(getOption("inline.WebPlot")), use.file=isTRUE(getOption("use.file.WebPlot")), ...) {
  if (use.file) {
    file <- paste('tmp-',paste(sprintf('%x',as.integer(runif(4)*65536)),collapse=''),'.tmp',sep='')
    dev <- Cairo(width, height, type=type, file=file, ...)
    mime <- switch(type, png="image/png", pdf="application/pdf", jpg="image/jpeg", jpeg="image/jpeg", gif="image/gif", "application/octet")
    structure(list(file=file,type=type,mime=mime,width=width,height=height,inline=inline,dev=dev), class="WebPlot")
  } else {
    dev <- Cairo(width, height, type='raster', ...)
    mime <- switch(type, png="image/png", pdf="application/pdf", jpg="image/jpeg", jpeg="image/jpeg", gif="image/gif", "application/octet")
    quality <- list(...)$quality
    if (is.null(quality)) quality <- 75
    structure(list(type=type,mime=mime,width=width,height=height,inline=inline,dev=dev,quality=quality/100), class="WebPlot")
  }
}

.encode.WebPlot <- function(x, dst=raw()) {
  if (x$type == "png")
    png::writePNG(Cairo.capture(x$dev), dst)
  else if (x$mime == "image/jpeg")
    jpeg::writeJPEG(Cairo.capture(x$dev), dst, quality=x$quality)
  else stop("Unsupported type, please set use.file=TRUE for this type")
}

as.WebResult.WebPlot <- function(x, ...) {
  dev.off()
  ## FIXME: base64 encoding blows up the content so maybe we should create a file anyway ...
  if (is.null(x$file)) structure(c("html", base64enc::base64encode(.encode.WebPlot(x)), s$mime, "Content-Transfer-Encoding: BASE64"), class="WebResult") else structure(c("tmpfile", x$file, x$mime), class="WebResult")
}

as.character.WebPlot <- function(x, ...) {
  dev.off()
  if (is.null(x$file)) {
    if (x$inline) 
      return(paste("<img src='", base64enc::dataURI(.encode.WebPlot(x), mime=x$mime), "' width=",x$width," height=",x$height,">",sep=''))
    
    ## no inlining? then dump it into a file ...
    x$file <- paste('tmp-',paste(sprintf('%x',as.integer(runif(4)*65536)),collapse=''),'.tmp',sep='')
    f <- file(x$file, "wb")
    .encode.WebPlot(x, f)
    close(f);
  }

  if (x$inline) {
    res <- paste("<img src='", base64enc::dataURI(file=x$file, mime=x$mime), "' width=",x$width," height=",x$height,">",sep='')
    unlink(x$file)
    return(res)
  }

  paste("<img src='tmp?file=",x$file,"&mime=",x$mime,"' width=",x$width," height=",x$height,">",sep='')
}
