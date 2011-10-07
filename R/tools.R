## environemnt to store result and headers in
.e <- new.env(parent = emptyenv())

out <- function(..., sep='', eol='\n')
    .e$out <- c(.e$out, paste(..., sep=sep, collapse=eol))

otable <- function(..., tab='', tr='', cs='</td><td>') {
  a <- list(...)
  if (length(a)==1 && is.list(a[[1]])) a <- a[[1]]
  ml <- max(unlist(lapply(a,length)))
  m <- matrix(unlist(lapply(a,function(x) rep(as.character(x),length.out=ml))),ml)
  tout <- unlist(lapply(1:ml, function(x) paste("<tr",tr,"><td>",paste(m[x,],collapse=cs),"</td></tr>",sep='')))
  .e$out <- c(.e$out, paste("<table ",tab,">\n",sep=''), tout, '</table>')
}

ohead <- function(..., level=3)
  .e$out <- c(.e$out, paste("<h",level,">",paste(...,sep=''),"</h",level,">",sep=''))

oprint <- function(..., sep='\n')
  .e$out <- c(.e$out, paste("<pre>",paste(capture.output(print(...)),collapse=sep),"</pre>",sep=''))

arequest <- function(txt, target, where, ..., attr='') {
     if (length(list(...)))
          paste("<a href='javascript:void(0);' onclick=\"javascript:req('",target,"','",where,"','",paste(..., sep=''),"');\"",attr,">",txt,"</a>",sep='')
     else
          paste("<a href='javascript:void(0);' onclick=\"javascript:req('",target,"','",where,"');\"",attr,">",txt,"</a>",sep='')
}

add.header <- function(txt) {
    if (!is.null(.e$headers))
      .e$headers <- as.character(txt)
    else
          .e$headers <- c(.e$headers, as.character(txt))
    invisible(.e$headers)
}

#link <- function(url,target,par,...) paste("<a href=# onclick=\"req('",where,"','",target,"','",par,"');\">",...,"</a>",sep='',co

done <- function(..., cmd="html", type="text/html; charset=utf-8")
  WebResult(cmd, ifelse(length(list(...)), paste(as.character(.e$out),paste(...,sep='',collapse='\n'),sep='',collapse='\n'), paste(as.character(.e$out),collapse='\n')), type)

# create query string from 'pars' and merge in any additional parameters passed
.npar <- function(...) {
     # we have to use get(), otherwise codetools get confused...
     q <- if (exists("pars") && is.list(get("pars"))) get("pars") else list()
     l <- list(...)
     for (i in names(l)) q[[i]] <- if (l[[i]] == "") NULL else l[[i]]
     if (!length(q)) return("")
     ml <- max(unlist(lapply(q,length)))
     for (i in names(q)) q[[i]] <- rep(paste(i,"=",as.character(q[[i]]),sep=''),length.out=ml)
     unlist(lapply(1:ml, function(x) paste(unlist(lapply(q, function(a) a[[x]])),sep='',collapse='&')))
}

#alink <- function(text, href, ...) {
#  if (missing(href)) href <- 'javascript:void(0);'
#  a <- list(...)
#}

#arequest <- function(what, where, ...) {
#  a <- list(...)
#  xp <- ''
#  if (length(a)) paste(names(a),as.character(a),sep='=',collapse='&'
#}


## general tools

# a fast way to convert a list into a dataframe
.df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.null(x)) return(NULL)
  if (!is.list(x)) x <- as.list(x)
  attr(x,"row.names") <- c(NA_integer_, -length(x[[1]]))
  class(x) <- "data.frame"
  x
}
