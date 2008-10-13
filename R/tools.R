out <- function(..., sep='', eol='\n') {
  .out <<- c(.out, paste(..., sep=sep, collapse=eol))
}

otable <- function(..., tab='', tr='', cs='</td><td>') {
  a=list(...)
  ml=max(unlist(lapply(a,length)))
  m=matrix(unlist(lapply(a,function(x) rep(as.character(x),length.out=ml))),ml)
  tout <- unlist(lapply(1:ml, function(x) paste("<tr",tr,"><td>",paste(m[x,],collapse=cs),"</td></tr>",sep='')))
  .out <<- c(.out, paste("<table ",tab,">\n",sep=''), tout, '</table>')
}

ohead <- function(..., level=3) .out <<- c(.out, paste("<h",level,">",paste(...,sep=''),"</h",level,">",sep=''))

oprint <- function(..., sep='\n') .out <<- c(.out, paste("<pre>",paste(capture.output(...),collapse=sep),"</pre>",sep=''))

arequest <- function(txt, target, where, ..., attr='') {
     if (length(list(...)))
          paste("<a href='javascript:void(0);' onclick=\"javascript:req('",target,"','",where,"','",paste(..., sep=''),"');\"",attr,">",txt,"</a>",sep='')
     else
          paste("<a href='javascript:void(0);' onclick=\"javascript:req('",target,"','",where,"');\"",attr,">",txt,"</a>",sep='')
}

#link <- function(url,target,par,...) paste("<a href=# onclick=\"req('",where,"','",target,"','",par,"');\">",...,"</a>",sep='',co

#initialize output to an empty string
.out <<- character(0)

done <- function(..., cmd="html", type="text/html; charset=utf-8") return(c(cmd,ifelse(length(list(...)),paste(.out,paste(...,sep='',collapse='\n'),sep='',collapse='\n'),paste(.out,collapse='\n')),type))

## general tools

# a fast way to convert a list into a dataframe
.df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.null(x)) return(NULL)
  if (!is.list(x)) x = as.list(x)
  attr(x,"row.names") = c(NA_integer_, -length(x[[1]]))
  class(x) = "data.frame"
  x
}
