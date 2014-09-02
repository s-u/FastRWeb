## environemnt to store result and headers in
.e <- new.env(parent = emptyenv())

out <- function(..., sep='', eol='\n')
    .e$out <- c(.e$out, paste(..., sep=sep, collapse=eol))

oclear <- function(output=TRUE, headers=FALSE) {
  if (output) .e$out <- character(0)
  if (headers) .e$headers <- NULL
}

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

.opts <- function(..., disabled=FALSE) {
  l <- list(...)
  disabled <- if(isTRUE(disabled)) " disabled" else ""
  paste(
        if (length(l)) {
          n <- names(l)
          if (is.null(n) || any(n=="")) stop("Invalid unnamed argument")
          paste(unlist(lapply(seq.int(l), function(i) paste(" ",n[i],"=\"",gsub("\"","&quot;",as.character(l[[i]])[1],fixed=TRUE),"\"",sep=''))), collapse='')
        } else ""
        , disabled, sep='')
}

oselection <- function(name, text, values=text, sel.index, sel.value, size, ...) {
  if (!missing(sel.index) && !missing(sel.value)) stop("only one of 'sel.index' and 'sel.value' can be specified")
  if (!length(name)) stop("element name must be not be empty")
  name <- as.character(name)[1]
  if (missing(sel.index) && missing(sel.value)) sel.index <- integer(0)
  if (missing(sel.index)) sel.index <- !is.na(match(values, sel.value))
  size <- if (missing(size)) '' else paste(" size=\"",as.character(size)[1],"\"",sep='')
  if (!is.logical(sel.index)) sel.index <- !is.na(match(seq.int(values), sel.index))
  name <- as.character(name)[1]
  .e$out <- c(.e$out,
              paste("<select name=\"", name, "\"", size, .opts(...), ">",sep=''),
              paste("<option value=\"",gsub('"','&quot;',values,fixed=TRUE),"\"",c(""," selected")[as.integer(sel.index) + 1L],">",text,"</option>",sep='',collapse='\n'),
              "</select>")
}

oinput <- function(name, value, size, type="text", checked=FALSE, ...) {
  if (!length(name)) stop("element name must be not be empty")
  name <- as.character(name)[1]
  size <- if (missing(size)) '' else paste(" size='",as.character(size)[1],"'",sep='')
  value <- if (missing(value)) '' else paste(" value=\"",gsub('"','&quot;',as.character(value)[1]),'"',sep='')
  checked <- if (isTRUE(checked)) " checked" else ""
  .e$out <- c(.e$out, paste("<input type=\"", as.character(type)[1], "\" name=\"",name,"\"", value, size, checked, .opts(...), ">", sep=''))
}

osubmit <- function(name="submit", ...) oinput(name=name, type="submit", ...)


arequest <- function(txt, target, where, ..., attr='') {
     if (length(list(...)))
          paste("<a href='javascript:void(0);' onclick=\"javascript:req('",target,"','",where,"','",gsub("'","&#39;", paste(..., sep=''), fixed=TRUE),"');\"",attr,">",txt,"</a>",sep='')
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

# note: .e$headers are added by WebResult automatically
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


Rcpage <- function(...){	
	arg <- paste0("<div id='divparent' style='margin:0px;width:100%;text-align:center;height:100%;>")
	xx <- list(...)
	seldd <- ""
	for(i in 1:length(xx)){
	  seldd <- paste0(seldd, xx[[i]])
	}
	containerdiv <- paste0("<div id='container' class='container-fluid' style='width:1000px'>",seldd)
	sds <- paste0("<html style='width:100%;height:100%'><body style = 'width:100%; height:100%'><nav id='myNavbar' class='navbar navbar-default navbar-inverse navbar-fixed-top' style='color:white;align-content: center;vertical-align: middle;width:100%;margin-bottom: 0px;' role='navigation'>Layout1</nav>",containerdiv,"</body></html>")
	 .e$out <- c(.e$out,sds)
	#print(seldd)
}

fixedpage <- function(agr1,arg2){
  return(paste0(agr1,arg2))
}

titlePanel <- function(arg1,arg2){
  arg1 <- paste0("<div id='header' style='margin:0px;width:100%;text-align:center;height:40px;'><h4  class='page-header' style='margin-bottom:0;'>",arg1,'</h4></div>')
  return(paste0(arg1,arg2))
}

layoutTwo <- function(arg1,arg2){
  #layoutlist <- list(...)
  layoutlist <-  paste0(arg1,arg2)
  return(layoutlist)
}

sidePaneLayout <- function(...){
  w <- 300
  height <- 400
  form1 <-list(...)
  sidpane <- paste0("<div class='sidepanel' id='divsidepane' style='float:left;width:",w,"px;height:",height,"px;'>")
  for (i in 1:length(form1)) {
    sidpane <- paste0(sidpane,form1[[i]])
  }
  sidpane <- paste0(sidpane , "</div>")
  return(sidpane)
}

mainPaneLayout <- function(...){
  w <- 700
  height <- 400
  form1 <-list(...)
  mainpane <- paste0("<div class='mainpanel' id='divmainpane' style='float:left;width:",w,"px;height:",height,"px;'>")
  for (i in 1:length(form1)) {
    mainpane <- paste0(mainpane,form1[[i]])
  }
  mainpane <- paste0(mainpane , "</div>")
  return(mainpane)
}

footerPanel <- function(arg){
  footerText<-paste0("</div><footer style='width: 100%;'><div class='footer1' style='float:left;clear:both;width:100%;height:20px;text-align:center'>",arg,"</div></footer>")
  return(footerText)
}

addchart <- function(width, height, path, type)
{
	paste0("<div style='margin-top:20px;margin-left: 20px;text-align:center;'><img src='../img/",path,".",type,"' alt='rChart' width='",width,"' height='",height,"'></div>")
}

olayout <- function(r, c, wid=100, height=100, ext) {
  wid <- wid/c
  height <- height/r
  table <- "<table style='width:100%;height:100%;' >"
  for(i in 1:r) {
    table <- paste0(table, "<tr style='height:",height,ext,";width:",wid,ext,";'>")
    for(j in 1:c) {
      table <- paste0(table, paste0("<td style='border:solid 1px gray;height:",height,ext,";width:",wid,ext,";'><div id='X",i,j,"' style='border:solid 0px black;height:100%;width:100%;' ondrop='drop(event)' ondragover='allowDrop(event)' draggable='true' droppable='true'></div></td>"))
    }
  }
  table <- paste0(table, "</tr></table>")
  .e$out <- c(.e$out, table)
  #print(table)
}


oaddrbilibs <- function (...) {
   .e$out <- c(.e$out, paste0("<head><link href='https://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css' rel='stylesheet'/>",
   "<link rel='stylesheet' type='text/css' href='../css/smoothness/jquery-ui-1.8.18.custom.css'>",
   "<link rel='stylesheet' type='text/css' href='../css/main.css'>",
    "<link rel='stylesheet' type='text/css' href='../css/bootstrap.css'>",
   "<script type='text/javascript' src='http://code.jquery.com/ui/1.10.3/jquery-ui.js'></script>",
   "<script type='text/javascript' src='../js/bootstrap_utils.js'></script>",
   "<script type='text/javascript' src='../js/bootstrap-select.js'></script>",
   "<script src='http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js'></script><head>"
  ))   
}

adddropdown <- function(DDjson){
  d.json <- fromJSON(DDjson)
  seldd <- "<select>"
  for(i in 1:length(d.json)) {
    seldd <- paste0(seldd, paste0('<option id="X',i,'" value=',d.json[[i]],' >',d.json[[i]],'</option>'))
  }
  seldd <- paste0(seldd, "</select />")
  return(seldd)
}

addtable <- function(..., tab='', tr='', cs='</td><td>') {
  a <- list(...)
  if (length(a)==1 && is.list(a[[1]])) a <- a[[1]]
  ml <- max(unlist(lapply(a,length)))
  m <- matrix(unlist(lapply(a,function(x) rep(as.character(x),length.out=ml))),ml)
  tout <- unlist(lapply(1:ml, function(x) paste("<tr",tr,"><td>",paste(m[x,],collapse=cs),"</td></tr>",sep='')))
  tablecomp <- c(paste('<table class="table table-condensed" ',tab,">\n",sep=''), tout, '</table>')
  return(tablecomp)
}

bindcomp <- function(comp1_id,comp,data){
  getComp1ID <- comp1_id
  dropdownTag <- paste0("add",comp)
  drop.function <- call(dropdownTag, data)
  result <- eval(drop.function)
  finalJsTag <- paste0("<script>","$('#",getComp1ID,"').append('",result,"')","</script>")
   return(finalJsTag)  
}
