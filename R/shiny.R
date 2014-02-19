# thiny only has append, it doesn't have prepend so we have to create it
tagPrependChild <- function(tag, child) {
  tag$children <- c(list(child), tag$children)
  tag
}

## fix up shiny page ...
auto.submit <- function(x) { x<-tagAppendChild(x, tags$script('$("#form").change(function() { $("#form").submit() }); var f=document.forms[0]; for (var i=0;f.elements[i]!=null;i++) f.elements[i].name=f.elements[i].id;')); x[[1]]<-tagPrependChild(x[[1]], tags$script(src="/shared/jquery.js")); x }
