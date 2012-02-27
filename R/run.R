.run <- function(request, root, path) {
  as.character(try({
    webapi <- 1.1
    .e$.out <- ''
    cmd <- 'html'
    ct <- 'text/html'
    setwd(file.path(root, "tmp"))

    # deprecated compatibility settings - should go away...
    requestURI <- request$uri
    remote.addr <- request$client.ip
    raw.cookies <- request$raw.cookies
    qs <- request$query.string

    # parse parameters in the order of precedence: query string, multipart, urlencoded
    pars <- list()
    if (request$c.type == 'application/x-www-form-urlencoded' && is.raw(request$body)) {
      ue <- rawToChar(request$body)
      lapply(strsplit(strsplit(ue,'&')[[1]], '='),function(x) pars[[x[1]]] <<- x[2])
    }
    if (grepl("^multipart", request$c.type)) pars <- parse.multipart()
    # add qs
    lapply(strsplit(strsplit(qs,'&')[[1]], '='),function(x) if (length(x) > 1L) pars[[x[1]]] <<- x[2])
  
    # find the script
    request$path.info <- ''
    sfn <- sprintf("%s/web.R/%s.R", root, path)
    if (!file.exists(sfn)) { # if the file doesn't exist, we try to separate script name from PATH_INFO
      left <- path
      while (nzchar(left <- gsub("/[^/]+$", "", left)) && !file.exists(cand <- sprintf("%s/web.R/%s.R", root, left))) {}
      if (!nzchar(left))
        return(c("html", paste("Script ", path, ".R not found", sep=''), "text/html", "Status: 404 Script not found"))
      request$path.info <- gsub(left, "", path, fixed=TRUE)
      sfn <- cand
    }

    if(exists('init') && is.function(init)) init()

    source(sfn, local=TRUE)
    as.WebResult(do.call(run, pars))
  }, silent=TRUE))
}
