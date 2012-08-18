.run <- function(request, root, path) {
  as.character(try({
    .GlobalEnv$webapi <- 1.1
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
      lapply(strsplit(strsplit(ue,'&')[[1]], '='),function(x) pars[[URLdecode(x[1])]] <<- URLdecode(x[2]))
    }
    if (grepl("^multipart", request$c.type)) pars <- parse.multipart()
    # add qs
    lapply(strsplit(strsplit(qs,'&')[[1]], '='),function(x) if (length(x) > 1L) pars[[URLdecode(x[1])]] <<- URLdecode(x[2]))
  
    # find the script
    request$path.info <- ''
    sfn <- sprintf("%s/web.R/%s.R", root, path)
    if (!file.exists(sfn)) { # if the file doesn't exist, we try to separate script name from PATH_INFO
      left <- path
      while (nzchar(left <- gsub("/[^/]*$", "", left)) && !file.exists(cand <- sprintf("%s/web.R/%s.R", root, left))) { if (!grepl("/", left)) left <- '' }
      if (!nzchar(left))
        return(c("html", paste("Script ", path, ".R not found", sep=''), "text/html", "Status: 404 Script not found"))
      request$path.info <- gsub(left, "", path, fixed=TRUE)
      sfn <- cand
    }

    .GlobalEnv$request <- request
    if(exists('init') && is.function(init)) init()

    source(sfn, local=TRUE)
    as.WebResult(do.call(run, pars))
  }, silent=TRUE))
}


## URLencode is *not* vectorized in R, believe it or not so we have to work around that ...
URLenc <- function(x) unlist(lapply(x, URLencode))

### this maps the Rhttpd/Rserve direct HTTP API into .run
.http.request <- function(url, query, body, headers) {
  root <- getOption("FastRWeb.root")
  if (is.null(root)) root <- "/var/FastRWeb"
  # FIXME: this is somewhat stupid - we already have the decoded query and we have to re-encode it
  #        we should create a back-door for encoded queries ...
  query <- if (is.null(query)) '' else paste(URLenc(names(query)),"=",URLenc(query),collapse='&',sep='')
  request <- list(uri=url, method='GET', c.type='', c.length=-1, body=NULL, client.ip='0.0.0.0', query.string=query, raw.cookies='')
  # this is a bit convoluted - the HTTP already parses the body - disable it where you can
  if (!is.raw(body)) {
    if (length(body)) {
      sb <- paste(unlist(lapply(names(body), function(x) paste(URLencode(x),"=",URLencode(as.character(body[[x]])),sep=''))),collapse='&')
      request$body <- charToRaw(sb)
      request$c.length <- length(request$body)
      request$c.type <- 'application/x-www-form-urlencoded'
    }
  } else {
    request$body <- body
    request$c.type <- attr(body, "content-type")
    request$c.length <- length(body)
  }
  # FIXME: we are ignoring headers ...
  r <- .run(request, root, url)
  if (length(r) < 2) return(list(r))
  cmd <- r[1]
  payload <- r[2]
  ct <- if (length(r) > 2) r[3] else "text/html"
  h <- if (length(r) > 3) r[4] else character(0)
  if (any(nchar(h) == 0L)) h <- h[nchar(h) > 0]
  if (cmd == "tmpfile" || cmd == "file") {
    fn <- paste(root, if (cmd == "tmpfile") "tmp" else "web", gsub("/", "_", payload, fixed=TRUE), sep='/')
    list(file=fn, ct, h)
  } else list(payload, ct, h)
}
