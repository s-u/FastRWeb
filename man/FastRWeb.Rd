\name{FastRWeb}
\alias{FastRWeb}
\alias{FastRWeb-package}
\title{
  FastRWeb - infratructure to serve web pages with R scripts efficiently
}
\description{
  \bold{FastRWeb} is not just a package, but an entire infrastructure
  allowing the use of R scripts to create web pages and graphics.

  The basic idea is that an URL of the form
  \code{http://server/cgi-bin/R/foo?bar=value} will be processed by FastRWeb
  such as to result in sourcing of the \code{foo.R} script and running
  the function \code{run(bar="value")} which is expected to be defined
  in that script. The results of a script can be anything from HTML
  pages to bitmap graphics or PDF document.

  \bold{FastRWeb} uses CGI or PHP as front-end and \code{Rserve} server
  as the back-end. For details see \cite{Urbanek, S. (2008)
  \emph{FastRWeb: Fast Interactive Web Framework for Data Mining Using R},
  IASC 2008}.
  
  The R code in the package itself provides R-side tools that
  facilitate the delivery of results to a browser - such as
  \code{\link{WebResult}}, \code{\link{WebPlot}}, \code{\link{out}},
  \code{\link{done}} - more in detail below.
}
\section{Installation}{
  The default configuration of FastRWeb assumes that the project root
  will be in \code{/var/FastRWeb} and that the server is a unix
  machine. It is possible to install FastRWeb in other settings, but it
  will require modification of the configuration.

  First, the \code{FastRWeb} package should be installed (typically
  using \code{install.packages("FastRWeb")} in R). The installed package
  contains shell script that will setup the environment in
  \code{/var/FastRWeb}. To run the script, use
  
  \code{system(paste("cd",system.file(package="FastRWeb"),"&& install.sh"))}

  For the anatomy of the \code{/var/FastRWeb} project root see below.

  Once created, you can inspect the Rserve configuration file
  \code{/var/FastRWeb/code/rserve.conf} and adjust it for your needs if
  necessary. You can also look a the Rserve initialization script
  located in \code{/var/FastRWeb/code/rserve.R} which is used to pre-load
  data, packages etc. into Rserve. If you are happy with it, you can
  start Rserve using \code{/var/FastRWeb/code/start}

  In order to tell your webserver to use FastRWeb, you have two options:
  CGI script or PHP script. The former is more common as it works with
  any web server. The FastRWeb R package builds and installs the Rcgi
  script as part of its installation process into the \code{cgi-bin}
  directory of the package, but it has no way of knowing about the
  location of your server's \code{cgi-bin} directory, so it is left to
  the user to copy the script in the proper location.
  Use \code{system.file("cgi-bin", package="FastRWeb")} in R to locate
  the package directory - it will contain an executable \code{Rcgi} (or
  \code{Rcgi.exe} on Windows) and copy that executable into you server's
  \code{cgi-bin} directory (on Debian/Ubuntu this is typically
  \code{/usr/lib/cgi-bin}, on Mac OS X it is
  \code{/Library/WebServer/CGI-Executables}). Most examples in FastRWeb
  assume that you have renamed the script to \code{R} instead of
  \code{Rcgi}, but you can choose any name.

  With Rserve started and the CGI script in place, you should be able to
  open a browser and run your first script, the URL will probably look
  something like \code{http://my.server/cgi-bin/R/main}.
  This will invoke the script \code{/var/FastRWeb/web.R/main.R} by
  sourcing it and running the \code{run()} function.

  For advanced topics, please see \code{Rserve} documentation. For
  production systems we encourage the use of \code{gid}, \code{uid},
  \code{sockmod} and \code{umask} configuration directives to secure the
  access to the Rserve according to your web server configuration.
}
\section{Project root anatomy}{
  The project root (typically \code{var/FastRWeb}) contains various
  directories:

  \itemize{
    \item \code{web.R} - this directory contains the R scripts that will
    be served by FastRWeb. The URL is parsed such that the path part
    after the CGI binary is taken, \code{.R} appended and serves to
    locate the file in the \code{web.R} directory. Once located, it is
    sourced and the \code{run()} function is called with query strang
    parsed into its arguments. The default installation also sources
    \code{common.R} in addition to the specified script (see
    \code{code/rserve.R} and the \code{init()} function for details on
    how this is achieved - you can modify the behavior as you please).

    \item \code{web} - this directory can contain static content that
    can be referenced using the \code{"file"} command in
    \code{\link{WebResult}}.
    
    \item \code{code} - this directory contains supporting
    infrastructure and configurations files in association with the
    Rserve back-end. If the \code{start} script in this directory is
    used, it loads the \code{rserve.conf} configuration file and sources
    \code{rserve.R} as initialization of the Rserve master. The
    \code{init()} function (if present, e.g., defined in \code{rserve.R})
    is run on every request.

    \item \code{tmp} - this directory is used for temporary
    files. It should be purged occasionally to prevent accumulation of
    temporary files. FastRWeb provides ways of cleanup (e.g., see
    \code{"tmpfile"} command in \code{\link{WebResult}}), but crashed or
    aborted requests may still leave temporary files around. Onyl files
    from this directory can be served using the \code{"tmpfile"}
    \code{\link{WebResult}} command.

    \item \code{logs} - this directory is optional and if present, the
    \code{Rcgi} script will log requests in the \code{cgi.log} file in
    this directory. It records the request time, duration, IP address,
    \code{\link{WebResult}} command, payload, optional cookie filter and
    the user-agent. If you want to enable logging, simply create the
    \code{logs} directory with sufficient permissions to allow the Rcgi
    script to write in it.

    \item \code{run} - this directory is optional as well and used for
    run-time systems such as global login authorization etc. It is not
    populated or used in the CRAN version of FastRWeb, but we encourage
    this structure for any user-defined subsystems.
  }

  In addition, the default configuration uses a local socket of the name
  \code{socket} to communicate with the Rserve instance. Note that you
  can use regular unix permissions to limit the access to Rserve this
  way.
}
\seealso{
  \code{\link{WebResult}}, \code{\link{WebPlot}}, \code{\link{out}},
  \code{\link{done}}, \code{\link{add.header}}
}
%\examples{
%}
\keyword{interface}
