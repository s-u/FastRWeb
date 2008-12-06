/*
 *  CGI interface to Rserve
 *  Copyright (C) 2004,2008 Simon Urbanek, All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License or
 *  (at your option) AT&T Proprietary license.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 */

/* -- CHANGE the following two variables to match your setup -- */

#ifndef PROJECT_ROOT
#define PROJECT_ROOT "/var/FastRWeb"
#endif
#ifndef PROJECT_SOCK
#define PROJECT_SOCK "/var/FastRWeb/socket"
#endif

/* -- end of user configuirable part -- */


/* The layout (relative to the project root):
   web.R    - contains R scripts .../cgi/<foo> request is translated
              to web.R/<foo>.R (multiple extensions are supported and
              desired, e.g. ../cgi/plot.png translates to plot.png.R)
   web      - static web content that can be addressed using the "file"
              directive
   tmp      - temporary working directory, used for exchanging
              temporary files using the "tmpfile" directove

   Scripts in web.R are
    1) 'source'd
    2) global variables are preset:
       qs (query string), pars (parameter list), cmd, ct, hdr
    3) run() function is called and its return value used 

   The result value must be a character vector of either form:
    a) exactly one element - this is assumed to be text to be passed
       back with the content type text/html and encoding UTF-8
    b) two or more elements. The elements are interpreted as follows:
       [1] command - must be one of "html","file","tmpfile" or "raw"
       [2] payload - the contents(=body) to send back
       [3] content-type - HTML content type (defaults to "text/html")
       [4] header - custom headers (use with care! must contain valid
           HTML headers, e.g. cookies etc.)

   A Hello World example here would be:
   run <- function() c("html", "<b>Hello, World!</b>")

   Valid commands:
     "html" - the payload is send as the body of the response. The
              content type defaults to text/html unless overridden
              by the third parameter
     "file" - the payload is expected to be in a given file in the
              "web" directory of the project root
     "tmpfile" - the payload is expected to be in a given file in
               the "tmp" directory of the project root. Also the
               file is automatically deleted after being loaded.
     "raw" - the payload is sent as-is with no headers, i.e. the
             payload must be a valid HTTP response

*/

/* project root directory */
const char *root = PROJECT_ROOT;

/* socket name - must match the name used when starting Rserve */
const char *sock = PROJECT_SOCK;

#define MAIN         // we are the main program, we need to define this
#define SOCK_ERRORS  // we will use verbose socket errors

#include "sisocks.h"
#include "Rconnection.h"

char sfb[4096];

int main(int argc, char **argv) {
    char *pi = getenv("PATH_INFO");
    while (pi && *pi=='/') pi++;
    if (!pi || !*pi) {
      printf("Content-type: text/html\n\n<b>Error: no function or path specified.</b>\n");
      return 0;
    }
    initsocks(); // this is needed for Win32 - it does nothing on unix

    // use unix sockets
    Rconnection *rc = new Rconnection(sock, -1);
    
    int i=rc->connect();
    if (i) {
        char msg[128];
        sockerrorchecks(msg, 128, -1);
        printf("Content-type: text/html\n\n<b>Unable to connect</b> (result=%d, socket:%s).\n", i, msg);
	delete rc;
	return 0;
    }
    char *qs = getenv("QUERY_STRING");
    char *sqs = "";
    if (qs && *qs) { /* sanitize qeury string */
      sqs = (char *) malloc(strlen(qs)*2+2);
      char *c = qs, *d = sqs;
      while (*c) {
	*d=*c;
	if (*c=='\\') (d++)[0]='\\';
	else if (*c=='\'') { d[0]='\\'; (d++)[0]='\''; }
	else if (*c=='\r' || *c=='\n') *c=' ';
	c++; d++;
      }
      *d=0;
    }
    char *pii = strdup(pi);
    { char *c=pii; while (*c) { if (c[0]=='.' && c[1]=='.') *c='_'; c++;  } }
snprintf(sfb, 4096, "{setwd('%s/tmp'); library(FastRWeb); .out<-''; cmd<-'html'; ct<-'text/html'; hdr<-''; qs<-'%s'; pars<-list(); lapply(strsplit(strsplit(qs,\"&\")[[1]],\"=\"),function(x) pars[[x[1]]]<<-x[2]); if(exists('init') && is.function(init)) init(); as.character(try({source('%s/web.R/%s.R'); as.WebResult(do.call(run, pars)) },silent=TRUE))}\n", root, sqs, root, pii);
    int res = 0;
    Rstrings *x = (Rstrings*) rc->eval(sfb, &res);
    
    if (x) { // if everything was fine, we have the result
      char *cmd = x->stringAt(0);
      char *pay = x->stringAt(1);
      char *ct  = x->stringAt(2);
      char *hdr = x->stringAt(3);
      if (!ct) ct="text/html; charset=utf-8";
      if (!pay) pay="";
      if (hdr && *hdr) { /* useful for cookies etc. */
	char *c = hdr;
	while (*c) c++;
	c--;
	if (c<hdr) c=hdr;
	if (*c!='\n' && *c!='\r')
	  printf("%s\n", hdr);
	else
	  fwrite(hdr, 1, strlen(hdr), stdout);
      }
      if (cmd) {
	if (!strcmp(cmd, "file") || !strcmp(cmd, "tmpfile")) {
	  if (*pay) {
	    char buf[256];
	    if (!strcmp(cmd, "tmpfile"))
	      snprintf(buf, 256, "%s/tmp/%s", root, pay);
	    else
	      snprintf(buf, 256, "%s/web/%s", root, pay);
	    FILE *f = fopen(buf, "rb");
	    if (f) {
	      int n = 0;
	      printf("Content-type: %s\n\n", ct);
	      while (!feof(f) && (n=fread(sfb, 1, 4096, f))>0)
		fwrite(sfb, 1, n, stdout);
	      fclose(f);
	      if (!strcmp(cmd, "tmpfile")) unlink(buf);
	    } else {
	      printf("Content-type: text/html\n\nFile %s not found\n", buf);
	    }
	  } else {
	    printf("Content-type: text/html\n\nFile not specified\n");
	  }
	} else if (!strcmp(cmd, "raw")) {
	  fwrite(pay, 1, strlen(pay), stdout);
	} else if (!strcmp(cmd, "html")) {
	  printf("Content-type: %s\n\n%s\n", ct, pay);
	} else { /* fall-back for anything unexpected */
	  printf("Content-type: %s\n\n%s\n%s", ct, cmd, pay);
	}
      } else
	printf("Content-type: text/html\n\nFunction failed (no result)\n");
      delete x;
    } else {
      printf("Content-type: text/html\n\nEvaluation failed with error code %d\n", res);
    }
    
    // dispose the connection object - this implicitly closes the connection
    delete rc;
    return 0;
}
