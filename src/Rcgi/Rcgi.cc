/*
 *  CGI interface to Rserve
 *  Copyright (C) 2004,2008,2011 Simon Urbanek, All rights reserved.
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

/* -- CHANGE the following two variables to match your setup or define
   PROJECT_ROOT and RSERVE_SOCK -- */

/* project root directory */
#ifndef PROJECT_ROOT
#define PROJECT_ROOT "/var/FastRWeb"
#endif
const char *root = PROJECT_ROOT;

/* socket name - must match the name used when starting Rserve (not used on Windows) */
#ifndef RSERVE_SOCK
#define RSERVE_SOCK "/var/FastRWeb/socket" 
#endif
const char *sock = RSERVE_SOCK;

/* cookie to log in the log files */
#ifndef LOG_COOKIE
#define LOG_COOKIE "userID="
#endif


/* NOTE: you can define RSERVE_HOST and (optionally ) RSERVE_PORT if you
         want to use TCP instead of unix sockets */

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

#define MAIN         // we are the main program, we need to define this
#define SOCK_ERRORS  // we will use verbose socket errors

#include "sisocks.h"
#include "Rconnection.h"
#include <sys/time.h>

#ifdef WIN32 // on Windows we have to turn stdout into binary mode
#include <io.h>
#include <fcntl.h>
#endif

char sfb[256*1024]; /* it should be big enough since we pass cookies as part of the command */

struct timeval startT, stopT;

/* log access to the log/cgi.log file - if possible */
static void wlog(const char *cmd, const char *info) {
    char wfn[512], idb[16], *cookie_end = 0;
  snprintf(wfn, 512, "%s/logs/cgi.log", root);
  gettimeofday(&stopT, 0);
  double t1 = (double) startT.tv_usec; t1 *= 0.000001; t1 += (double) startT.tv_sec;
  double t2 = (double) stopT.tv_usec; t2 *= 0.000001; t2 += (double) stopT.tv_sec;
  t2 -= t1;
  FILE *f = fopen(wfn, "a");
  if (!f) return;
  /* you can tag your logs with some cookie content - as an exmaple we use userID cookie */
  char *s = getenv("HTTP_COOKIE");
  if (s) {
      s = strstr(s, LOG_COOKIE);
      if (s) {
	  cookie_end = s;
	  while (*cookie_end && *cookie_end != ';') cookie_end++;
	  if (*cookie_end) *cookie_end = 0; else cookie_end = 0;
      }
  } else s = (char*) "";
  if (!s) s = (char*) "";
  fprintf(f,"%u\t%.2f\t%s\t%s\t%s\t%s\t%s\t%s\n",
	  (unsigned int) time(0),
	  t2,
	  getenv("REMOTE_ADDR"),
	  s,
	  getenv("REQUEST_URI"),
	  cmd,
	  info,
	  getenv("HTTP_USER_AGENT"));
  if (cookie_end) *cookie_end = ';'; /* restore cookie trailing char if it was not the last cookie */
  fclose(f);
}

/* creates a sanitized copy of the string - escape \, ', " and replace \r or \n by ' ' */
static char *sanitize(const char *src) {
    char *dst = (char *) malloc(strlen(src) * 2 + 2); /* at most every character will be replaced */
    const char *c = src;
    char *d = dst;
    while (*c) {
	*d = *c;
	if (*c == '\\') *(++d) = '\\';
	else if (*c == '\'') { *(d++) = '\\'; *d = '\''; }
	else if (*c == '\r' || *c == '\n') *d = ' ';
	c++; d++;
    }
    *d = 0;
    return dst;
}

int main(int argc, char **argv) {
    gettimeofday(&startT, 0);
    char *pi = getenv("PATH_INFO");
    while (pi && *pi=='/') pi++; /* skip leading slashes in PATH_INFO */
    if (!pi || !*pi) {
		printf("Content-type: text/html\nStatus: 400 No function or path specified\n\n<b>Error: no function or path specified.</b>\n");
		return 0;
    }
    initsocks(); // this is needed for Win32 - it does nothing on unix

#ifdef RSERVE_HOST /* did the user request a specific TCP host ? */
#ifdef RSERVE_PORT /* is there also a custom port ? */
    Rconnection *rc = new Rconnection(RSERVE_HOST, RSERVE_PORT);    
#else /* custom host, but default port */
    Rconnection *rc = new Rconnection(RSERVE_HOST);    
#endif
#else /* no custom host - use default behavior */
#ifdef Win32 // no unix sockets, use local TCP/IP
    Rconnection *rc = new Rconnection();
#else /* default on unix is socket */
    // use unix sockets
    Rconnection *rc = new Rconnection(sock, -1);
#endif
#endif
	// try to connect
    int i=rc->connect();
    if (i) { // show error page on failure
        char msg[256];
		snprintf(msg, 256, "%s/web/connection_down.html", root);
		FILE *f = fopen(msg, "r");
		if (f) { /* if web/connection_down.html exists, send it */
			int n = 0;
			while ((n = fread(sfb, 1, sizeof(sfb), f)) > 0)
				fwrite(sfb, 1, n, stdout);
			fclose(f);
			delete rc;
			return 0;
		}
        sockerrorchecks(msg, 256, -1);
        printf("Content-type: text/html\nStatus: 500 Cannot connect to Rserve\n\n<b>Unable to connect to Rserve</b> (result=%d, socket:%s).\n", i, msg);
		delete rc;
		return 0;
    }
	
    /* we need to forward QUERY_STRING, REQUEST_URI and HTTP_COOKIE to Rserve since it has
       no access to the CGI environemnt variables as it's in a separate progess */
    char *qs = getenv("QUERY_STRING");
    char *sqs = (qs && *qs) ? sanitize(qs) : (char*) "";

    char *rqs = getenv("REQUEST_URI");
    char *srqs = (rqs && *rqs) ? sanitize(rqs) : (char*) "";
	
    char *cook = getenv("HTTP_COOKIE");
    char *scook = (char*) "";
    if (cook && *cook) { /* sanitize by URI-encoding dangerous characters */
      scook = (char*) malloc(strlen(cook) * 3 + 3); /* very conservative estimate */
      char *c = cook, *d = scook;
      while (*c) {
	  if (*c < ' ' || *c == '\\' || *c == '\"' || *c == '\'') {
	      snprintf(d, 4, "%%%02x",(int)((unsigned char)*c));
	      d += 2;
	  } else *d = *c;
	  c++; d++;
      }
      *d = 0;
    }

    char *pii = strdup(pi); /* sanitize path: replace .. by _. */
    { char *c=pii; while (*c) { if (c[0]=='.' && c[1]=='.') *c='_'; c++;  } }

    char *client_ip = getenv("REMOTE_ADDR");
    if (!client_ip) client_ip = (char*) "";

    char *method = getenv("REQUEST_METHOD");
    if (!method) method = (char*) "GET";

    char *rctype = getenv("CONTENT_TYPE");
    if (!rctype) rctype = (char*) ""; 
	
    int rclen = -1;
    char *rcl = getenv("CONTENT_LENGTH");
    if (rcl && *rcl >= '0' && *rcl <= '9') rclen = atoi(rcl);

    if (rclen > -1) {
	char *data = (char*) malloc(rclen + 8), *dptr = data;
	if (!data) { printf("Content-type: text/html\nStatus: 500 Out of memory\n\nERROR: cannot allocate memory for request body\n"); return 1; }
	/* the current CXX client doesn't have a class for RAW so we use the low-level Rexp class for that */
	int dp = itop(rclen), to_go = rclen;
	memcpy(data, &dp, sizeof(int));
	dptr += sizeof(int);
	while (to_go > 0) {
	    int n = fread(dptr, 1, to_go, stdin);
	    if (n < 0) { printf("Content-type: text/html\nStatus: 400 request body read error\n\nERROR: read error reading request body.\n"); return 1; }
	    to_go -= n;
	    dptr += n;
	}
	Rexp *r_body = new Rexp(XT_RAW, data, dptr - data);
	rc->assign("request.body", r_body);
	delete r_body;
    }

    /* create R code to evaluate */
    snprintf(sfb, sizeof(sfb),
	     "{library(FastRWeb);"					\
	     "request<-list(uri='%s', method='%s', c.type='%s', c.length=%d, body=.GlobalEnv$request.body, client.ip='%s', query.string='%s', raw.cookies='%s'); FastRWeb:::.run(request,'%s','%s')}",
	     srqs, method, rctype, rclen, client_ip, sqs, scook, root, pii);

	/* Note: for efficientcy we don't parse cookies. Use getCookies() to populate cookies. */
	int res = 0;
	
	/* evaluate the constructed code */
    Rstrings *x = (Rstrings*) rc->eval(sfb, &res);
    
    if (x) { // if everything was fine, we have the result
		char *cmd = x->stringAt(0);
		char *pay = x->stringAt(1);
		char *ct  = x->stringAt(2);
		char *hdr = x->stringAt(3);
		if (!ct) ct = (char*) "text/html; charset=utf-8";
		if (!pay) pay = (char*) "";
		if (hdr && *hdr) { /* useful for cookies etc. */
			char *c = hdr;
			while (*c) c++;
			c--;
			if (c < hdr) c = hdr; /* for empty strings */
			if (*c!='\n' && *c!='\r') /* if the header doesn't end with \n or \r then add \r\n */
				printf("%s\n", hdr);
			else
				fwrite(hdr, 1, strlen(hdr), stdout);
		}
		if (cmd) {
			if (!strcmp(cmd, "file") || !strcmp(cmd, "tmpfile")) {
				wlog(cmd, pay);
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
#ifdef WIN32
						/* continue with payload in binary mode */
						setmode(fileno(stdout), O_BINARY);
#endif
						while (!feof(f) && (n=fread(sfb, 1, 4096, f))>0)
							fwrite(sfb, 1, n, stdout);
						fclose(f);
						if (!strcmp(cmd, "tmpfile")) unlink(buf);
					} else {
						printf("Content-type: text/html\nStatus: 404 File not found\n\nFile %s not found\n", buf);
					}
				} else {
					printf("Content-type: text/html\nStatus: 500 script result error - file not specified\n\nFile not specified\n");
				}
			} else if (!strcmp(cmd, "raw")) {
				wlog(cmd, "");
				fwrite(pay, 1, strlen(pay), stdout);
			} else if (!strcmp(cmd, "header")) {
				wlog(cmd, hdr ? hdr : "<empty-header>");
				printf("\n"); /* add another \n to terminate the header */
			} else if (!strcmp(cmd, "html")) {
				wlog(cmd, "");
				printf("Content-type: %s\n\n%s\n", ct, pay);
			} else { /* fall-back for anything unexpected */
				wlog("none",cmd);
				printf("Content-type: %s\n\n%s\n%s", ct, cmd, pay);
			}
		} else {
			wlog("empty","");
			printf("Content-type: text/html\nStatus: 500 R function failed (no result)\n\nFunction failed (no result)\n");
		}
		delete x;
    } else {
		char ef[16];
		snprintf(ef, 16, "%d", res);
		wlog("efail",ef);
		printf("Content-type: text/html\nStatus: 500 Evaluation failed\n\nEvaluation failed with error code %d\n", res);
    }
    
    // dispose of the connection object - this implicitly closes the connection
    delete rc;
    return 0;
}
