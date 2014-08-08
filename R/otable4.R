otable4 <- function(b,d,cb.id="cb",details=NULL,th,...) {
# Function that makes an unordered list of links with show/hide checkbox and CSV download
# each linking to a table in the list of data.
#
# Parameters
# b: list of named data.frames
# d: named vector of strings. Same names as b
# th: table headers. Can be missing. Check example.
# details: same as in otable2
# cb.id: same as in otable3
#
# Example usage:
# otable4(list(
#	t1=data.frame(list(a=1:3,b=2:4)),
#	t2=data.frame(list(a=1:3,b=2:4)),
#	t3=data.frame(list(a=1:3,b=2:4)),
#	t4=data.frame(list(a=1:3,b=2:4,c=2:4))
#	),
#	d={ x=c("Table 1","Table 2","Table 3","Table 4"); names(x)=c("t1","t2","t3","t4"); x },
#	th=list(
#		t1={ o=c("Column A","Column B"); names(o)=c("a","b"); o },
#		t2={ o=c("Column A","Column B"); names(o)=c("a","b"); o },
#		t3={ o=c("Column A","Column B"); names(o)=c("a","b"); o },
#		t4={ o=c("Column A","Column B","Column C"); names(o)=c("a","b","c"); o }
#	)
# )

	if(missing(d)) {
		d=names(b)
		names(d)=d
	}
	
	if(missing(th)) { th=lapply(names(b),function(a) colnames(b[[a]])); names(th)=names(b); }

	stopifnot(length(d)==length(b))
	stopifnot(length(names(d))==length(names(b)))
	stopifnot(all(sort(names(d))==sort(names(b))))
	
	out("<ul>")
	sapply(names(b),function(a) {
		out("<li>")
		if(!is.null(b[[a]])) {
			if(!is.data.frame(b[[a]])) b[[a]]=data.frame(b[[a]])
			if(nrow(b[[a]])>0) {
				otable3(b[[a]],
					tab="border=1",
					th=th[[a]],
					cb.title=sprintf("%s (%s, %1.0f rows)",d[a],variableToCsvTemp(b[[a]],"CSV"),nrow(b[[a]])),
					cb.id=sprintf("%s_%s",cb.id,a),
					details=if(!is.null(details)) details[[a]],
					...)
			}  else out(sprintf("%s: no data",d[a]))
		} else out(sprintf("%s: no data",d[a]))
		out("</li>")
	})
	out("</ul>")
}
