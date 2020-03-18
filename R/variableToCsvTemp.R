variableToCsvTemp <- function(x,label,row.names=FALSE) {
# generates html link to temporary csv file containing the data.frame x
# Example usage: variableToCsvTemp(x=data.frame(list(a=1:3,b=2:4)),label="Csv file")

	if(!is.null(x)) {
		civ.f=sprintf("%s.csv",tempfile(tmpdir="/var/FastRWeb/tmp"))
		write.csv(x, file=civ.f, row.names=row.names, na = "")
		o=sprintf("<a href='/cgi-bin/R/tmp?file=%s&mime=text/csv'>%s</a>",basename(civ.f),label)
	} else {
		o=sprintf("%s: no data",label)
	}
o
}
