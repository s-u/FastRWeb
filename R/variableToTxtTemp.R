variableToTxtTemp <- function(x,label) {
# generates html link to temporary txt file containing the strings in x
# Example usage: variableToCsvTemp(x=c("lorem ipsum","bla bla 12312 Bleiwe","ya hwayda lah"),label="Txt file")

	if(!is.null(x)) {
		civ.f=sprintf("%s.txt",tempfile(tmpdir="/var/FastRWeb/tmp"))
		write(x,file=civ.f)
		o=sprintf("<a href='/cgi-bin/R/tmp?file=%s&mime=text/plain'>%s</a>",basename(civ.f),label)
	} else {
		o=sprintf("%s: no data",label)
	}
o
}
