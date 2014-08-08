variableToXlsxTemp <- function(b,d,label,return.fn=FALSE,...) {
# inspired by otable4 and variableToCsvTemp.
# b: list of named data.frames
# d: named vector of strings. Same names as b
# return.fn: TRUE to return the filename used, FALSE to return a string with the "a" tag (html link)
# function that makes an xlsx with sheets being each table in the list of data
# Requires the "xlsx" package
#
# Example usage:
# variableToXlsxTemp(
#	b=list(x=data.frame(list(a=1:3,b=2:4)),y=data.frame(list(a=1:3,b=2:4))),
#	d={ x=c("Table 1","Table 2"); names(x)=c("x","y"); x },
#	label="Xlsx file",
#	return.fn=TRUE
# )


	if(missing(d)) {
		d=names(b)
		names(d)=d
	}
	
	stopifnot(length(d)==length(b))
	stopifnot(length(names(d))==length(names(b)))
	stopifnot(all(sort(names(d))==sort(names(b))))

	library(xlsx)
	wb <- createWorkbook()
	sapply(names(b),function(a) {
	   sheet  <- createSheet(wb, sheetName=as.character(d[a]))
	   cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()  # header
		if(!is.null(b[[a]])) {
			if(!is.data.frame(b[[a]])) b[[a]]=data.frame(b[[a]])
			addDataFrame(b[[a]], sheet, startRow=1, startColumn=1, colnamesStyle=cs3, row.names=FALSE)
		}
	})

	# Don't forget to save the workbook ...
	civ.f=sprintf("%s.xlsx",tempfile(tmpdir="/var/FastRWeb/tmp"))
	saveWorkbook(wb, civ.f)
	
	if(!return.fn) {
		o=sprintf("<a href='/cgi-bin/R/tmp?file=%s&mime=application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'>%s</a>",basename(civ.f),label)
		o
	} else {
		civ.f
	}
}
