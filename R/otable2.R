otable2 <- function (a, 
	tab = "", tr = "", cs = "</td><td style='text-align:right'>", th, do.out=TRUE, 
	prettyNumCols=NULL, pncd=2, details=NULL, detailsFun=variableToXlsxTemp, ...
)	
# based on otable from the FastRWeb package, but with option for table headers
# detailsFun: variableToXlsxTemp, exportToEurexMarginCalculator
# Example usage:
# otable2(
#	a=data.frame(list(a=1:3,b=c(2,3.123123,1231412.1231))), # data.frame to convert to html table
#	tab="border=1", # attributes to add to the "table" html tag
#	tr="height=5", # attributes to add to the "tr" html tag. Already featured in the original otable function from FastRWeb
#	th=c("letter a","letter b"), # table headers
#	prettyNumCols=c("b"), # which columns to prettify using the "formatC" function call formatC(...,big.mark=",",digits=pncd,format="f")
#	pncd=3, # number of digits to use in the formatC call (along with the prettyNumCols parameter)
#	details=list(
#		list(table=data.frame(list(c1=1:5,c2=3:7,c3=5:9)),descr="More details"),
#		list(table=data.frame(list(c1=1:5,c2=3:7,c3=5:9)),descr="More details"),
#		list(table=data.frame(list(c1=1:5,c2=3:7,c3=5:9))descr="More details")
#	), # list of length equal to the nrow of data.frame passed to otable (via parameter "a"). This creates an extra column in the table with a link to an excel file or csv file, usually to more details pertaining to this row in particular. It is passed to the detailsFun function (also a parameter in this function)
#	detailsFun=function(b,d,label,...) variableToCsvTemp(x=b,label=label,...),
#	do.out=FALSE, # false to return the html string, true to output it via the "out" function from FastRWeb
#	)
{
	#a <- list(...)
	#if (length(a) == 1 && is.list(a[[1]]))  a <- a[[1]]
	stopifnot(is.data.frame(a))
	
	# preprocess numeric columns in a to be pretty
	if(!is.null(prettyNumCols)) {
		if(any(!prettyNumCols%in%colnames(a))) warning(sprintf("otable2: Some columns in prettyNumCols are not in the table: %s",paste(setdiff(prettyNumCols,colnames(a)),collapse=",")))
		prettyNumCols=intersect(prettyNumCols,colnames(a))
		if(length(prettyNumCols)>0) {
			# a[,prettyNumCols]=prettyNum(a[,prettyNumCols],big.mark=",",digits=pncd,format="f")
			for(pnc in prettyNumCols) {
				a[,pnc]=unlist(lapply(a[,pnc],function(x) formatC(x,big.mark=",",digits=pncd,format="f")))
			}
		}
	}
	
	if(!missing(th)) {
		tout1 <- paste( "<tr><th>", paste(th, collapse = "</th><th>"), "</th></tr>", sep = "")
	} else tout1=""

	#
	ml <- nrow(a)
	if(ml>0) {
		m <- matrix(unlist(lapply(a, function(x) rep(as.character(x), length.out = ml))), ml)
		tout <- unlist(lapply(1:ml, function(x) paste(
			"<tr ", tr, ">",
			"<td style='text-align:right'>", paste(m[x, ], collapse = cs), "</td>",
			if(!is.null(details)) {
				if(!is.null(details[[x]])) {
					sprintf("<td>%s</td>",detailsFun(b=details[[x]]$table,d=details[[x]]$descr,label="xlsx",...))
				} else {
					""
				}
			} else {
				""
			},
			"</tr>", 
			sep = "")))
	} else tout=""
	
	ooo<- c(paste("<table ", tab, ">\n", sep = ""), 
		tout1, tout, "</table>")
	
	# return
	if(do.out) out(ooo) else ooo
}
