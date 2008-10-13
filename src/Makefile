all:
	@echo 'compiling Rcgi'
	@(cd Rcgi && $(MAKE))
	rm -rf inst/Rcgi
	cp -r Rcgi inst/
	mkdir inst/cgi-bin
	@(if test -e Rcgi/Rcgi; then cp Rcgi/Rcgi inst/cgi-bin/; fi)
	@(if test -e Rcgi/Rcgi.exe; then cp Rcgi/Rcgi.exe inst/cgi-bin/; fi)
	touch null.so null.dll
