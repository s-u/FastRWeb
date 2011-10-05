INST=../inst
CGIBIN=$(INST)/cgi-bin

all:
	@echo 'compiling Rcgi'
	@if test ! -e Rcgi/Makefile; then cp Rcgi/Makefile.win Rcgi/Makefile; echo ' - using Windows setup'; fi
	@(cd Rcgi && $(MAKE))
	rm -rf $(INST)/Rcgi
	cp -r Rcgi $(INST)
	mkdir $(CGIBIN)
	@(if test -e Rcgi/Rcgi; then cp Rcgi/Rcgi $(CGIBIN)/; fi)
	@(if test -e Rcgi/Rcgi.exe; then cp Rcgi/Rcgi.exe $(CGIBIN)/; fi)
	touch null.so null.dll # to make R CMD SHLIB happy
