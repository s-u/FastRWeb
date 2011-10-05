# Makefile for C++ Rserve clients - Win32 version
#
# $Id: Makefile.win 203 2007-05-10 18:12:06Z urbanek $

SRC = $(wildcard *.cc)
OBJ = $(SRC:%.cc=%.o)
DST = Rcgi.exe

CXXFLAGS=-I../include -I../include/Win32 -I.. -I. -DWin32
LIBS=-lwsock32
linkRL=

all: $(DST)

Rcgi.exe: Rconnection.o Rcgi.o
	$(CXX) $^ -o $@ $(LIBS)

debug:
	CXXFLAGS=-g $(MAKE) all

clean:
	rm -rf *~ *.o \#* .\#* $(DST)
