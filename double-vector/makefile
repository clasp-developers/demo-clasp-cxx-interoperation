# makefile
#

all: doubleVector.bundle

doubleVector.bundle: doubleVector.cc
	$(CLANG) $(OPTIONS) -o doubleVector.bundle doubleVector.cc


test: doubleVector.bundle
	$(CLASP_BOEHM) -l double-vector.lsp
