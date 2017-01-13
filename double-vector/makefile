# makefile
#

all:
	$(CLANG) $(OPTIONS) -o doubleVector.bc doubleVector.cc


test: doubleVector.bundle
	$(CLASP_BOEHM) -l double-vector.lsp
