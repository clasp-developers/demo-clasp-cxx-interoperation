# makefile
#

double-vector-cxx.bc:
	$(CLANG) $(OPTIONS) -o double-vector-cxx.bc double-vector-cxx.cc


test: double-vector-cxx.bc
	$(CLASP) -l double-vector.lsp
