# makefile
#

hello-world-cxx.bc:
	$(CLANG) $(OPTIONS) -o hello-world-cxx.bc hello-world-cxx.cc


test: hello-world-cxx.bc
	$(CLASP) -l hello-world.lsp
