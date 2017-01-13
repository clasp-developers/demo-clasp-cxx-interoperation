# makefile
#
export CLASP_DEMO_HOME = $(shell pwd)

export TARGET_OS ?= $(shell uname)
export TARGET_OS := $(or $(filter $(TARGET_OS), Linux),\
                         $(filter $(TARGET_OS), Darwin),\
                         $(error Invalid TARGET_OS: $(TARGET_OS)))

# In local.config export the following environment variables
# EXTERNALS_CLASP_DIR
# CLASP_HOME
# CLASP_RUNTIME  (boehm, mps, boehmdc)

include $(wildcard local.config)

# setup the clasp executable
export CLASP = $(CLASP_HOME)/build/$(CLASP_RUNTIME)/cclasp-$(CLASP_RUNTIME)

# These are things that I'm hardcoding for now
# but should be set up better using waf
#
# CLASP_GC_FILENAME defines the interface to the GC
# the name depends on what extensions are in this
# version of clasp
export CLASP_GC_FILENAME='"clasp_gc.cc"'

ifeq ($(CLANG),)
export CLANG = $(EXTERNALS_CLASP_DIR)/build/release/bin/clang++
endif

export OPTIONS = -I$(CLASP_HOME)/include \
		-I$(CLASP_HOME)/src/main \
		-I$(CLASP_HOME)/build/$(CLASP_RUNTIME) \
		-c -emit-llvm \
		-std=c++11 \
		-Wno-deprecated-register \
		-Wno-inconsistent-missing-override

all:
	(cd hello-world; make $*)
	(cd double-vector; make $*)

test:
	(cd hello-world; make test)
	(cd double-vector; make test)

dv:
	(cd double-vector; make $*)

all-linux:
	make TARGET_OS=linux

all-darwin:
	make TARGET_OS=darwin


#
# Run the demos after typing:    make shell
#    - this will set CLASP_DEMO_HOME and the demos will find the bitcode files
#
shell:
	echo Now CLASP_DEMO_HOME is set to $(CLASP_DEMO_HOME)
	bash


ir:
	clang++ -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bc helloWorld.cc
	llvm-dis helloWorld.bc


clean:
	(cd hello-world; rm -f *.bc *.so *.ll)
	(cd double-vector; rm -f *.bc *.so *.ll)
