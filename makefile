# makefile
#

export TARGET_OS ?= $(shell uname)
export TARGET_OS := $(or $(filter $(TARGET_OS), Linux),\
                         $(filter $(TARGET_OS), Darwin),\
                         $(error Invalid TARGET_OS: $(TARGET_OS)))

# In local.config export the following environment variables
# LLVM_CONFIG_PATH - path to llvm-config
# CLASP_HOME - path to clasp
# CLASP_RUNTIME  (boehm | mps)
include $(wildcard local.config)

# These are things that I'm hardcoding for now
# but should be set up better using waf
#
# CLASP_GC_FILENAME defines the interface to the GC
# the name depends on what extensions are in this
# version of clasp
export CLASP_GC_FILENAME='"clasp_gc.cc"'

export CLANG_DIR := $(shell $(LLVM_CONFIG_PATH) --bindir)
export CLANG := $(CLANG_DIR)/clang++
export LLVM_INCLUDE_DIR := $(shell $(LLVM_CONFIG_PATH) --includedir)

# setup the clasp executable
export CLASP = $(CLASP_HOME)/build/$(CLASP_RUNTIME)/cclasp-$(CLASP_RUNTIME)

# These are things that I'm hardcoding for now
# but should be set up better using waf
#
# -DNDEBUG suppresses link_compatibility checking in clbind that always fails - must fix


export OPTIONS = -v -I$(CLASP_HOME)/include \
		-I$(CLASP_HOME)/src/main \
		-I$(CLASP_HOME)/include/clasp/main \
		-I$(CLASP_HOME)/build/$(CLASP_RUNTIME) \
		-I$(CLASP_HOME)/build/$(CLASP_RUNTIME)/generated \
		-I$(LLVM_INCLUDE_DIR) \
		-c -emit-llvm \
		-std=c++11 \
		-Wno-macro-redefined \
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
	$(CLANG) -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o hello-world-cxx.bc hello-world-cxx.cc
	llvm-dis helloWorld.bc


clean:
	(cd hello-world; rm -f *.bc *.so *.ll)
	(cd double-vector; rm -f *.bc *.so *.ll)
