# makefile
#

export TARGET_OS ?= $(shell uname)
export TARGET_OS := $(or $(filter $(TARGET_OS), Linux),\
                         $(filter $(TARGET_OS), Darwin),\
                         $(error Invalid TARGET_OS: $(TARGET_OS)))

include $(wildcard local.config)

# These are things that I'm hardcoding for now
# but should be set up better using waf
#
# CLASP_GC_FILENAME defines the interface to the GC
# the name depends on what extensions are in this
# version of clasp
export CLASP_GC_FILENAME='"clasp_gc.cc"'
# GC sets the name of the GC
export GC=boehm


ifeq ($(CLANG),)
export CLANG = $(EXTERNALS_CLASP_DIR)/build/release/bin/clang++
endif

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
# -DNDEBUG suppresses link_compatibility checking in clbind that always fails - must fix


export COMMON_OPTIONS = -v \
			-DX86_64 \
			-D_ADDRESS_MODEL_64 \
			-I$(CLASP_HOME)/include/ \
			-I$(CLASP_HOME)/src/main \
			-I$(CLASP_HOME)/build/$(GC) \
			$(CLASP_HOME)/build/$(GC)/fasl/$(GC)-all-cxx.a \
			-I $(EXTERNALS_CLASP_DIR)/build/release/include \
			-L $(EXTERNALS_CLASP_DIR)/build/release/lib \
			-flto=thin \
			-DNDEBUG \
			-DBUILDING_CLASP 

#			-L $(CLASP_HOME)/build/$(GC)/fasl/ \

export DARWIN_OPTIONS = -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
			$(COMMON_OPTIONS) \
			-D_TARGET_OS_DARWIN \
			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/8.0.0 \
			-O3 \
			-flat_namespace \
			-undefined suppress \
			-bundle \
			-Wl,-flat_namespace,-undefined,dynamic_lookup \
			-Wno-deprecated-register \
			-DCLASP_GC_FILENAME=$(CLASP_GC_FILENAME) \
			-lboost_filesystem -lboost_regex -lboost_date_time -lboost_system \
			-lgmp -lgmpxx \
			-lgc \
			-lreadline

#			-I $(XCODE_INCLUDE) \
#			-I $(CLASP_HOME)/include/clasp/core \
#			-I $(CLASP_HOME)/include/clasp/clbind \
#			-I $(CLASP_HOME)/include \
#			-I $(CLASP_HOME)/include/clasp/main \

#			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
#			-resource-dir $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/../lib/clang/3.6.1 \

#From makefile.faheem


export LINUX_OPTIONS = $(COMMON_OPTIONS) \
			-fPIC \
			-std=c++11 \
			-D_TARGET_OS_LINUX \
			-DUSE_BOEHM \
			-DCLASP_GC_FILENAME=$(CLASP_GC_FILENAME) \
			-iquote $(CLASP_SOURCE)/../include/clasp/main \
			-iquote $(CLASP_SOURCE)/../include/clasp \
			-Wl,--start-group \
			-Wl,--end-group \
			-fuse-ld=gold \
			--shared \
			-lgmp -lgmpxx \
			-lgc \
			-lreadline -ltermcap 

ifeq ($(TARGET_OS),Darwin)
	export OPTIONS = $(DARWIN_OPTIONS) $(LOCAL_OPTIONS)
else
	export OPTIONS = $(LINUX_OPTIONS) $(LOCAL_OPTIONS)
endif

export OPTIONS = -v -I$(CLASP_HOME)/include \
		-I$(CLASP_HOME)/src/main \
		-I$(CLASP_HOME)/build/$(CLASP_RUNTIME) \
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
	clang++ -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bc helloWorld.cc
	llvm-dis helloWorld.bc


clean:
	(cd hello-world; rm -f *.bc *.so *.ll)
	(cd double-vector; rm -f *.bc *.so *.ll)
