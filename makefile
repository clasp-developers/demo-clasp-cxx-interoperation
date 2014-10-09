# makefile
#

export EXTERNALS_BUILD_TARGET_DIR = $(HOME)/local/externals-clasp
export CLASP_SOURCE = $(HOME)/Development/clasp/src
export CLANG = $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/clang++
#		-fvisibility=hidden

export DARWIN_OPTIONS = -bundle -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
			-D_TARGET_OS_DARWIN \
			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
			-O0 -g \
			-Wl,-flat_namespace,-undefined,dynamic_lookup \
			-Wno-deprecated-register \
			-I $(CLASP_SOURCE) \
			-I $(CLASP_SOURCE)/include \
			-I $(CLASP_SOURCE)/clbind/bin/boehm/clang-darwin-4.2.1/release/link-static \
			-I $(EXTERNALS_BUILD_TARGET_DIR)/common/include \
			-I $(EXTERNALS_BUILD_TARGET_DIR)/release/include \
			-L $(CLASP_SOURCE)/gctools/bundle \
			-l gctools_boehm_dbg \
			-L $(CLASP_SOURCE)/core/bundle \
			-l core_boehm_dbg \
			-L $(CLASP_SOURCE)/clbind/bundle \
			-l clbind_boehm_dbg \
			-L $(EXTERNALS_BUILD_TARGET_DIR)/common/lib \
			-L $(EXTERNALS_BUILD_TARGET_DIR)/release/lib \
			-lboost_filesystem -lboost_regex -lboost_date_time -lboost_system \
			-lgmp -lgmpxx \
			-lgc \
			-lreadline

#			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
#			-resource-dir $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/../lib/clang/3.6.0 \

#From makefile.faheem
export LINUX_OPTIONS = -v -shared -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
			-D_TARGET_OS_LINUX \
			-x c++ \
			-O0 -g \
			-Wl,-flat_namespace,-undefined,dynamic_lookup \
			-Wno-deprecated-register \
		-I $(CLASP_SOURCE) \
		-I $(CLASP_SOURCE)/include \
		-I $(CLASP_SOURCE)/core \
		-I $(CLASP_SOURCE)/clbind/bin/boehm/clang-darwin-4.2.1/release/link-static \
		-I $(EXTERNALS_BUILD_TARGET_DIR)/common/include \
		-I $(EXTERNALS_BUILD_TARGET_DIR)/release/include \
		-I $(EXTERNALS_BUILD_TARGET_DIR)/release/lib/clang/3.6.0/include \
		-internal-isystem /home/meister/local/gcc-4.8.3/lib/gcc/x86_64-redhat-linux/4.8.3/../../../../include/c++/4.8.3 \
		 -internal-isystem /home/meister/local/gcc-4.8.3/lib/gcc/x86_64-redhat-linux/4.8.3/../../../../include/c++/4.8.3/x86_64-redhat-linux \
		-internal-isystem /home/meister/local/gcc-4.8.3/lib/gcc/x86_64-redhat-linux/4.8.3/../../../../include/c++/4.8.3/backward  \
		-L $(CLASP_SOURCE)/gctools/bundle \
		-l gctools_boehm_dbg \
		-L $(CLASP_SOURCE)/core/bundle \
		-l core_boehm_dbg \
		-L $(CLASP_SOURCE)/clbind/bundle \
		-l clbind_boehm_dbg \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/common/lib \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/release/lib \
		-lgmp -lgmpxx \
		-lgc \
		-lreadline

ifeq ($(TARGET_OS),darwin)
	export OPTIONS = $(DARWIN_OPTIONS)
else
	export OPTIONS = $(LINUX_OPTIONS)
endif


all:
	(cd hello-world; make)
	(cd double-vector; make)


all-linux:
	make TARGET_OS=linux

all-darwin:
	make TARGET_OS=darwin


shell:
	bash


ir:
	clang++ -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bc helloWorld.cc
	llvm-dis helloWorld.bc


clean:
	rm -f *.bundle *.so
	rm -f *.ll *.bc
