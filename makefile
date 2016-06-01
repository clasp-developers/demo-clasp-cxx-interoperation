# makefile
#
include local.config

ifeq ($(CLANG),)
export CLANG = $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/clang++
endif

ifeq ($(CLASP_BOEHM),)
export CLASP_BOEHM = clasp_boehm_o
endif

#		-fvisibility=hidden
#
# -DNDEBUG suppresses link_compatibility checking in clbind that always fails - must fix

export DARWIN_OPTIONS = -bundle -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
			-D_TARGET_OS_DARWIN \
			-DNDEBUG \
			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
			-O3 \
			-Wl,-flat_namespace,-undefined,dynamic_lookup \
			-Wno-deprecated-register \
			-I $(CLASP_SOURCE) \
			-I $(CLASP_SOURCE)/include \
			-I $(CLASP_SOURCE)/clbind/bin/boehm/clang-darwin-4.2.1/release/link-static \
			-I $(EXTERNALS_BUILD_TARGET_DIR)/common/include \
			-I $(EXTERNALS_BUILD_TARGET_DIR)/release/include \
			-L $(CLASP_SOURCE)/gctools/bundle \
			-l gctools_boehm_opt \
			-L $(CLASP_SOURCE)/core/bundle \
			-l core_boehm_opt \
			-L $(CLASP_SOURCE)/clbind/bundle \
			-l clbind_boehm_opt \
			-L $(EXTERNALS_BUILD_TARGET_DIR)/common/lib \
			-L $(EXTERNALS_BUILD_TARGET_DIR)/release/lib \
			-lboost_filesystem -lboost_regex -lboost_date_time -lboost_system \
			-lgmp -lgmpxx \
			-lgc \
			-lreadline

#			-resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
#			-resource-dir $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/../lib/clang/3.6.1 \

#From makefile.faheem

export LINUX_OPTIONS = -v \
			-fPIC \
			--shared \
			-std=c++11 \
			-D_TARGET_OS_LINUX \
			-DNDEBUG \
			-DBUILDING_CLASP \
			-DUSE_BOEHM \
			-iquote $(CLASP_SOURCE)/../include/clasp/main \
			-iquote $(CLASP_SOURCE)/../include/clasp \
			-I $(CLASP_SOURCE)/../include \
			-I $(CLASP_SOURCE)/include \
			-I $(CLASP_SOURCE)/core/bin/boehm/clang-linux-3.6.1/release/link-static \
			-I $(CLASP_SOURCE)/clbind/bin/boehm/clang-linux-3.6.1/release/link-static \
			-I $(CLASP_SOURCE) \
			-Wl,--start-group \
			$(CLASP_SOURCE)/gctools/bundle/libgctools_boehm_opt.a \
			$(CLASP_SOURCE)/core/bundle/libcore_boehm_opt.a \
			$(CLASP_SOURCE)/clbind/bundle/libclbind_boehm_opt.a \
			-Wl,--end-group \
#		-L $(CLASP_SOURCE)/gctools/bundle \
#		-l gctools_boehm_opt \
#		-L $(CLASP_SOURCE)/core/bundle \
#		-l core_boehm_opt \
#		-L $(CLASP_SOURCE)/clbind/bundle \
#		-l clbind_boehm_opt \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/common/lib \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/release/lib \
		-lgmp -lgmpxx \
		-lgc \
		-lreadline -ltermcap \
		-Wl,--end-group


# for reference only
export LINUX_OPTIONS_OLD_BROKEN = -v -shared -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
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
		-I $(EXTERNALS_BUILD_TARGET_DIR)/release/lib/clang/3.6.1/include \
		-L $(CLASP_SOURCE)/gctools/bundle \
		-l gctools_boehm_opt \
		-L $(CLASP_SOURCE)/core/bundle \
		-l core_boehm_opt \
		-L $(CLASP_SOURCE)/clbind/bundle \
		-l clbind_boehm_opt \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/common/lib \
		-L $(EXTERNALS_BUILD_TARGET_DIR)/release/lib \
		-lgmp -lgmpxx \
		-lgc \
		-lreadline

ifeq ($(TARGET_OS),darwin)
	export OPTIONS = $(DARWIN_OPTIONS) $(LOCAL_OPTIONS)
else
	export OPTIONS = $(LINUX_OPTIONS) $(LOCAL_OPTIONS)
endif

all:
	(cd hello-world; make $*)
	(cd double-vector; make $*)

test:
	(cd hello-world; make test)
	(cd double-vector; make test)


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
