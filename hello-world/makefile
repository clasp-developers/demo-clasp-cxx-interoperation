# makefile
#

export EXTERNALS_BUILD_TARGET_DIR = $(HOME)/local/externals-clasp
export CLASP_SOURCE = $(HOME)/Development/clasp/src
export CLANG = $(EXTERNALS_BUILD_TARGET_DIR)/release/bin/clang++
#		-fvisibility=hidden

osx-dbg:
	clang++ -bundle -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
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
		-lreadline \
		-o helloWorld.bundle helloWorld.cc


#From makefile.faheem
linux-dbg:
	$(CLANG) -shared -DUSE_BOEHM -std=c++11 -stdlib=libc++ \
	 -resource-dir /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/6.0 \
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
		-lreadline \
		-o helloWorld.bundle helloWorld.cc



shell:
	bash


ir:
	clang++ -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bc helloWorld.cc
	llvm-dis helloWorld.bc


clean:
	rm -f *.bundle *.so
	rm -f *.ll *.bc
