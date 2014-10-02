# makefile
#
all-osx:
	clang++ -bundle -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bundle helloWorld.cc

all-linux:
	echo Put something extremely clever here to build on linux


ir:
	clang++ -c -emit-llvm -std=c++11 -stdlib=libc++ -fvisibility=hidden -o helloWorld.bc helloWorld.cc
	llvm-dis helloWorld.bc


clean:
	rm -f *.bundle *.so
	rm -f *.ll *.bc
