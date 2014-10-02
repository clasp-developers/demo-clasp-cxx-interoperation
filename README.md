hello-world
==========

Example of exposing C++ to Clasp

Works on OS X now - linux later.

To build:  make all-osx

To run:
fry:hello-world$ clasp_boehm_o
../../src/clbind/scope.cc:146 Declaring package: LLVM
../../src/clbind/scope.cc:146 Declaring package: CLANG-AST
../../src/clbind/scope.cc:146 Declaring package: AST-TOOLING
../../src/clbind/scope.cc:146 Declaring package: CLANG-COMMENTS
Starting Clasp 0.1... loading image... it takes a few seconds
!
! WARNING:   The file #P"image.bundle" is out of date 
!            relative to app-resources:lib;release;intrinsics_bitcode_boehm.o
!
!  Solution: Recompile the Common Lisp code
!
Loading .clasprc
Top level.
> (core:load-bundle "helloWorld.bundle")
!
!
!
!   Starting CLASP_MAIN
!
!
Expose C++ code to Clasp here
../../src/clbind/scope.cc:146 Declaring package: HELLO

#<CORE:POINTER :ptr 0x7ff43c79a360>
NIL
> (hello:hello-world)
Hello World

> (hello:add-three-numbers 1.5 2.3 9.8)

13.6
> 
