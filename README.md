hello-world
==========

Examples of exposing C++ to Clasp

To build this demo do the following steps:

1. Clone this repository into the clasp/extensions directory
2. Configure the build by running the following in the root of the Clasp repo
   ```sh
   ./koga --extensions=demo-clasp-cxx-interoperation
   ```
3. Build Clasp by running
   ```sh
   ninja -C build
   ```

To run the demos run `./build/boehmprecise/clasp` the execute the following in
the Clasp REPL: `(hw:demo)`