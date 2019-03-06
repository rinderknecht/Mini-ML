# A Mini-ML programming language

Mini-ML is toy subset of OCaml, except for the fact that the `match`
construct is closed by the keyword `end`. An interpreter and a
transpiler to OCaml are provided. There is no type-checker, so, if you
need type inference, you need to run the transpiler and then run an
OCaml compiler on the generated code.

The directory `Lang1` contains the most complete version of Mini-ML.

# Build

Obviously, you need the OCaml system, but also the libraries `getopt`
and `zarith`. We recommend to use `opam`:

         $ opam install getopt zarith

You also need a library available at this github account: clone the
following repository in the directory containing the present sources:

         $ git clone https://github.com/rinderknecht/UnionFind

In the directory `Lang1`, create the symbolic links for third-party
OCaml libraries, if any, by running

         $ ./link.sh

Then build the executables to byte-code:

         $ ./build.sh

After running the executables, time to clean up the directory:

         $ ./clean.sh
