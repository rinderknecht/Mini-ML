# A series of programming languages for the compilation to Michelson

In particular, two Mini-ML languages (strict subsets of OCaml) and
their interpreter in OCaml. Some also have a translator to OCaml in
case the Mini-ML evolves into a Domain-Specific Language.

# Build

Obviously, you need the OCaml system.

In order to build those interpreters, you need to clone the following
repositories in the directory containing the present sources:

         $ git clone https://github.com/rinderknecht/UnionFind
         $ git clone https://github.com/rinderknecht/OCaml-build

In each directory (one per language), create the symbolic links for
third-party OCaml libraries, if any:

         $ ./link.sh

Then build the executables to byte-code:

         $ ./build.sh

After running the executables, time to clean up the directory:

         $ ./clean.sh
