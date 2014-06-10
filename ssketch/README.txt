SSketch
=======
Richard Uhler <ruhler@csail.mit.edu>
June 2014

Introduction
------------
SSketch is a smten-based re-implementation of the Sketch program synthesis
tool available at http://people.csail.mit.edu/asolar.

Prerequisites
-------------
SSketch requires the happy parser generator and smten.

To install 'happy', use cabal:

  $ cabal install happy


Smten can be retrieved from:

  http://github.com/ruhler/smten

Note: you may need to add $HOME/.cabal/bin to your PATH environment variable
in order for the build scripts to find the installed versions of happy and
smten.

For evaluating performance, the original sketch tools should be installed and
available in the PATH.

Building SSketch
----------------
To build ssketch, run:
 
 $ make

This will build both release and profiling builds of ssketch. The release
version can be found at build/release/ssketch, and the profiling version can
be found at build/profile/ssketch.

Running make will execute the unit test cases.

Running SSketch
---------------
To run ssketch, give it one or more sketch files on the command line.
For example:

  $ ./build/release/ssketch benchmarks/unit/not.smten.sk
  > Running ssketch on "benchmarks/unit/not.smten.sk"...
  > bit[4] sketch1(bit[4] in) implements spec {
  >    return in + {0,1,1,0};
  > }
  >
  >bit[4] spec(bit[4] in) {
  >    bit[4] x = {1,0,0,1};
  >    return in + !x;
  > }


The -s option can be used to select a different solver. Currently supported
solvers are yices1, yices2, stp, minisat, and z3. For example:

  $ ./build/release/ssketch -s yices2 benchmarks/unit/not.smten.sk
  > ...

  $ ./build/release/ssketch -s z3 benchmarks/unit/not.smten.sk
  > ...

The -d option can be used to output the generated SMT queries. For example:
  $ ./build/release/ssketch -d not.dbg benchmarks/unit/not.smten.sk
  > ...

This outputs the queries to the file not.dbg.
  

Test Cases and Benchmarks
-------------------------
The 'benchmarks/' directory contains test cases and benchmarks for ssketch.

benchmarks/unit:: unit tests

benchmarks/tests:: mini tests distributed with Sketch. A list of mini tests
with features supported by ssketch is available in utils/supported.txt

benchmarks/perf:: Minimized performance tests for ssketch.

benchmarks/gallery:: Gallery benchmarks distributed with Sketch. Some of these
have been modified slightly to work with the features supported by ssketch.

Evaluating Performance 
----------------------
The script 'utils/mini.tcl' runs ssketch and the original Sketch on each of
the supported mini tests. The script always uses the yices2 solver for
ssketch.

The makefile target 'mini' runs sketch and ssketch 1 time on each mini test,
and outputs the results to build/mini.data.

The makefile target 'mini_fast' does the same thing, but only runs on the
first hundred mini tests.

After either 'mini' or 'mini_fast' has been run, the makefile target 'graph'
will produce a graph build/mini.pdf comparing ssketch against the original
Sketch tool.

For example, to run fast performance numbers (takes about a minute):
  $ make mini_fast
  $ make graph

To run full performance numbers:
  $ make mini
  $ make graph

