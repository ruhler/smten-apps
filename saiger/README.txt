Saiger
======
Richard Uhler <ruhler@csail.mit.edu>
June 2014

Introduction
------------
Saiger is a smten-based implementation of an AIGER (pre aiger-1.9) model
checker. See http://fmv.jku.at/aiger for more information on the AIGER format.

Prerequisites
-------------
Saiger requires smten is installed. Smten can be retrieved from:

  http://github.com/ruhler/smten

Saiger requires aiger-1.9 is installed. In particular, aigsim and aig2aig from
aiger-1.9.4 should be in $PATH. (Note: saiger uses aiger-1.9, even though it
is an implementation of AIGER format pre aiger-1.9).
Aiger is available at http://fmv.jku.at/aiger.

For running tests and benchmarks, saiger also requires the hwmcc10 benchmarks
are available extracted to the directory indicated by the $HWMCC10 environment
variable. These benchmarks are available at:

    http://fmv.jku.at/hwmcc/hwmcc10-except-intel-aiger1.7z

For performance evaluation, saiger requires PdTrav be installed. PdTrav is
available at:
   http://fmgroup.polito.it/index.php/download/finish/3-pdtrav-package/5-pdtools-1-3-0/0


Building Saiger
---------------
To build saiger, run:

 $ make

This will build both release and profiling builds of saiger. The release
version can be found at build/release/saiger, and the profiling version can be
found at build/profile/saiger.

Running make will execute the unit tests cases.

Checking Saiger
---------------
To check saiger on some of the hwmcc10 benchmarks, run:

 $ make check

Running Saiger
--------------
To run saiger, use aigtoaig to output ascii format for the given problem.
For example:

  $ aigtoaig -a $HWMCC10/139442p0neg.aig | ./build/release/saiger

The -s option can be used to select a different solver. Currently supported
solvers are yices1, yices2, stp, minisat, and z3. For example:

  $ aigtoaig -a $HWMCC10/139442p0neg.aig | ./build/release/saiger -s yices2

  $ aigtoaig -a $HWMCC10/139442p0neg.aig | ./build/release/saiger -s z3


The -k0 and -ki options can be used to adjust the initial bound and increment.
For example, to start with traces of length 4, and increment the trace length
by 2 each iteration, run:

  $ aigtoaig -a $HWMCC10/139442p0neg.aig | ./build/release/saiger -k0 4 -ki 2


Evaluating Performance 
----------------------
The script 'utils/perf.tcl' runs saiger and pdtrav on each of the
benchmarks given on the command line. The files utils/sat.txt and
utils/unsat.txt list the sat and unsat benchmarks from hwmcc10.
The utils/perf.tcl script uses utils/run to invoke saiger and utils/pdtrav to
invoke pdtrav.

The makefile target 'perf' runs all sat and unsat benchmarks from the hwmcc10
benchmark set and outputs the results to build/sat.data and build/unsat.data.

The makefile target 'perf_fast' runs a subset of benchmarks from the hwmcc100
set and outputs the results to build/sat.data and build/unsat.data.

After either 'perf' or 'perf_fast' has been run, the makefile target 'graph'
will produce graph build/perf.pdf comparing saiger against pdtrav.

For example, to run fast performance numbers (takes about a minute):
  $ make perf_fast
  $ make graph

To run full performance numbers:
  $ make perf
  $ make graph

