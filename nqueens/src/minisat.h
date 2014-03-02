
#ifndef MINISAT_H_
#define MINISAT_H_

#include "minisat/core/Solver.h"

using namespace Minisat;

extern "C" Solver* minisat_new();
extern "C" void minisat_delete(Solver* s);
extern "C" int minisat_true(Solver* s);
extern "C" int minisat_false(Solver* s);
extern "C" int minisat_var(Solver* s);
extern "C" int minisat_not(Solver* s, int x);
extern "C" int minisat_and(Solver* s, int a, int b);
extern "C" int minisat_or(Solver* s, int a, int b);
extern "C" void minisat_assert(Solver* s, int x);


// 0 if UNSAT
// 1 if SAT
extern "C" int minisat_check(Solver* s);

// 0 if False
// 1 if True
extern "C" int minisat_getvar(Solver* s, int v);

#endif//MINISAT_H_

