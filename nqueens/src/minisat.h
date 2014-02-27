
#ifndef MINISAT_H_
#define MINISAT_H_

#include "minisat/core/Solver.h"

using namespace Minisat;

Solver* ms_create();
void ms_free(Solver* s);
Var ms_var(Solver* s);
Lit ms_lit(Var x);
Lit ms_not(Solver* s, Lit x);
Lit ms_and(Solver* s, Lit a, Lit b);
Lit ms_or(Solver* s, Lit a, Lit b);
Lit ms_true(Solver* s);
Lit ms_false(Solver* s);
void ms_assert(Solver* s, Lit x);


// 0 if UNSAT
// 1 if SAT
int ms_check(Solver* s);

// 0 if False
// 1 if True
int ms_getvar(Solver* s, Var v);

#endif//MINISAT_H_
