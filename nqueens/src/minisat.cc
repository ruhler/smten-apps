
#include "minisat/core/Solver.h"

using namespace Minisat;

Solver* ms_create()
{
    return new Solver();
}

void ms_free(Solver* s)
{
    delete s;
}

Var ms_var(Solver* s)
{
    return s->newVar();
}

Lit ms_lit(Var x)
{
    return mkLit(x, true);
}

Lit ms_not(Solver* s, Lit x)
{
    return ~x;
}

Lit ms_and(Solver* s, Lit a, Lit b)
{
    Lit x = mkLit(s->newVar(), true);
    s->addClause(~x, a);   // x -> a
    s->addClause(~x, b);   // x -> b
    s->addClause(~a, ~b, x);    // a & b ==> x
    return x;
}

Lit ms_or(Solver* s, Lit a, Lit b)
{
    Lit x = mkLit(s->newVar(), true);
    s->addClause(~a, x);   // a -> x
    s->addClause(~b, x);   // b -> x
    s->addClause(~x, a, b);    // x ==> a | b
    return x;
}

void ms_assert(Solver* s, Lit x)
{
    s->addClause(x);
}


// 0 if UNSAT
// 1 if SAT
int ms_check(Solver* s)
{
    if (!s->simplify()) {
        return 0;
    }
    bool r = s->solve();
    return r ? 1 : 0;
}

// 0 if False
// 1 if True
int ms_getvar(Solver* s, Var v)
{
    return (toInt(s->model[v]));
}

Lit ms_true(Solver* s)
{
    Var x = s->newVar();
    s->addClause(mkLit(x, true));
    return mkLit(x, true);
}

Lit ms_false(Solver* s)
{
    Var x = s->newVar();
    s->addClause(mkLit(x, true));
    return mkLit(x, false);
}

