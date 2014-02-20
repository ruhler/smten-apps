
#include <vector>
#include "minisat/core/Solver.h"

using namespace Minisat;

void nqueens(int n)
{
    Solver s;

    // Create an n x n array of boolean variables.
    std::vector<std::vector<Var> > vars;
    for (int r = 0; r < n; r++) {
        vars.push_back(std::vector<Var>());
        for (int c = 0; c < n; c++) {
            vars[r].push_back(s.newVar());
        }
    }

    // Assert each row must have at least one placed.
    for (int r = 0; r < n; r++) {
        vec<Lit> ps;
        for (int c = 0; c < n; c++) {
            ps.push(mkLit(vars[r][c], true));
        }
        s.addClause(ps);
    }

    // Assert there cannot be conflicts
    for (int r1 = 0; r1 < n; r1++) {
        for (int c1 = 0; c1 < n; c1++) {
            for (int c2 = c1+1; c2 < n; c2++) {
                s.addClause(mkLit(vars[r1][c1], false), mkLit(vars[r1][c2], false));
            }
                
            for (int r2 = r1 + 1; r2 < n; r2++) {
                for (int c2 = 0; c2 < n; c2++) {
                    bool eqcol = c1 == c2;
                    bool eqpdiag = (r1 + c1) == (r2 + c2);
                    bool eqndiag = (r1 - c1) == (r2 - c2);
                    if (eqcol || eqpdiag || eqndiag) {
                        s.addClause(mkLit(vars[r1][c1], false), mkLit(vars[r2][c2], false));
                    }
                }
            }
        }
    }

    if (!s.simplify() || !s.solve()) {
        printf("no solution\n");
        return;
    }

    for (int r = 0; r < n; r++) {
        for (int c = 0; c < n; c++) {
            if (toInt(s.model[vars[r][c]])) {
                printf("X");
            } else {
                printf(".");
            }
        }
        printf("\n");
    }
}


int main(int argc, char* argv[])
{
    if (argc < 2) {
        printf("usage: msnqueens <n>\n");
        return 1;
    }

    int n = atoi(argv[1]);
    nqueens(n);
    return 0;
}

