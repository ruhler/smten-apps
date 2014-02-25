
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

    // Assert there are no conflicts in each row.
    for (int r = 0; r < n; r++) {
        for (int c1 = 0; c1 < n; c1++) {
            for (int c2 = c1+1; c2 < n; c2++) {
                s.addClause(mkLit(vars[r][c1], false), mkLit(vars[r][c2], false));
            }
        }
    }

    // Assert there are no conflicts in each col.
    for (int c = 0; c < n; c++) {
        for (int r1 = 0; r1 < n; r1++) {
            for (int r2 = r1+1; r2 < n; r2++) {
                s.addClause(mkLit(vars[r1][c], false), mkLit(vars[r2][c], false));
            }
        }
    }

    // Assert there are no conflicts in each positive diagonal
    //  r = c + d
    for (int d = 2-n; d < n-1; d++) {
        for (int c1 = std::max(0, -d); c1 < std::min(n, n-d); c1++) {
            int r1 = c1 + d;
            for (int c2 = c1+1; c2 < std::min(n, n-d); c2++) {
                int r2 = c2 + d;
                s.addClause(mkLit(vars[r1][c1], false), mkLit(vars[r2][c2], false));
            }
        }
    }

    // Assert there are no conflicts in each negative diagonal
    //  r = -c + d
    for (int d = 1; d < 2*(n-1); d++) {
        for (int c1 = std::max(0, 1+d-n); c1 < std::min(n, 1+d); c1++) {
            int r1 = -c1 + d;
            for (int c2 = c1+1; c2 < std::min(n, 1+d); c2++) {
                int r2 = -c2 + d;
                s.addClause(mkLit(vars[r1][c1], false), mkLit(vars[r2][c2], false));
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

