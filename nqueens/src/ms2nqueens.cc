
// Implementation of n-queens using MiniSat
// Produces a formula using the same interface as smten-minisat
// Namely: Not expressed directly in conjunctive normal form.

#include <stdlib.h>
#include <vector>
#include "minisat.h"

void nqueens(int n, bool pretty)
{
    Solver* s = minisat_new();

    // Create an n x n array of boolean variables.
    std::vector<std::vector<int> > vars;
    for (int r = 0; r < n; r++) {
        vars.push_back(std::vector<int>());
        for (int c = 0; c < n; c++) {
            int x = minisat_var(s);
            vars[r].push_back(x);
        }
    }
    int f = minisat_true(s);

    // Assert each row must have at least one placed.
    for (int r = 0; r < n; r++) {
        int fr = minisat_false(s);
        for (int c = 0; c < n; c++) {
            fr = minisat_or(s, fr, vars[r][c]);
        }
        f = minisat_and(s, f, fr);
    }

    // Assert there are no conflicts in each row.
    for (int r = 0; r < n; r++) {
        for (int c1 = 0; c1 < n; c1++) {
            for (int c2 = c1+1; c2 < n; c2++) {
                int na = minisat_not(s, vars[r][c1]);
                int nb = minisat_not(s, vars[r][c2]);
                f = minisat_and(s, f, minisat_or(s, na, nb));
            }
        }
    }

    // Assert there are no conflicts in each col.
    for (int c = 0; c < n; c++) {
        for (int r1 = 0; r1 < n; r1++) {
            for (int r2 = r1+1; r2 < n; r2++) {
                int na = minisat_not(s, vars[r1][c]);
                int nb = minisat_not(s, vars[r2][c]);
                f = minisat_and(s, f, minisat_or(s, na, nb));
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
                int na = minisat_not(s, vars[r1][c1]);
                int nb = minisat_not(s, vars[r2][c2]);
                f = minisat_and(s, f, minisat_or(s, na, nb));
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
                int na = minisat_not(s, vars[r1][c1]);
                int nb = minisat_not(s, vars[r2][c2]);
                f = minisat_and(s, f, minisat_or(s, na, nb));
            }
        }
    }

    minisat_assert(s, f);
    int r = minisat_check(s);
    if (r == 0) {
        printf("no solution");
    } else {
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                int x = minisat_getvar(s, vars[r][c]); 
                if (x) {
                    if (pretty) {
                        printf("X");
                    } else {
                        printf("%i ", c);
                    }
                } else if (pretty) {
                    printf(".");
                }
            }
            if (pretty) {
                printf("\n");
            }
        }
    }

    minisat_delete(s);

    printf("\n");
    fflush(stdout);

}


int main(int argc, char* argv[])
{
    if (argc < 2) {
        printf("usage: msnqueens <n>\n");
        return 1;
    }

    int n = atoi(argv[1]);
    if (n < 0) {
        for (int i = 0; i <= -n; i++) {
            nqueens(i, false);
        }
    } else {
        nqueens(n, true);
    }
    return 0;
}

