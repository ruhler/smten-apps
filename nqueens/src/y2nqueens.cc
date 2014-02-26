
#include <stdlib.h>
#include <vector>
#include "yices.h"

void nqueens(int n, bool pretty)
{
    yices_init();
    context_t* ctx = yices_new_context(NULL);

    // Create an n x n array of boolean variables.
    std::vector<std::vector<term_t> > vars;
    for (int r = 0; r < n; r++) {
        vars.push_back(std::vector<term_t>());
        for (int c = 0; c < n; c++) {
            type_t ty = yices_bool_type();
            term_t x = yices_new_uninterpreted_term(ty);
            vars[r].push_back(x);
        }
    }
    term_t f = yices_true();

    // Assert each row must have at least one placed.
    for (int r = 0; r < n; r++) {
        term_t fr = yices_false();
        for (int c = 0; c < n; c++) {
            fr = yices_or2(fr, vars[r][c]);
        }
        f = yices_and2(f, fr);
    }

    // Assert there are no conflicts in each row.
    for (int r = 0; r < n; r++) {
        for (int c1 = 0; c1 < n; c1++) {
            for (int c2 = c1+1; c2 < n; c2++) {
                term_t na = yices_not(vars[r][c1]);
                term_t nb = yices_not(vars[r][c2]);
                f = yices_and2(f, yices_or2(na, nb));
            }
        }
    }

    // Assert there are no conflicts in each col.
    for (int c = 0; c < n; c++) {
        for (int r1 = 0; r1 < n; r1++) {
            for (int r2 = r1+1; r2 < n; r2++) {
                term_t na = yices_not(vars[r1][c]);
                term_t nb = yices_not(vars[r2][c]);
                f = yices_and2(f, yices_or2(na, nb));
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
                term_t na = yices_not(vars[r1][c1]);
                term_t nb = yices_not(vars[r2][c2]);
                f = yices_and2(f, yices_or2(na, nb));
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
                term_t na = yices_not(vars[r1][c1]);
                term_t nb = yices_not(vars[r2][c2]);
                f = yices_and2(f, yices_or2(na, nb));
            }
        }
    }

    yices_assert_formula(ctx, f);
    smt_status_t r = yices_check_context(ctx, NULL);
    if (r == STATUS_UNSAT) {
        printf("no solution");
    } else if (r == STATUS_SAT) {
        model_t* model = yices_get_model(ctx, 1);

        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                int32_t x = -1;
                int ir = yices_get_bool_value(model, vars[r][c], &x); 
                if (ir == 0 && x) {
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
        yices_free_model(model);
    } else {
        printf("YICES2 CHECK FAILED\n");
    }

    yices_free_context(ctx);
    yices_exit();

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

