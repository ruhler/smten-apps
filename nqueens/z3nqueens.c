
#include <stdio.h>
#include <stdlib.h>

#include <z3.h>

// Compute and print out an n-queens placement for given n.
// See 4queens.smt for a model of the kind of query we want to generate.
void nqueens(int n)
{
    // Initialize Z3 and create a solver.
    Z3_config cfg = Z3_mk_config();
    Z3_context ctx = Z3_mk_context(cfg);
    Z3_solver slv = Z3_mk_solver(ctx);
    Z3_solver_inc_ref(ctx, slv);
    Z3_del_config(cfg);

    // Some common things it will be nice to have.
    Z3_sort intsort = Z3_mk_int_sort(ctx);
    Z3_ast zero = Z3_mk_int(ctx, 0, intsort);
    Z3_ast nval = Z3_mk_int(ctx, n, intsort);

    // Declare free Ints q1, ... qn 
    Z3_ast* q = malloc(n * sizeof(Z3_func_decl));
    for (int i = 0; i < n; i++) {
        // (declare-fun qi () Int)
        Z3_func_decl qdecl = Z3_mk_fresh_func_decl(ctx, NULL, 0, NULL, intsort);
        q[i] = Z3_mk_app(ctx, qdecl, 0, NULL);

        // (assert (and (>= qi 0) (< qi n)))
        Z3_ast gt0 = Z3_mk_ge(ctx, q[i], zero);
        Z3_ast ltn = Z3_mk_lt(ctx, q[i], nval);
        Z3_ast args[2] = {gt0, ltn};
        Z3_ast bounded = Z3_mk_and(ctx, 2, args);
        Z3_solver_assert(ctx, slv, bounded);
    }

    // Rows are disjoint by construction.

    // Assert columns are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            // (assert (not (= qi qj)))
            Z3_ast eq = Z3_mk_eq(ctx, q[i], q[j]);
            Z3_ast neq = Z3_mk_not(ctx, eq);
            Z3_solver_assert(ctx, slv, neq);
        }
    }

    // Define terms for positive diagonals.
    Z3_ast* pdiags = malloc(n * sizeof(Z3_ast));
    for (int i = 0; i < n; i++) {
        Z3_ast ival = Z3_mk_int(ctx, i, intsort);
        Z3_ast args[2];
        args[0] = ival;
        args[1] = q[i];
        pdiags[i] = Z3_mk_add(ctx, 2, args);
    }

    // Assert positive diagonals are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            // (assert (not (= pi pj)))
            Z3_ast eq = Z3_mk_eq(ctx, pdiags[i], pdiags[j]);
            Z3_ast neq = Z3_mk_not(ctx, eq);
            Z3_solver_assert(ctx, slv, neq);
        }
    }

    // Define terms for negative diagonals.
    Z3_ast* ndiags = malloc(n * sizeof(Z3_ast));
    for (int i = 0; i < n; i++) {
        Z3_ast ival = Z3_mk_int(ctx, i, intsort);
        Z3_ast args[2];
        args[0] = ival;
        args[1] = q[i];
        ndiags[i] = Z3_mk_sub(ctx, 2, args);
    }

    // Assert negative diagonals are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            // (assert (not (= ni nj)))
            Z3_ast eq = Z3_mk_eq(ctx, ndiags[i], ndiags[j]);
            Z3_ast neq = Z3_mk_not(ctx, eq);
            Z3_solver_assert(ctx, slv, neq);
        }
    }

    // (check-sat)
    Z3_lbool res = Z3_solver_check(ctx, slv);
    switch (res) {
        case -1: printf("no solution\n"); break;
        case 0: printf("error in z3 check\n"); break;
        case 1:
        {
            Z3_model model = Z3_solver_get_model(ctx, slv);
            int* cols = malloc(n * sizeof(int));
            for (int i = 0; i < n; i++) {
                Z3_ast qval;
                Z3_model_eval(ctx, model, q[i], Z3_TRUE, &qval);
                Z3_get_numeral_int(ctx, qval, cols + i);
            }

            // Print out the board.
            for (int r = 0; r < n; r++) {
                for (int c = 0; c < n; c++) {
                    printf("%c", c == cols[r] ? 'X' : '.');
                }
                printf("\n");
            }
        }
    }

    // Clean up
    free(ndiags);
    free(pdiags);
    free(q);
    Z3_solver_dec_ref(ctx, slv);
    Z3_del_context(ctx);

}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        printf("usage: z3nqueens <n>\n");
        return 1;
    }

    int n = atoi(argv[1]);
    nqueens(n);
    return 0;
}

