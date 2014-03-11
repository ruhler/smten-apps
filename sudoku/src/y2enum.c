
#include "yices.h"

int digitOf(const char* board, int r, int c)
{
    return board[9*r+c] - '1';
}

void sudoku(const char* board)
{
    yices_init();
    context_t* ctx = yices_new_context(NULL);
    type_t ty = yices_new_scalar_type(9);
    term_t digits[10];
    term_t terms[9][9];

    for (int i = 0; i < 9; i++) {
        digits[i] = yices_constant(ty, i);
    }

    // Create a variable for each blank cell.
    for (int r = 0; r < 9; r++) {
        for (int c = 0; c < 9; c++) {
            int d = digitOf(board, r, c);
            if (d >= 0 && d < 9) {
                terms[r][c] = digits[d];
            } else {
                terms[r][c] = yices_new_uninterpreted_term(ty);
            }
        }
    }

    // Assert rows are distinct.
    for (int r = 0; r < 9; r++) {
        term_t arg[9];
        for (int c = 0; c < 9; c++) {
            arg[c] = terms[r][c];
        }
        yices_assert_formula(ctx, yices_distinct(9, arg));
    }

    // Assert columns are distinct.
    for (int c = 0; c < 9; c++) {
        term_t arg[9];
        for (int r = 0; r < 9; r++) {
            arg[r] = terms[r][c];
        }
        yices_assert_formula(ctx, yices_distinct(9, arg));
    }

    // Assert boxes are distinct.
    for (int b = 0; b < 9; b++) {
        term_t arg[9];
        term_t* p = arg;
        for (int r = 0; r < 3; r++) {
            for (int c = 0; c < 3; c++) {
                *p = terms[3*(b/3)+r][3*(b%3)+c];
                p++;
            }
        }
        yices_assert_formula(ctx, yices_distinct(9, arg));
    }

    smt_status_t r = yices_check_context(ctx, NULL);
    if (r == STATUS_UNSAT) {
        printf("no solution");
    } else if (r == STATUS_SAT) {
        model_t* model = yices_get_model(ctx, 1);

        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                int d = digitOf(board, r, c);
                if (d >= 0 && d < 9) {
                    printf("%c", d+'1');
                } else {
                    int32_t v;
                    yices_get_scalar_value(model, terms[r][c], &v);
                    printf("%c", v+'1');
                }
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
    char board[82];
    while (!feof(stdin)) {
        fgets(board, 82, stdin);
        sudoku(board);
    }
    return 0;
}

