
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Text-based z3 solution to nqueens.

// Generate the query in smtlib2 format.
void genquery(FILE* fout, int n)
{
    fprintf(fout, "(set-option :produce-models true)\n");
    fprintf(fout, "(set-logic QF_LIA)\n\n");

    // Declare Int specifying the column of the queen for each row.
    for (int i = 0; i < n; i++) {
        fprintf(fout, "(declare-fun q%i () Int)\n", i);
    }

    // Bound the columns properly.
    for (int i = 0; i < n; i++) {
        fprintf(fout, "(assert (and (>= q%i 0) (< q%i %i)))\n", i, i, n);
    }

    // Assert columns are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            fprintf(fout, "(assert (not (= q%i q%i)))\n", i, j);
        }
    }

    // Define terms for positive diagonals.
    for (int i = 0; i < n; i++) {
        fprintf(fout, "(define-fun p%i () Int (+ %i q%i))\n", i, i, i);
    }

    // Assert positive diagonals are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            fprintf(fout, "(assert (not (= p%i p%i)))\n", i, j);
        }
    }

    // Define terms for negative diagonals.
    for (int i = 0; i < n; i++) {
        fprintf(fout, "(define-fun n%i () Int (- %i q%i))\n", i, i, i);
    }

    // Assert negative diagonals are disjoint
    for (int i = 0; i < n; i++) {
        for (int j = i+1; j < n; j++) {
            fprintf(fout, "(assert (not (= n%i n%i)))\n", i, j);
        }
    }

    fprintf(fout, "(check-sat)\n");
    for (int i = 0; i < n; i++) {
        fprintf(fout, "(get-value (q%i))\n", i);
    }
    fclose(fout);
}

// Read the result from the SAT solver in, print out the board based on that.
void getresult(FILE* fin, int n)
{
    // First line is 'sat' or 'unsat'
    char* line = NULL;
    size_t linesz = 0;
    getline(&line, &linesz, fin);
    if (strncmp("sat", line, 3) != 0) {
        printf("no solution");
        free(line);
        return;
    }
    free(line);

    int* cols = malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        fscanf(fin, " ((q%*i %i))", cols + i);
    }

    // Print out the board.
    for (int r = 0; r < n; r++) {
        for (int c = 0; c < n; c++) {
            printf("%c", c == cols[r] ? 'X' : '.');
        }
        printf("\n");
    }
    free(cols);
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        printf("usage: z3txtnqueens <n>\n");
        return 1;
    }

    int n = atoi(argv[1]);
    FILE* fout = fopen("z3txtnqueens.smt", "w");
    genquery(fout, n);
    system("z3 -smt2 z3txtnqueens.smt > z3txtnqueens.result");
    FILE* fin = fopen("z3txtnqueens.result", "r");
    getresult(fin, n);
    return 0;
}

