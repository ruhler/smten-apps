
# All tests from tests/sat/*.aig should pass
foreach x [glob tests/sat/*.aig] {
    puts "$x ..."
    exec ./utils/runsat $x
}

# All tests from tests/unsat/*.aig should be unsat
foreach x [glob tests/unsat/*.aig] {
    puts "$x ..."
    exec ./utils/rununsat $x
}

