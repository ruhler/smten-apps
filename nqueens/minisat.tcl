
source bench.tcl

set cfgs [list [cfg "Int" "minisat" "Int"] \
               [cfg "Bit" "minisat" "Bit"] \
               [cfg "Bool" "minisat" "Bool"]]
gendata 60 $cfgs

