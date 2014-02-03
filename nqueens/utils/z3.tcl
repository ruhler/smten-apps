
source bench.tcl

set cfgs [list [cfg "Int" "z3" "Int"] \
               [cfg "Integer" "z3" "Integer"] \
               [cfg "Bit" "z3" "Bit"] \
               [cfg "Bool" "z3" "Bool"]]
gendata 60 $cfgs

