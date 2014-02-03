
source bench.tcl

set cfgs [list [cfg "Int" "stp" "Int"] \
               [cfg "Bit" "stp" "Bit"] \
               [cfg "Bool" "stp" "Bool"]]
gendata 60 $cfgs

