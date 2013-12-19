
source bench.tcl

set cfgs [list [cfg "Int" "yices1" "Int"] \
               [cfg "Integer" "yices1" "Integer"] \
               [cfg "Bit" "yices1" "Bit"] \
               [cfg "Bool" "yices1" "Bool"]]
gendata 60 $cfgs

