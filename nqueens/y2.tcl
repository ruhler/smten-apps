
source bench.tcl

set cfgs [list [cfg "Int" "yices2" "Int"] \
               [cfg "Integer" "yices2" "Integer"] \
               [cfg "Bit" "yices2" "Bit"] \
               [cfg "Bool" "yices2" "Bool"]]
gendata 1 $cfgs

