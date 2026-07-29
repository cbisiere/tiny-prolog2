"Prolog II version 2 (1982) predefined rules"
"English predicates"

-> syscall(sysassign,sysbackupfile,"start/saved.pro")
    syscall(sysinsert,"v1-v2.pro")
    syscall(sysinsert,"v2.pro")
    fail;
