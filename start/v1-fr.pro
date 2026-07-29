"Prolog II v1-specific predefined rules"
"French predicates"

purger(W) -> syscall(syskillworld,W,false);

boum(I,S) -> syscall(sysstringident,S,I);

renommer(S1,S2) -> syscall(sysrename,S1,S2);

in-ph(T) -> syscall(sysinputis,S) syscall(sysin,S,X,T,sentence,true,false);

"neutralize an undocumented predicate called by the supervisor:"
fi-sortie("") ->;