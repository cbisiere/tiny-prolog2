"Prolog II v1-specific predefined rules"
"English predicates"

purge(W) -> syscall(syskillworld,W,false);

boom(I,S) -> syscall(sysstringident,S,I);

rename(S1,S2) -> syscall(sysrename,S1,S2);

in-sentence(T) -> syscall(sysinputis,S) syscall(sysin,S,X,T,sentence,true,false);

"neutralize an undocumented predicate called by the supervisor:"
fi-output("") ->;