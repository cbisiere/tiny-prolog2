"Prolog II v1-specific predefined rules"
"English predicates"

new-subworld(W) -> syscall(sysnewworld,W);

real(T) -> syscall(sysis,T,real);
dot(T) -> syscall(sysis,T,dot);
tuple(T) -> syscall(sysis,T,tuple);

substring(S1,N1,N2,S2) -> syscall(syssubstring,S1,N1,N2,S2);
find-pattern(S1,S2,N) -> syscall(sysfindpattern,S1,S2,N);
string-ident(S,I) -> syscall(sysstringident,S,I);
list-string(L,S) -> syscall(sysliststring,L,S);
list-tuple(L,T) -> syscall(syslisttuple,L,T);
split(T,L) -> syscall(syssplit,T,L);

in-real(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,real,true,false);
in-sentence(T1,T2) -> syscall(sysinputis,S) syscall(sysin,S,T1,T2,sentence,true,false);

rule(T,Q) -> syscall(sysrule,T,Q);

