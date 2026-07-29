"Prolog II v2-specific predefined rules"
"French predicates"

creer-monde(W) -> syscall(sysnewworld,W);

reel(T) -> syscall(sysis,T,real);
liste(T) -> syscall(sysis,T,dot);
nuplet(T) -> syscall(sysis,T,tuple);

dans-chaine(S1,N1,N2,S2) -> syscall(syssubstring,S1,N1,N2,S2);
sous-chaine(S1,S2,N) -> syscall(sysfindpattern,S1,S2,N);
chaine-ident(S,I) -> syscall(sysstringident,S,I);
liste-chaine(L,S) -> syscall(sysliststring,L,S);
liste-nuplet(L,T) -> syscall(syslisttuple,L,T);
decompose(T,L) -> syscall(syssplit,T,L);

in-reel(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,real,true,false);
in-ph(T1,T2) -> syscall(sysinputis,S) syscall(sysin,S,T1,T2,sentence,true,false);

regle(T,Q) -> syscall(sysrule,T,Q);
