"Prolog II predefined rules common to version 1 and version 2"
"French predicates"

"worlds"

monde(W) -> syscall(sysworld,W);
sous-mondes(W) -> syscall(syssubworlds,W);
tuer-monde(W) -> syscall(syskillworld,W,true);

"statements"

lister(N) -> syscall(syslist,N,false);
lister -> lister(0);
haut -> syscall(systopstatement);
bas -> syscall(sysbottomstatement);
supprimer(N) -> syscall(syssuppress,N);

"worlds and statements"

monter(N) -> entier(N) syscall(sysupstatement,N);
monter(W) -> syscall(sysclimbworld,W);
monter -> syscall(sysparentworld,W) syscall(sysclimbworld,W);

descendre(W) -> chaine(W) syscall(syssyntax,"PIIv1") syscall(sysdownworld,W,true);
descendre(W) -> chaine(W) syscall(syssyntax,"PII") syscall(sysdownworld,W,false);
descendre(N) -> entier(N) syscall(sysdownstatement,N);
descendre -> descendre(1);

"rules"

tete(I) -> syscall(sysfindrule,I);
inserer(F) -> syscall(sysinsert,F);
inserer -> syscall(sysinsert,"");
ajout(<T,Q>) -> syscall(sysassert2,T,Q,true);

"session"

bonsoir -> 
    exm("sauvegarde... ") 
    syscall(syseval,sysbackupfile,F)
    syscall(syssavestate,F) adieu;
adieu -> exml("bye!") syscall(sysquit);

"is"

ident(T) -> syscall(sysis,T,ident);
entier(T) -> syscall(sysis,T,integer);
chaine(T) -> syscall(sysis,T,string);

libre(T) -> syscall(sysfree,T,true);
pris(T) -> syscall(sysfree,T,false);

"char, string, list, tuple"

no-car(C,N) -> syscall(syscharcode,C,N);
arg(N,T1,T2) -> syscall(sysarg,N,T1,T2);

"array"

def-tab(I,N) -> syscall(sysdefarray,I,N);

"i/o"

entree(F) -> syscall(sysfree,F,true) / syscall(sysinputis,F) /;
entree(F) -> syscall(sysselectinput,F) /;
entree(F) -> syscall(sysopennew,F,read,S,nil);
fermer-entree(F) -> syscall(syscloseinput,F);
fermer-entree -> syscall(sysinputis,F) syscall(syscloseinput,F);

sortie(F) -> syscall(sysfree,F,true) / syscall(sysoutputis,F) /;
sortie(F) -> syscall(sysselectoutput,F) /;
sortie(F) -> syscall(sysopennew,F,write,S,nil);
fermer-sortie -> syscall(sysoutputis,F) syscall(syscloseoutput,F);

tampon-neuf(T) -> syscall(sysnewbuffer) T syscall(sysdelbuffer);

"in: char"

in-car(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,false,false);
in-car'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,true,false);
car-apres(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,false,true);
car-apres'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,true,true);
fin-ligne(T) -> syscall(syssubeol,T);

"in: others"

in(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,term,true,false);
in-entier(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,integer,true,false);
in-ident(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,ident,true,false);
in-chaine(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,string,true,false);

"out"

ex(T) -> syscall(sysout,T);
exl(T) -> syscall(sysout,T) ligne;
exm(S) -> syscall(sysoutm,S);
exml(S) -> syscall(sysoutm,S) ligne;
ligne -> syscall(sysline);
page -> syscall(sysclrsrc);
en-xy(N1,N2) -> syscall(sysgotoxy,N1,N2);
lg-ligne(N) -> syscall(syssetlinewidth,N);
pos(N) -> syscall(syssetlinecursor,N);

"assign and eval"

affecter(I,T) -> syscall(sysassign,I,T);
val(T1,T2) -> syscall(syseval,T1,T2);

"control"

geler(V,G) -> syscall(sysfreeze,V,G);
bloc(T,G) -> syscall(sysblock,T,G);
fin-bloc(T) -> syscall(sysblockexit,T);
impasse -> syscall(sysfail);

"trace"

trace -> syscall(sysonoff,trace,true);
sans-trace -> syscall(sysonoff,trace,false);

"paper"

papier -> syscall(sysonoff,paper,true);
sans-papier -> syscall(sysonoff,paper,false);

"echo"

echo -> syscall(sysonoff,echo,true);
sourd -> syscall(sysonoff,echo,false);

"infinite"

boucle -> syscall(sysonoff,infinite,true);
sans-boucle -> syscall(sysonoff,infinite,false);

"helpers"

eg(X,X) ->;
dif(X,Y) -> syscall(sysdif,X,Y);
G1.G2 -> G1 G2;

"debug: additional rules (not in PII)"

debug -> syscall(sysonoff,debug,true);
sans-debug -> syscall(sysonoff,debug,false);

bt -> syscall(sysbacktrace);
dump -> syscall(sysdump);
