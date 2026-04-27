"Prolog II version 2 (1982) predefined rules"

"worlds"

world(W) -> syscall(sysworld,W);
new-subworld(W) -> syscall(sysnewworld,W);
kill-subworld(W) -> syscall(syskillworld,W,true);
purge(W) -> syscall(syskillworld,W,false);
climb(W) -> syscall(sysclimbworld,W);
climb -> syscall(sysparentworld,W) syscall(sysclimbworld,W);
down(W) -> string(W) syscall(sysdownworld,W,false);
subworlds(W) -> syscall(syssubworlds,W); "TODO: name? check doc"

"statements"

down(N) -> integer(N) syscall(sysdownstatement,N);
down -> down(1);
up(N) -> integer(N) syscall(sysupstatement,N);
up -> up(1);
top -> syscall(systopstatement);
bottom -> syscall(sysbottomstatement);
suppress(N) -> syscall(syssuppress,N);

"rules"

rule(T,Q) -> syscall(sysrule,T,Q);

find-rule(I) -> syscall(sysfindrule,I);

insert(F) -> syscall(sysinsert,F);
insert -> syscall(sysinputis,F) syscall(sysinsert,F);
assert(T,Q) -> syscall(sysassert2,T,Q,true);

list(N) -> syscall(syslist,N);
list -> list(0);

"session"

quit -> outml("bye!") syscall(sysquit);

"is"

ident(T) -> syscall(sysis,T,ident);
integer(T) -> syscall(sysis,T,integer);
real(T) -> syscall(sysis,T,real);
string(T) -> syscall(sysis,T,string);
dot(T) -> syscall(sysis,T,dot);
tuple(T) -> syscall(sysis,T,tuple);

free(T) -> syscall(sysfree,T,true);
bound(T) -> syscall(sysfree,T,false);

"char, string, list, tuple"

char-code(C,N) -> syscall(syscharcode,C,N);
substring(S1,N1,N2,S2) -> syscall(syssubstring,S1,N1,N2,S2);
find-pattern(S1,S2,N) -> syscall(sysfindpattern,S1,S2,N);
string-ident(S,I) -> syscall(sysstringident,S,I);
list-string(L,S) -> syscall(sysliststring,L,S);
list-tuple(L,T) -> syscall(syslisttuple,L,T);
split(T,L) -> syscall(syssplit,T,L);
arg(N,T1,T2) -> syscall(sysarg,N,T1,T2);

"array"

def-array(I,N) -> syscall(sysdefarray,I,N);

"i/o"

input-is(F) -> syscall(sysinputis,F);
input(F) -> syscall(sysselectinput,F) /;
input(F) -> syscall(sysopennew,F,read,S,nil);
close-input -> syscall(sysinputis,F) close-input(F);
close-input(F) -> syscall(syscloseinput,F);
clear-input -> syscall(sysclearinput);

output-is(F) -> syscall(sysoutputis,F);
output(F) -> syscall(sysselectoutput,F) /;
output(F) -> syscall(sysopennew,F,write,S,nil);
close-output -> syscall(sysoutputis,F) close-output(F);
close-output(F) -> syscall(syscloseoutput,F);
flush -> syscall(sysflush);

new-buffer(T) -> syscall(sysnewbuffer) T syscall(sysdelbuffer);

"in: char"

in-char(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,false,false);
in-char'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,true,false);
next-char(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,false,true);
next-char'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,char,true,true);
end-of-line(T) -> syscall(syssubeol,T); "FIXME: check name"

"in: others"

in(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,term,true,false);
in-integer(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,integer,true,false);
in-ident(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,ident,true,false);
in-string(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,string,true,false);
in-real(T) -> syscall(sysinputis,S) syscall(sysin,S,T,X,real,true,false);
in-sentence(T1,T2) -> syscall(sysinputis,S) syscall(sysin,S,T1,T2,sentence,true,false);

"out"

out(T) -> syscall(sysout,T);
outm(S) -> syscall(sysoutm,S);
line -> syscall(sysline);
outl(T) -> out(T) line;
outml(S) -> outm(S) line;
page -> syscall(sysclrsrc);
clear -> page fail;
set-cursor(N1,N2) -> syscall(sysgotoxy,N1,N2);

"FIXME: names of following predicates are guessed from PII+ doc"
set-line-width(N) -> syscall(syssetlinewidth,N);
line-width(N) -> syscall(sysgetlinewidth,N);
set-line-cursor(N) -> syscall(syssetlinecursor,N);

"assign and eval"

assign(I,T) -> syscall(sysassign,I,T);
val(T1,T2) -> syscall(syseval,T1,T2);

"control"

freeze(V,G) -> syscall(sysfreeze,V,G);
block(T,G) -> syscall(sysblock,T,G);
block-exit(T) -> syscall(sysblockexit,T);
fail -> syscall(sysfail);

"date/time"

time(V) -> syscall(systime,V,startofday,integer);

"trace"

trace -> syscall(sysonoff,trace,true);
no-trace -> syscall(sysonoff,trace,false);

"paper"

paper -> syscall(sysonoff,paper,true);
no-paper -> syscall(sysonoff,paper,false);

"echo"

echo -> syscall(sysonoff,echo,true);
no-echo -> syscall(sysonoff,echo,false);

"helpers"

eq(X,X) ->;
dif(X,Y) -> syscall(sysdif,X,Y);
G1.G2 -> G1 G2;

"evaluable functions; see Giannesini et al. 1985, p 144 "
"Note that expressions (e.g., 3 + 2) are not supported in Prolog II syntax; "
"these declarations are used to evaluate val(T1,T2) predicates. "
->
    syscall(sysop,700,xfx,inf,inf)
    syscall(sysop,700,xfx,eql,eql)
    syscall(sysop,500,yfx,add,add)
    syscall(sysop,500,yfx,sub,sub)
    syscall(sysop,400,yfx,mul,mul)
    syscall(sysop,400,yfx,div,div)
    syscall(sysop,400,yfx,mod,mod)
    syscall(sysop,200,fx,add,add)
    syscall(sysop,200,fx,sub,sub)
    fail;

"debug: additional rules (not in PII)"

debug -> syscall(sysonoff,debug,true);
no-debug -> syscall(sysonoff,debug,false);

bt -> syscall(sysbacktrace);
dump -> syscall(sysdump);

