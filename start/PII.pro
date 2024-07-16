"Prolog II version 2 (1982) predefined rules"

"worlds"

world(W) -> syscall(sysworld,W);
new-subworld(W) -> syscall(sysnewworld,W);
kill-subworld(W) -> syscall(syskillworld,W,true);
purge(W) -> syscall(syskillworld,W,false);
climb(W) -> syscall(sysclimbworld,W);
climb -> syscall(sysparentworld,W) syscall(sysclimbworld,W);
down(W) -> string(W) syscall(sysdownworld,W,false);

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
assert(T,Q) -> syscall(sysasserta,T,Q);

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

"string"

char-code(C,N) -> syscall(syscharcode,C,N);

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

echo -> syscall(sysecho,true);
no-echo -> syscall(sysecho,false);

"in"

in-char(T) -> syscall(sysinputis,S) syscall(sysin,S,T,char,false,false);
in-char'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,char,true,false);
next-char(T) -> syscall(sysinputis,S) syscall(sysin,S,T,char,false,true);
next-char'(T) -> syscall(sysinputis,S) syscall(sysin,S,T,char,true,true);
in(T) -> syscall(sysinputis,S) syscall(sysin,S,T,term,true,false);
in-integer(T) -> syscall(sysinputis,S) syscall(sysin,S,T,integer,true,false);
in-ident(T) -> syscall(sysinputis,S) syscall(sysin,S,T,ident,true,false);
in-string(T) -> syscall(sysinputis,S) syscall(sysin,S,T,string,true,false);
in-real(T) -> syscall(sysinputis,S) syscall(sysin,S,T,real,true,false);

"out"

out(T) -> syscall(sysout,T);
outm(S) -> syscall(sysoutm,S);
line -> syscall(sysline);
outl(T) -> out(T) line;
outml(S) -> outm(S) line;
page -> syscall(sysclrsrc);
clear -> page fail;

"assign and eval"

assign(I,T) -> syscall(sysassign,I,T);
val(T1,T2) -> syscall(syseval,T1,T2);

"control"

freeze(V,G) -> syscall(sysfreeze,V,G);
fail -> syscall(sysfail);

"debug"

trace -> syscall(systrace,true);
no-trace -> syscall(systrace,false);

"helpers"

eq(X,X) ->;
dif(X,Y) -> syscall(sysdif,X,Y);

"evaluable functions; see Giannesini et al. 1985, p 144 \
Note that expressions (e.g., 3 + 2) are not supported in Prolog II syntax; \
these declarations are used to evaluate val(T1,T2) predicates. \
TODO: eql, mod"
->
    syscall(sysop,700,xfx,inf,inf)
    syscall(sysop,500,yfx,add,add)
    syscall(sysop,500,yfx,sub,sub)
    syscall(sysop,400,yfx,mul,mul)
    syscall(sysop,400,yfx,div,div)
    syscall(sysop,200,fx,add,add)
    syscall(sysop,200,fx,sub,sub)
    fail;

"debug: additional rules (not in PII)"

debug -> syscall(sysdebug,true);
no-debug -> syscall(sysdebug,false);

bt -> syscall(sysbacktrace);
dump -> syscall(sysdump);

