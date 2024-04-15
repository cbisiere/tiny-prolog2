"Prolog II version 2 (1982) predefined rules"

"worlds"

world(W) -> syscall(sysworld,W);
new-subworld(W) -> syscall(sysnewworld,W);
kill-subworld(W) -> syscall(syskillworld,W);
climb(W) -> syscall(sysclimbworld,W);
climb -> syscall(sysparentworld,W) syscall(sysclimbworld,W);
down(W) -> string(W) syscall(sysdownworld,W);
state -> syscall(sysstateworld);

"statements"

down(N) -> integer(N) syscall(sysdownstatement,N);
down -> down(1);
up(N) -> syscall(sysupstatement,N);
up -> up(1);
top -> syscall(systopstatement);
bottom -> syscall(sysbottomstatement);

find-rule(I) -> syscall(sysfindrule,I);
rule(T,Q) -> syscall(sysrule,T,Q);

insert(F) -> syscall(sysinsert,F);
insert -> input-is(F) syscall(sysinsert,F);
suppress(N) -> syscall(syssuppress,N);

"rules"

list(N) -> syscall(syslist,N);
list -> list(0);
assert(<T,Q>) -> syscall(sysasserta,T,Q);

"session"

quit -> outml("bye!") syscall(sysquit);

"is"

ident(T) -> syscall(sysisdent,T);
integer(T) -> syscall(sysisinteger,T);
real(T) -> syscall(sysisreal,T);
string(T) -> syscall(sysisstring,T);
dot(T) -> syscall(sysisdot,T);
tuple(T) -> syscall(sysistuple,T);

"string"

char-code(C,N) -> syscall(syscharcode,C,N);

"i/o"

input-is(F) -> syscall(sysinputis,F);
input(F) -> syscall(sysselectinput,F) /;
input(F) -> syscall(sysopennew,F,read,S,alias(F));
close-input -> input-is(F) close-input(F);
close-input(F) -> syscall(syscloseinput,F);
clear-input -> syscall(sysclearinput);

output-is(F) -> syscall(sysoutputis,F);
output(F) -> syscall(sysselectoutput,F) /;
output(F) -> syscall(sysopennew,F,write,S,alias(F));
close-output -> output-is(F) close-output(F);
close-output(F) -> syscall(syscloseoutput,F);
flush -> syscall(sysflush);

new-buffer(t) -> syscall(sysnewbuffer) t fail;
new-buffer(t) -> syscall(sysdelbuffer);

"in"

in(T) -> input-is(S) syscall(sysinterm,S,T);
in-char(C) -> input-is(S) syscall(sysinchar,S,C);
in-char'(C) -> input-is(S) syscall(sysincharskipspaces,S,C);

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

block(E,T) ->;
block-exit(E) ->;

"debug"

bt -> syscall(sysbacktrace);
dump -> syscall(sysdump);

"helpers"

eq(X,X) ->;

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
