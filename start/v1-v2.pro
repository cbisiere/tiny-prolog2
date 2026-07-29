"Prolog II predefined rules common to version 1 and version 2"
"English predicates"

"worlds"

world(W) -> syscall(sysworld,W);
subworlds(W) -> syscall(syssubworlds,W); "TODO: name? check doc"
kill-subworld(W) -> syscall(syskillworld,W,true);
climb(W) -> syscall(sysclimbworld,W);
climb -> syscall(sysparentworld,W) syscall(sysclimbworld,W);

"statements"

list(N) -> syscall(syslist,N,false);
list -> list(0);
top -> syscall(systopstatement);
bottom -> syscall(sysbottomstatement);
up(N) -> integer(N) syscall(sysupstatement,N);
up -> up(1);
suppress(N) -> syscall(syssuppress,N);

"worlds and statements"

down(W) -> string(W) syscall(syssyntax,"PIIv1") syscall(sysdownworld,W,true);
down(W) -> string(W) syscall(syssyntax,"PII") syscall(sysdownworld,W,false);
down(N) -> integer(N) syscall(sysdownstatement,N);
down -> down(1);

"rules"

find-rule(I) -> syscall(sysfindrule,I);
insert(F) -> syscall(sysinsert,F);
insert -> syscall(sysinsert,"");
assert(<T,Q>) -> syscall(sysassert2,T,Q,true);

"session"

exit ->
    exm("saving... ") 
    syscall(syseval,sysbackupfile,F)
    syscall(syssavestate,F) quit;
quit -> outml("bye!") syscall(sysquit);

"is"

ident(T) -> syscall(sysis,T,ident);
integer(T) -> syscall(sysis,T,integer);
string(T) -> syscall(sysis,T,string);

free(T) -> syscall(sysfree,T,true);
bound(T) -> syscall(sysfree,T,false);

"char, string, list, tuple"

char-code(C,N) -> syscall(syscharcode,C,N);
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

"infinite"

infinite -> syscall(sysonoff,infinite,true);
finite -> syscall(sysonoff,infinite,false);

"helpers"

eq(X,X) ->;
dif(X,Y) -> syscall(sysdif,X,Y);
G1.G2 -> G1 G2;

"debug: additional rules (not in PII)"

debug -> syscall(sysonoff,debug,true);
no-debug -> syscall(sysonoff,debug,false);

bt -> syscall(sysbacktrace);
dump -> syscall(sysdump);

