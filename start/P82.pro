"Prolog II (1982) predefined rules"

"session"

quit -> outml("bye!") syscall(sysquit);
list -> syscall(syslist);
insert(F) -> syscall(sysinsert,F);

"i/o"

input-is(F) -> syscall(sysinputis,F);
input(F) -> syscall(sysopen,F,read,S,nil);
close-input -> syscall(sysclosecurrentinput);
close-input(F) -> syscall(syscloseinput,F);
clear-input -> syscall(sysclearinput);

output-is(F) -> syscall(sysoutputis,F);
output(F) -> syscall(sysopen,F,write,S,nil);
close-output -> syscall(sysclosecurrentoutput);
close-output(F) -> syscall(syscloseoutput,F);
flush -> syscall(sysflush);

"the following is needed to run the exercise on Nobel Prizes unmodified, \
even if it does not fit the description in the 1985 Prolog book (p. 297), \
which translates to: add a new file to the stack of buffer files"
new-buffer(t) -> t;

"in"

in(T) -> input-is(S) syscall(sysinterm,S,T);
in-char(C) -> input-is(S) syscall(sysinchar,S,C);

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
    syscall(sysop,200,fx,sub,sub);
