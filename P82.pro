"Prolog II (1982) predefined rules"

"session"

quit -> outml("bye!") syscall(sysquit);
list -> syscall(syslist);
insert(F) -> syscall(sysinsert,F);

"i/o"

input-is(F) -> syscall(sysinputis,F);
input(F) -> syscall(sysinput,F);
close-input -> syscall(sysclosecurrentinput);
close-input(F) -> syscall(syscloseinput,F);
clear-input -> syscall(sysclearinput);

output-is(F) -> syscall(sysoutputis,F);
output(F) -> syscall(sysoutput,F);
close-output -> syscall(sysclosecurrentoutput);
close-output(F) -> syscall(syscloseoutput,F);
flush -> syscall(sysflush);

"in"

in(T) -> syscall(sysinterm,T);
in-char(C) -> syscall(sysinchar,C);

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

