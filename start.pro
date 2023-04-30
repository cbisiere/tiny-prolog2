"session"

quit -> outml("Bye!") <SYSCALL,QUIT>;
list -> <SYSCALL,LIST>;
insert(f) -> <SYSCALL,INSERT,f>;

"print"

out(t) -> <SYSCALL,OUT,t>;
outm(s) -> <SYSCALL,OUTM,s>;
line -> <SYSCALL,LINE>;
outl(t) -> out(t) line;
outml(s) -> outm(s) line;
page -> <SYSCALL,CLRSRC>;
clear -> page fail;

"assign and eval"

assign(i,t) -> <SYSCALL,ASSIGN,i,t>;
val(t1,t2) -> <SYSCALL,EVAL,t1,t2>;

"debug"

bt -> <SYSCALL,BACKTRACE>;
dump -> <SYSCALL,DUMP>;

"helpers"

eq(x,x)->;
