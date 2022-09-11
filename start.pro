"session"

quit -> outml("Bye!") <SYSCALL,QUIT>;
list -> <SYSCALL,LIST>;

"print"

out(t) -> <SYSCALL,OUT,t>;
outm(s) -> <SYSCALL,OUTM,s>;
line -> <SYSCALL,LINE>;
outl(t) -> out(t) line;
outml(s) -> outm(s) line;

"debug"

bt -> <SYSCALL,BACKTRACE>;

"helpers"

eq(x,x)->;
