quit -> outml("Bye!") <SYSCALL,QUIT>;
list -> <SYSCALL,LIST>;
out(t) -> <SYSCALL,OUT,t>;
outm(s) -> <SYSCALL,OUTM,s>;
line -> <SYSCALL,LINE>;
outl(t) -> out(t) line;
outml(s) -> outm(s) line;
