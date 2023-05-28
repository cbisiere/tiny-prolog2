"session"

quit -> outml("Bye!") <SYSCALL,QUIT>;
list -> <SYSCALL,LIST>;
insert(f) -> <SYSCALL,INSERT,f>;

"i/o"

input_is(f) -> <SYSCALL,INPUT_IS,f>;
input(f) -> <SYSCALL,INPUT,f>;
close_input -> <SYSCALL,CLOSE_CURRENT_INPUT>;
close_input(f) -> <SYSCALL,CLOSE_INPUT,f>;
clear_input -> <SYSCALL,CLEAR_INPUT>;

output_is(f) -> <SYSCALL,OUTPUT_IS,f>;
output(f) -> <SYSCALL,OUTPUT,f>;
close_output -> <SYSCALL,CLOSE_CURRENT_OUTPUT>;
close_output(f) -> <SYSCALL,CLOSE_OUTPUT,f>;
flush -> <SYSCALL,FLUSH>;

"in"

in(t) -> <SYSCALL,IN_TERM,t>;
in_char(c) -> <SYSCALL,IN_CHAR,c>;

"out"

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
dif(x,y) -> { x <> y };

"compatibility with old dash-in-identifier syntax"

input-is(f) -> input_is(f);
close-input -> close_input;
close-input(f) -> close_input(f);
output-is(f) -> output_is(f);
close-output -> close_output;
close-output(f) -> close_output(f);
in-char(c) -> in_char(c);

