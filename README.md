# A Simple Prolog II Interpreter 
A simple Prolog II interpreter written in Pascal

## Why?
2022 is [the 50th anniversary of Prolog](http://prologyear.logicprogramming.org/). As a modest tribute for this anniversary, I decided to dig up a Prolog interpreter I wrote almost 35 years ago, clean it a bit, add a few missing features (e.g., the "cut"), and push it online. As this program remains a toy program, this serves no real purpose other than to celebrate this anniversary.

I wrote this program as a course assignment, back in 1988, when I was a student at the University of Aix-Marseille II, pursuing a MSc in Computer Science and Mathematics ("Diplôme d'Études Approfondies en Informatique et Mathématique"). The course, entitled "Prolog II", was taught by the late [Alain Colmerauer](https://en.wikipedia.org/wiki/Alain_Colmerauer), creator of the language.

## References
Basically, the program implements two algorithms described in the following paper: 

* Alain Colmerauer (1984). [Equations and Inequations on Finite and Infinite Trees](https://www.ueda.info.waseda.ac.jp/AITEC_ICOT_ARCHIVES/ICOT/Museum/FGCS/FGCS84en-proc/84eILEC-1.pdf). FGCS 1984: 85-99. 

Here are two additional interesting papers:

+ Alain Colmerauer (1985). [Prolog in 10 Figures](https://dl.acm.org/doi/pdf/10.1145/214956.214958). Communications of the ACM, vol. 28, num. 12, December.

* Philippe Körner et al. (2022). [Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD). Theory and Practice of Logic Programming, 1-83.


## Overview
The interpreter handles classical Prolog rules, expressed in so-called "Marseille syntax". Being a simple implementation exercice, it offers no built-in functions or other advanced features.

The source code contains detailed comments (though in French) about the implementation (parsing and execution).

### Lists
A dot `.` is used to separate items in a list. For instance, the rules defining the insertion of an element before any item in a list could be written as:

```
insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);
```

Note that variable names (e.g., `e`) start with a single letter, while identifiers (e.g., `insertion` start with at least two letters.

Inserting an element in a list of four items gives three different solutions:

```
-> insertion(0,1.2.nil,x);
{ x = 0.1.2.nil }
{ x = 1.0.2.nil }
{ x = 1.2.0.nil }
```

### Constraints

The interpreter also handles constraints on trees, expressed as equalities (`=`) or inequalities (`<>`). So, for instance, one can write a simple `dif` rule as:

```
dif(x,y) -> { x <> y };
```

Then, the following query gives no results, as there is no `x` satisfying the constraint:

```
-> dif(x,x);
```

while the following query displays the resulting constraints:  

```
-> dif(x,1);
{ x = x_94 , x_94 <> 1 }
```
(Note that the interpreter has created an additional variable `x_94`, and does not try to get rid of it when displaying the resulting constraint set.)

### Strings

Strings are used as values or as comments. They must be double quoted. Inside a string, `"` must be doubled as `""`. Backslash `\`is a continuation character. Comments can appear anywhere outside of rules.

Querying the program

```
"This is a comment"

"This is \
another one"

string("Hello, world!")->;
string("They say: ""hello, world!""")->;
string("Pro\
log")->;
```

with the goal `string(s) outml(s) fails;` gives

```
Hello, world!
They say: "hello, world!"
Prolog
```

The primitive `outml(s)` display the string `s` without the surrounding quotes. 

### The "cut"
As in standard Prolog, when a rule containing a cut (`!` or, equivalently, `/`) is used to execute a goal, the execution of this cut prunes the search tree, making the search engine forget the other ways of executing that goal.

To illustrate how the cut works, consider the following example, taken from the [Prolog II Reference Manual](https://www.prolog-heritage.org/en/m2.html), Section 2.1, page R2-2:


```
color(red) ->;
color(blue) ->;

size(big) ->;
size(small) ->;

choice1(x.y) -> color(x) size(y);
choice1("that's all") ->;

choice2(x.y) -> ! color(x) size(y);
choice2("that's all") ->;

choice3(x.y) -> color(x) ! size(y);
choice3("that's all") ->;

choice4(x.y) -> color(x) size(y) !;
choice4("that's all") ->;
```

The following executions show that what the engine forgets after executing a cut in a certain rule or query is all the rules having the same head, plus all the rules which could have been used to clear the terms between the start of the rule's body (or the start of the query) and the cut.


```
-> choice1(u);
{ u = red.big }
{ u = red.small }
{ u = blue.big }
{ u = blue.small }
{ u = "that's all" }
-> choice2(u);
{ u = red.big }
{ u = red.small }
{ u = blue.big }
{ u = blue.small }
-> choice3(u);
{ u = red.big }
{ u = red.small }
-> choice4(u) ;
{ u = red.big }
-> choice1(u) !;
{ u = red.big }
```


## Compilation

### Turbo Pascal 3

The program was initially developed in [Turbo Pascal 3](https://en.wikipedia.org/wiki/Turbo_Pascal#Version_3) (TP3). Turbo Pascal 3.02A is [provided](https://web.archive.org/web/20101124092418/http://edn.embarcadero.com/article/20792) to the Borland community free of charge, as a [zip file] (https://web.archive.org/web/20110815014726/http://altd.embarcadero.com/download/museum/tp302.zip).

I chose to maintain compatibility with TP3. Because why not.

You may use a FreeDOS box to install TP3, compile `Main.pas` and run the Prolog interpreter.

### Free Pascal Compiler

To compile the interpreter with the [Free Pascal Compiler](https://en.wikipedia.org/wiki/Free_Pascal) (FPC), edit `src/Main.pas`, changing the compiler directive `{$I TP3.pas }` into `{$I FPC.pas }`. Then run FPC in Turbo Pascal mode: 

```bash
fpc -Mtp -FE. -otprolog2 src/Main.pas
```
Alternatively, you may use the following script, which does that editing and then compiles the interpreter:

```bash
fpc.sh
```

## Execution

A Prolog program to execute is a text file containing both the program rules and the queries. Rules are written using the old "Marseille syntax". Each query starts with a `->`and ends with a `;`. The end of the text file, or, alternatively, an additional `;`, ends the program.

For instance, the file `examples/permu.pro` contains four rules and two queries:

```
permutation(nil,nil) ->;
permutation(e.x,z) -> permutation(x,y)
                      insertion(e,y,z);

insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);

-> permutation(1.2.3.nil,x);
-> permutation(3.a.1.b.nil,2.4.c.d.nil);

```
(Note the `nil` has no special meaning in the language. In this example, `nil` is just an identifier used as an end-of-list mark.)

To execute this program, run `tprolog2 examples/permu.pro`: 


```
$ ./tprolog2 examples/permu.pro
Program "examples/permu.pro" loaded
-> permutation(1.2.3.nil,x);
{ x = 1.2.3.nil }
{ x = 2.1.3.nil }
{ x = 2.3.1.nil }
{ x = 1.3.2.nil }
{ x = 3.1.2.nil }
{ x = 3.2.1.nil }
-> permutation(3.a.1.b.nil,2.4.c.d.nil);
{ a = 2, b = 4, c = 3, d = 1 }
{ a = 2, b = 4, c = 1, d = 3 }
{ a = 4, b = 2, c = 3, d = 1 }
{ a = 4, b = 2, c = 1, d = 3 }
>
```

The final `>`is a prompt, inviting you to type in other queries to execute, e.g.:

```
> permutation(1.2.3.nil,3.x.y.nil);
-> permutation(1.2.3.nil,3.x.y.nil);
{ x = 1, y = 2 }
{ x = 2, y = 1 }
>
```

Predefined commands include `list` to list the user current rules:

```
> list fail;
-> list fail ;
permutation(nil,nil) ->;
permutation(e.x,z) ->
        permutation(x,y)
        insertion(e,y,z);
insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) ->
        insertion(e,x,y);
>
```

and `insert(f)` to insert rules and queries from a file with file path `f`. 

When you are done, use `quit` or hit `Ctrl+C` to quit the interpreter.

```
> quit;
-> quit;
Bye!
$
```

## BNF Syntax

```
letter ::= "a" | ... | "z" | "A" | ... | "Z" | "_"
digits ::= <digit>[<digit>]*
letters := <letter>[<letter>]*

variable ::= <letter>[<digits>]["'"]*

identifier ::= <ident-start>[<ident-middle>]*[<ident-end>]

ident-start ::= <letter><letter>[<letters>]
ident-middle ::= "-"<letters>
ident-end ::= <digits>

integer ::= <digits>                            

string ::= """ [<char-no-quotes> | """" | "\"<newline>]* """ 

constant ::= <identifier> | <integer> | <string>                 

term ::= <simple-term> ["."<term>]                          

simple-term ::= <constant> |                               
                <variable>  |                               
                <identifier>"(" <term> ["," <term>]* ")" |  
                "<" <term> ["," <term>]* ">" |                   
                "(" <term> ")"                                 

constraint ::= <term> "=" <term> |
               <term> "<>" <term>          

cut ::= "!" | "/"

system ::= "{" <constraint> ["," <constraint>]* "}"                         

rule ::= <term> "->" [<term>| <cut>]* [<system>] ";"

query ::= -> [<term> | <cut>]* [system] ";"                                

comment ::= <string>

rules-and-queries ::= [<comment> | <rule> | <query>]*

program ::= [<rules-and-queries>]* [";"]

```

## Author

* [Christophe Bisière](https://github.com/cbisiere)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
