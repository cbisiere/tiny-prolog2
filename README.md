# A Simple Prolog II Interpreter 
A simple Prolog II interpreter written in Pascal

## Overview
I wrote this program as a course assignment, back in 1988, when I was a student at the University of Aix-Marseilles II, pursuing a MSc in Computer Science and Mathematics ("Diplôme d'Études Approfondies en Informatique et Mathématique"). The course, entitled "Prolog II", was taught by the late [Alain Colmerauer](https://en.wikipedia.org/wiki/Alain_Colmerauer).

The interpreter handles classical Prolog rules, expressed in so-called "Marseilles syntax". Being a simple implementation exercice, it offers no built-in functions or other advanced features.

The source code contains detailed comments (though in French) about the implementation (program encoding and execution).

### Lists
A dot `.` is used to separate items in a list. For instance, the rules defining the insertion of an element before any item in a list could be written as:

```
insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);
```

Note that variable names (e.g., `e`) start with a single letter, while identifiers (e.g., `insertion` start with at least two letters.

Inserting an element in a list of four items gives three different solutions:

```
insertion(0,1.2.nil,x)  ?

{ x = 0.1.2.nil }
{ x = 1.0.2.nil }
{ x = 1.2.0.nil }
```

### Constraints

The interpreter also handles constraints on trees, expressed as equalities (`=`) or inequalities (`<>`). So, for instance, one can write a simple `diff` rule as:

```
diff(x,y) -> { x <> y };
```

Then, the following query gives no results, as there is no `x` satisfying the constraint:

```
diff(x,x)  ?
```

while the following query displays the resulting constraints:  

```
diff(x,1)  ?

{ x = x69 , x69 <> 1 }
```
(Note that the interpreter created an additional variable `x69`, and does not try to get rid of it when displaying the resulting constraint set.)

## Compilation

### Turbo Pascal 3

The program was developed in [Turbo Pascal 3](https://en.wikipedia.org/wiki/Turbo_Pascal#Version_3) (TP3). Turbo Pascal 3.02A is [provided](https://web.archive.org/web/20101124092418/http://edn.embarcadero.com/article/20792) to the Borland community free of charge, as a [zip file] (https://web.archive.org/web/20110815014726/http://altd.embarcadero.com/download/museum/tp302.zip).

You may use a FreeDOS box to install TP3, compile `Main.pas` and run the Prolog interpreter.

### Free Pascal Compiler

The program nicely compiles with [Free Pascal Compiler](https://en.wikipedia.org/wiki/Free_Pascal) (FPC).

To compile the interpreter with FPC, just type:

```bash
fpc -Mtp -FE. -otprolog2 src/Main.pas
```
## Execution

A Prolog program to execute is a text file containing both the program rules and the queries. Rules are written using the old "Marseilles syntax". Each query starts with a `>`and ends with a `?`. A final dot `.` ends the program.

For instance, the program `examples/permu.pro` contains four rules and two queries:  

```
permutation(nil,nil) ->;
permutation(e.x,z) -> permutation(x,y)
                      insertion(e,y,z);

insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);

> permutation(1.2.3.nil,x) ?
> permutation(3.a.1.b.nil,2.4.c.d.nil) ?

.
```
(Note the `nil` has no special meaning in the language. In this example, `nil` is just an identifier used as an end-of-list marker.)

To execute this program, run `tprolog2`. When prompted for the Prolog program to execute, type in the path of the Prolog file, `examples/permu.pro`. The interpreter will compile and display the Prolog program, execute the first query, and display a prompt `Ok`. Hit the return key to execute the next query, until all queries are executed.

The full output will be as follows:

```
cbisiere:~/git/tiny-prolog2$ ./tprolog2
Prolog file to execute: examples/permu.pro

--------------------------------------------------------------

permutation(nil,nil) ->  {  };

permutation(e.x,z) ->
        permutation(x,y)
        insertion(e,y,z) {  };

insertion(e,x,e.x) ->  {  };

insertion(e,f.x,f.y) ->
        insertion(e,x,y) {  };

--------------------------------------------------------------

permutation(1.2.3.nil,x)  ?

{ x = 1.2.3.nil }
{ x = 2.1.3.nil }
{ x = 2.3.1.nil }
{ x = 1.3.2.nil }
{ x = 3.1.2.nil }
{ x = 3.2.1.nil }

Ok


permutation(3.a.1.b.nil,2.4.c.d.nil)  ?

{ a = 2 , b = 4 , c = 3 , d = 1 }
{ a = 2 , b = 4 , c = 1 , d = 3 }
{ a = 4 , b = 2 , c = 3 , d = 1 }
{ a = 4 , b = 2 , c = 1 , d = 3 }

Ok

cbisiere:~/git/tiny-prolog2$
```


## BNF Syntax

```
variable ::= <letter> [<digit>]* [']*

identifier ::= <letter> <letter> [<letter>]* [<digit>]*

integer ::= <digit> [<digit>]*                            

constant ::= <identifier> | <integer>                    


term ::= <simple term> [.<term>]                          

simple term ::= <constant> |                               
                <variable>  |                               
                <identifier> ( <term> [,<term>]* ) |  
                < <term> [,<term>]* > |                   
                ( <term> )                                 

contrainte ::= <term> = <term> |                           
               <term> <> <term>                            

system ::= { <contraint> [,<contraint>]* }                         

rule ::= <term> -> [<term>]* [<system>] ;                

query ::= > [<term>]* [system] ?                                

program-and-queries ::= [<rule>]*  [<query>]* .  

```

## Author

* [Christophe Bisière](https://github.com/cbisiere)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
