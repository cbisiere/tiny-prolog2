# Predefined Predicates 

This page lists the predefined predicates implemented so far in the supported flavours of Prolog.

## Prolog II

First columns of the tables below give the French and English versions of the predefined predicates described in the row.

### Worlds
Predicate | Action | Example
--- | --- | ---
`monde(W)` /`world(W)`| unify `W` with the name of the current world | `> world(W);` <br> `{ W="Normal" }` <br> `>`
`new-subworld(W)` | create a new subworld below the current world, and go down to it | `> new-subworld("Facts");` <br> `{  }` <br> `> world(W);` <br> `{ W="Facts" }` <br> `>`
`tuer-monde(W)` / `kill-subworld(W)` | delete subworld `W` only if it has no subworlds itself | `> world(W);` <br> `{ W="Facts" }` <br> `> climb;` <br> `> kill-subworld("Facts");` <br> `{  }` <br> `>`
`purger(W)` / `purge(W)` | same as above, but delete inconditionally    | 
`monter(W)`, `monter`/ `climb(W)`, `climb` | go up the parent word; fail if `W ` is not the parent world |
`descendre(W)` / `down(W)` | go down to subword `W` of the current word; creates subworld `W` if it does not exist (French v1 only)

### Statements

Worlds are populated with statements, that is, comments and rules. In each world, an index defines the _current statement_. The current statement is where statement are inserted or deleted. The following predicates set the current statement of the current world.

Predicate | Action | Example
--- | --- | ---
`descendre(N)`,`descendre` / `down(N)`,`down` | move the index downward by `N` or by 1 | 
`monter(N)` / `up(N)` | move the index upward by `N` | 
`haut` / `top` | move the index up to the first statement | 
`bas` / `bottom` | move the index down to the last statement | 
`supprimer(N)` / `suppress(N)` | suppress `N` statement from the current statement; the statement after the last suppressed statement becomes the current statement | 

### Rules

Predicate | Action | Example
--- | --- | ---
 `lister(N)`, `lister` / `list(N)`,`list` | display on the current input `N` (or all) rules, starting from the current statement
`tete(I)` / `find-rule(I)` | move the index to the first rule with access `I` |
`inserer(F)`, `inserer` / `insert(F)`, `insert` | insert statements before the current index, from file with path `F` or from the current input unit, until an empty statement is read, that is `;;` 
`ajout(<H,Q>)` / `assert(H,Q)` | insert a new rule with head `H` and queue `Q` at the beginning of the group of rules having the same identifier and arity, or at the end of the current world ; `H`is a predicate or an identifier, `Q` is a list of terms | `> assert(data(1),nil) list;` <br> `data(1) ->;` <br> `{  }`<br> `>`
`regle(H,Q)` / `rule(H,Q)` | give all the rules such that `H` and `Q` unify with a rule head and queue (as a list), respectively, at the time the predefined predicate was first cleared | `> assert(data(2),nil);` <br> `{ }` <br> `> assert(data(1),nil);`  <br> `{ }` <br> `> rule(data(n),nil);` <br> `{ n=1 }` <br> `{ n=2 }` <br> `>`

### Input / Output

Prolog II uses a system of stacked input and output units. 

Predicate | Action | Example
--- | --- | ---
`entree(F)`, `sortie(F)` / `input-is(F)`, `output-is(F)` | unify `F` with the current input (output) unit | `> input-is(f);` <br> `{ f="console" }` <br> `>`
`entree(F)`, `sortie(F)` / `input(F)`, `output(F)` | if `F` is not in the input (output) stack yet, open it and stack it; set `F` as the current input (output) unit | `> output("num.txt")` `out(123)` `close-output;` <br> `{  }` <br> `> input("num.txt")` `in-integer(n);` <br> `{ n=123 }` <br> `>` <br>
`ferme-entree(F)`, `ferme-entree`, `ferme-sortie(F)`, `ferme-sortie` / `close-input(F)`, `close-input`,`close-output(F)`, `close-output` | close the input (output) unit `F` or the current input (output) unit
`tampon-neuf(G)`, `new-buffer(G)` | open a new read/write buffer, clear goal `G` and then close the buffer

### Terms: tests

Predicate | Action | Example
--- | --- | ---
`ident(I)` / `ident(I)` | succeed if `I` is an ident | `> ident(yellow);` <br> `{  }` <br> `>`
`entier(N)` / `integer(N)` | succeed if `N` is an integer | `> integer(1);` <br> `{  }` <br> `>`
`dot(L)` / `dot(L)` | succeed if `L` is a list | `> dot(1.nil);` <br> `{  }` <br> `>`
`tuple(U)` / `tuple(U)` | succeed if `U` is a tuple | `> tuple(<x,y>);` <br> `{  }` <br> `>`
`libre(T)` / `free(T)` | succeed if `T` is free | `> free(x);` <br> `{  }` <br> `>`
`pris(T)` / `bound(T)` | succeed if `T` is bound | `> eq(x,1) bound(x);` <br> `{ x=1 }` <br> `>`

### Terms: read and write

Predicate | Action | Example
--- | --- | ---
`in-car(C)`, `in-car'(C)`, `car-apres(C)`, `car-apres'(C)`, `in(T)`, `in-entier(N)`, `in-ident(I)`, `in-chaine(S)` / `in-char(C)`, `in-char'(C)`, `next-char(C)`, `next-char'(C)`, `in(T)`, `in-integer(N)`, `in-ident(I)`, `in-string(S)`, `in-real(R)` | read (or lookup) a character `C`, read a term `T`, an integer `N`, an identifier `I`, a string `S` or a real number `R`; non-primed predicates reading a character do not skip leading spaces; all the others do skip spaces | `> in-char(c);` <br> `{ c="\n" }` <br> `> in-char'(c);` <br> `a` <br> `{ c="a" }` <br> `>` <br>
`ex(T)`, `exl(T)`, `exm(S)`, `exml(S)` / `out(T)`, `outl(T)`,`outm(S)`, `outml(S)` | write term `T`, without or with a newline, or a string `S` without double-quotes, without or with a new line | `> outml("Hello, world!");` <br> `Hello, world!` <br> `{  }` <br> `>`
`ligne`, `page` / `line`, `page` | write a new line, or clear the screen |


### Evaluation

Predicate | Action | Example
--- | --- | ---
`affecter(I,T)` / `assign(I,T)` | assign value `T` to identifier `I`; the identifier becomes a global, persistent variable whose value is `T` | `> assign(greetings,"hello");` <br> `{  }` <br> `> outml(greetings);` <br> `hello` <br> `{  }` <br> `>`
`val(E,V)` / `val(E,V)` | evaluate expression `E` and unify the result with `V` | `> val(add(5,mul(3,2)),x);` <br> `{ x=11 }` <br> `>` 
`eg(X,Y)`, `dif(X,Y)` / `eq(X,Y)`, `dif(X,Y)` | term `X` is equal to (or different from) term `Y` | `> dif(x,1);` <br> `{ x#1 }` <br> `>`

### Operators

Operator | Type | Precedence | Example
--- | --- | --- | ---
`inf` | xfx | 700 | 
`add`,`sub` | yfx | 500 | 
`mul`,`div` | yfx | 400 | 
`add`,`sub` | fx | 200 | 

### Strings

Predicate | Action | Example
--- | --- | ---
`no-car(C,N)` / `char-code(C,N)` | match character `C` and code `N`; either `C` or `N` must be bound | `> char-code("A",n);` <br> `{ n=65 }` <br> `>`

### Control

Predicate | Action | Example
--- | --- | ---
`geler(V,G)` / `freeze(V,G)`| postpone the clearing of goal `G` as long as `V` is free |
`impasse`, `fail` | fail, without displaying a warning that the goal does not exist


### Session

Predicate | Action | Example
--- | --- | ---
`adieu` / `quit` | quit without saving | `> quit;` <br> `bye!` <br> `{  }` <br> `$`
`echo`, `sourd` / `echo`, `no-echo` | set the echo state on or off; when echo is on, inserted Prolog files are echoed to console | 
`trace`, `sans-trace` / `trace`, `no-trace` | set the trace state on or off; when trace is on, cleared goals are displayed | 


## Prolog II+, Marseille syntax

Prolog II+ retains most of the predicates above, written with underscores instead of dashes (e.g. `in_char/1` instead of `in-char/1`. Notable exceptions are world and statement related predicates, which do not exist in Prolog II.

Main supported additions or changes are as follows.

### Input / Output

Predicate | Action | Example
--- | --- | ---
`flush` | flush the current output unit

### Evaluation

Predicate | Action | Example
--- | --- | ---
`op(N,T,S,I)`, `op(N,T,S)` | create a new operator with precedence `N` and bracketing type `T`; when used within an expression, name in string `S` is used; when used as a functor, identifier `I` is used; for `op/3`, `I` is equal to `S` | `+> op(100,fx,'€',euro);` <br> `{  }` <br> `+> eq(x,'€' 10);` <br> `{ x=euro(10) }` <br> `+>`

### Operators

Prolog II+ syntax accepts expressions. Operators used elsewhere in Prolog syntax (e.g. `'<'`, also used in tuples) must be single quoted.

Identifier | Operator | Type | Precedence | Example
--- | --- | --- | --- | ---
`inf`, `infe`, `sup`, `supe` | `'<'`, `'=<'`, `'>'`, `'>='` | xfx | 700 | `+> val(1 '<' 2,x);` <br> `{ x=1 }` <br> `+>`
`add`,`sub` | `+`, `-` | yfx | 500 | `+> val(1+1,x);` <br> `{ x=2 }` <br> `+>`
`mul`,`div` | `*`, `/` | yfx | 400 | 
`add`,`sub` | `+`, `-` | fx | 200 | 
`^`         | `^` | xfy | 200  | `+> val(2^3,x);` <br> `{ x=8 }` <br> `+>`

### Control

Predicate | Action | Example
--- | --- | ---
`not(G)` | fail if goal `G` can be cleared

### Rules

Predicate | Action | Example
--- | --- | ---
`assert(H,Q)`, `asserta(H,Q)` | same as Prolog II's `assert/2` 
`assert''(H,Q)`, `assertz(H,Q)` | same as `assert/2` but create the rule at the end of the group of rules with the same identifier and arity
`retract(H,Q)` | suppress (one at a time) the rules whose head and queue unify with `H` and `Q`, respectively, at the time `retract` was first cleared


## Edinburg syntax

On top of the Prolog II+ (Marseille syntax) predicates, the interpreter supports a few Edinburg-specific Prolog II+ predicates and a few ISO predicates as well.

### Rules

Predicate | Action | Example
--- | --- | --- 
`consult(F)` | insert a Prolog file | `?- consult('examples/menu.pl').` <br> ...
`clause(H,Q)` | same as Prolog II+ `rule(T,Q)` | 
`asserta(H,Q)`, `assertz(H,Q)`| insert a fact at the beginning or at the end of a group of rules | `?- asserta(animal(cat),[]).` <br> `{ }` <br> `?-`
`listing` | display the current rules | `?- listing.` <br> ...


### Input and output

Predicate | Action | Example
--- | --- | ---
`expand_file_name(F,L)`| list files using a pattern | `?- expand_file_name('~/bin/*',L).`
`open(F,M,D,O)`, `open(F,M,D)`| open a stream, setting a descriptor `D` | `?- open('~/data.txt',read,D,alias(data)).`
`close(S)`| close a stream `S` | `?- close(data).`

### Terms: read and write
Predicate | Action | Example
--- | --- | ---
`get_char(S,C)`, `get_char(C)`| read a character from stream `S` or from the current stream | `?- get_char(data,C).`
`put_char(C)` | write a character to the current stream | `?- put_char('a').`
`read(S,T)`, `read(T)`| read a term from stream `S` or from the current stream | `?- read(data,T).`


### Control

Predicate | Action | Example
--- | --- | ---
`once(G)`| clear goal `G` in the first way possible
`true` | always succeed


### Operators

Operator | Type | Precedence | Example
--- | --- | --- | --- 
`is` | xfx | 700 | 
`=`, `<`, `=<`, `>`, `>=` | xfx | 700 | 
`@<`, `@=<`, `@>`, `@>=` | xfx | 700 | 
`\=` | xfx | 700 | 
`=..` | xfx | 700 | 
`+`, `-` | yfx | 500 | 
`*`, `/`, `//` | yfx | 400 | 
`+`, `-` | fx | 200 | 
 `^` | xfy | 200  |
 