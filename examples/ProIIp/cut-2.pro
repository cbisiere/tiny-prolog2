"Prolog II+ Reference Manual, Section 2.1, page R2-2"
"Available at: https://www.prolog-heritage.org/en/m2.html"

"First Solution Only"
first_solution_only(b) -> b !;

"If Then Else"
if_then_else(p,a,b) -> p ! a;
if_then_else(p,a,b) -> b;

"Not"
not(p) -> p ! fail;
not(p) ->;

man(Michel) ->;
woman(x) -> not(man(x));

-> woman(Cristina);         
-> woman(Michel);           
-> woman(x) eq(x,Cristina); 
