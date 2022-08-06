
permutation(vide,vide) ->;
permutation(e.x,z) -> permutation(x,y)
                      insertion(e,y,z);

insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);

> permutation(1.2.3.vide,x) ?
> permutation(3.a.1.b.vide,2.4.c.d.vide) ?

.
