permutation(nil,nil) ->;
permutation(e.x,z) -> permutation(x,y)
                      insertion(e,y,z);

insertion(e,x,e.x) ->;
insertion(e,f.x,f.y) -> insertion(e,x,y);

-> permutation(1.2.3.nil,x);
-> permutation(3.a.1.b.nil,2.4.c.d.nil);
