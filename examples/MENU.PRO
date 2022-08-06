

hors-d-oeuvre(radis) ->;
hors-d-oeuvre(pates) ->;

poisson(sole) ->;
poisson(thon) ->;

viande(porc)  ->;
viande(boeuf) ->;

dessert(glace) ->;
dessert(fruit) ->;

repas(h,p,d) -> hors-d-oeuvre(h)
                plat(p)
                dessert(d);

plat(p) -> poisson(p);
plat(p) -> viande(p);

repas-leger(h,p,d) -> repas(h,p,d)
                      points(h,x)
                      points(p,y)
                      petite-somme(x,y,u)
                      points(d,z)
                      petite-somme(z,u,v);

points(boeuf,3) ->;
points(fruit,1) ->;
points(glace,5) ->;
points(pates,6) ->;
points(porc ,7) ->;
points(radis,1) ->;
points(sole ,2) ->;
points(thon ,4) ->;

petite-somme(1,x,y) -> petit-successeur(x,y);
petite-somme(x',y,z') -> petit-successeur(x,x')
                         petite-somme(x,y,z)
                         petit-successeur(z,z');

petit-successeur(1,2) ->;
petit-successeur(2,3) ->;
petit-successeur(3,4) ->;
petit-successeur(4,5) ->;
petit-successeur(5,6) ->;
petit-successeur(6,7) ->;
petit-successeur(7,8) ->;
petit-successeur(8,9) ->;

> points(m,x) ?
> repas-leger(h,p,d) ?
.

