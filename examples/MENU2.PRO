
dif(x,y) -> { x <> y };

menu(x,y,z) ->
  hors-d-oeuvre(x)
  plat(y)
  dessert(z);

plat(x) -> viande(x);
plat(x) -> poisson(x);

hors-d-oeuvre(x) ->
  hors-d-oeuvre-disponibles(h)
  element-de(x,h);

viande(x) ->
  viandes-disponibles(v)
  element-de(x,v);

poisson(x) ->
  poissons-disponibles(p)
  element-de(x,p);

dessert(x) ->
  desserts-disponibles(d)
  element-de(x,d);

element-de(x,x.u) ->;
element-de(x,u.y) -> element-de(x,y);

hors-d-oeuvre-disponibles(sardine.pate.melon.celeri.nil) ->;

viandes-disponibles(poulet.roti.steak.nil) ->;

poissons-disponibles(merlan.colin.loup.nil) ->;

desserts-disponibles(gateau.fruit.glace.nil) ->;


> element-de(5,5.7.9.0.8.5.nil) ?
> element-de(x,1.2.3.4.4.nil) ?

> viandes-disponibles(v) ?

> viande(x) ?

> plat(p) ?

> menu(h,p,d) ?

> menu(melon,p,d) poisson(p) ?

> menu(h,p,d) dif(h,sardine) dif(p,poulet) ?


.
