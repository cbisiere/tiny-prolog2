
sans-repetition(vide) -> ;
sans-repetition(u.l) -> hors-de(u,l)
                        sans-repetition(l);

hors-de(u,vide) ->;
hors-de(u,v.l)  -> hors-de(u,l)  { u <> v } ;

-> hors-de(3,1.2.3.vide);
-> hors-de(4,9.9.8.7.1.0.vide);
-> hors-de(x,1.2.3.vide);
-> sans-repetition(5.6.7.8.9.3.vide);
-> sans-repetition(6.9.0.8.7.6.vide);
-> sans-repetition(x.8.7.5.1.0.vide);