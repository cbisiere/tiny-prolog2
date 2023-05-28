" ****************************************************** "
"                                                        "
"      Programme d'interrogation de base de donnée       "
"       appliqué au cas d'une base de Prix Nobel         "
"                                                        "
"           - Chrisophe BISIERE - Mars 1988 -            "
"                                                        "
" ****************************************************** "
" ---------- "
" 30 phrases "
" ---------- "
ph("Qui a obtenu le Prix Nobel de literature en 1901 ?                       ",
   qu(x,pn(x,e,1901,<n,m>,p,li).dif(x,nil).nil)) ->;
ph("Est-ce qu'un Francais a deja obtenu le Prix Nobel de literature ?        ",
   OuiNon(pn(x,e,d,<n,m>,Fr,li).dif(x,nil).nil)) ->;
ph("Un prix a-t-il deja été distribué en literature ?                        ",
   OuiNon(pn(x,e,d,<n,m>,p,li).dif(x,nil).nil)) ->;
ph("Un Francais a-t-il obtenu un Prix Nobel entre 1900 et 1910 ?             ",
   OuiNon(pn(x,e,d,<n,m>,Fr,s).dif(x,nil).entre(d,1900,1910).nil)) ->;
ph("Quels sont les francais Prix Nobel ?                                     ",
   qu(x,pn(x,e,d,<n,m>,Fr,s).dif(x,nil).nil)) ->;
ph("Combien de francais ont été primés ?                                     ",
   combien(pn(x,e,d,<n,m>,Fr,s).dif(x,nil).nil)) ->;
ph("A quel age est mort Romain Rolland ?                                     ",
   qu(a,pn(Rolland,e,d,<n,m>,p,s).age(a,n,m).nil)) ->;
ph("A quel age Romain Rolland a-t-il obtenu son Prix ?                       ",
   qu(a,pn(Rolland,e,d,<n,m>,p,s).age(a,n,d).nil)) ->;
ph("Qui a obtenu son prix avant 35 ans ?                                     ",
   qu(x,pn(x,e,d,<n,m>,p,s).dif(x,nil).age(a,n,d).inferieur(a,35).nil)) ->;
ph("Quels ont-été les Prix Nobel en 1925 ?                                   ",
   qu(x,pn(x,e,1925,<n,m>,p,s).dif(x,nil).nil)) ->;
ph("Quels ont-été les Prix Nobel francais entre 1925 et 1950 ?               ",
   qu(x,pn(x,e,d,<n,m>,Fr,s).dif(x,nil).entre(d,1925,1950).nil)) ->;
ph("Quels Prix Nobel sont morts en 1980 ?                                    ",
   qu(x,pn(x,e,d,<n,1980>,p,s).dif(x,nil).nil)) ->;
ph("Qui a obtenu le Prix Nobel d'Economie avant 1950 ?                       ",
   qu(x,pn(x,e,d,<n,m>,p,eco).dif(x,nil).inferieur(d,1950).nil)) ->;
ph("Quel Prix Nobel a vecu moins de 40 ans ?                                 ",
   qu(x,pn(x,e,d,<n,m>,p,s).dif(x,nil).age(a,n,m).inferieur(a,40).nil)) ->;
ph("Qui a obtenu le Prix Nobel l'année de sa mort ?                          ",
   qu(x,pn(x,e,d,<n,d>,p,s).dif(x,nil).nil)) ->;
ph("Pourquoi les francais ont-ils été récompensé entre 1925 et 1950 ?        ",
   qu(s,pn(x,e,d,<n,m>,Fr,s).dif(s,nil).entre(d,1925,1950).nil)) ->;
ph("Combien de fois la France a-t-elle obtenue le Prix Nobel d'Economie ?    ",
   combien(pn(x,e,d,<n,m>,Fr,eco).nil)) ->;
ph("Quelle était la nationalité du Prix Nobel de literature en 1901 ?        ",
   qu(p,pn(x,e,1901,<n,m>,p,li).dif(p,nil).nil)) ->;
ph("De quelle nationalité est le Prix Nobel Romain Rolland ?                 ",
   qu(p,pn(Rolland,e,d,<n,m>,p,s).dif(p,nil).nil)) ->;
ph("Quelle est l'année de naissance du Prix Nobel de la paix en 1982 ?       ",
   qu(n,pn(x,e,1982,<n,m>,p,pea).dif(n,nil).nil)) ->;
ph("Quel était le nom, la nationalité et la spécialité des Prix Nobel de 1925 ?"
   ,qu(<x,p,s>,pn(x,e,1925,<n,m>,p,s).dif(x,nil).dif(p,nil).dif(s,nil).nil)) ->;
ph("Combien d'années a vecu le Prix Nobel d'Economie de 1950 ?               ",
   qu(a,pn(x,e,1950,<n,m>,p,eco).dif(x,nil).age(a,n,m).nil)) ->;
ph("Quel est l'age moyen des Prix Nobel en literature l'année de leur prix ? ",
   qu(a-ge,lister(a,pn(x,e,d,<n,m>,p,li).age(a,n,d).nil,l).moyenne(l,a-ge).nil))
    ->;
ph("Quel était l'age moyen des Prix Nobel de 1925 ?                          ",
   qu(a-ge,lister(a,pn(x,e,1925,<n,m>,p,s).age(a,n,1925).nil,l).moyenne(l,a-ge).
   nil)) ->;
ph("Nom et date de naissance des Prix Nobel de literature de plus de 60 ans ?",
   qu(<x,n>,pn(x,e,d,<n,m>,Fr,li).dif(x,nil).age(a,n,m).superieur(a,60).nil)) ->
   ;
ph("Age du plus jeune Prix Nobel de literature au moment de sa nomination ?  ",
   qu(a-ge,lister(<a,<>>,pn(x,e,d,<n,m>,p,li).age(a,n,d).nil,l).minimum(l,<a-ge,
   <>>).nil)) ->;
ph("Nom, nationalité et la date de recompence du plus jeune nobel de la Paix ?",
   qu(<d-ate,i-dent,n-ation>,lister(<a,<d,x,p>>,pn(x,e,d,<n,m>,p,pea).dif(x,nil)
   .superieur(d,1945).age(a,n,d).nil,l).minimum(l,<a,<d-ate,i-dent,n-ation>>).
   nil)) ->;
ph("A quelle date a été distribué le premier Prix Nobel de la paix ?          ",
   qu(d-ate,lister(<d,<>>,pn(x,e,d,<n,m>,p,pea).dif(d,nil).nil,l).minimum(l,<
   d-ate,<>>).nil)) ->;
ph("Qui a obtenu le premier prix d'economie ?                                 ",
   qu(i-dent,lister(<d,<x>>,pn(x,e,d,<n,m>,p,eco).dif(d,nil).dif(x,nil).nil,l).
   minimum(l,<d-ate,<i-dent>>).nil)) ->;
ph("Est-ce-que le premier prix de literature était un francais ?              ",
   OuiNon(lister(<d,<p>>,pn(x,e,d,<n,m>,p,li).nil,l).dif(x,nil).dif(d,nil).
   minimum(l,<d-ate,<Fr>>).nil)) ->;

" ---------------------- "
" lancement du programme "
" ---------------------- "

go ->
   assign(question,0)
   ph(x,y)
   incrementer(question)
   val(question,q)
   outm("Question ")
   out(q)
   outm(" : ")
   line
   outm(x)
   line
   outm("Reponse : ")
   line
   y
   line
   line
   outm("Pressez <RETURN>")
   in-char(c)
   line
   impasse;
go -> outm("Bonsoir...");

" ------------------- "
" questions possibles "
" ------------------- "

qu(x,p) ->
   assign(ok,0)
   effacer-liste(p)
   assign(ok,1)
   line
   outm("          ")
   out(x)
   impasse;
qu(x,p) -> val(ok,1);
qu(x,p) ->
   val(ok,0)
   line
   outm("          Pas de réponse ...")
   line;

combien(p) ->
   compter(c,p)
   line
   outm("          ")
   out(c);

OuiNon(p) ->
   effacer-liste(p)
   outm("          Oui")
   /;
OuiNon(p) ->
   outm("          Non");

" ---------------------------------------- "
" utiliaires pour traitement des questions "
" ---------------------------------------- "

effacer-liste(nil) ->;
effacer-liste(x.r) ->
   x
   effacer-liste(r);

compter(n,p) ->
   assign(compteur,0)
   effacer-liste(p)
   incrementer(compteur)
   impasse;
compter(n,p) ->
   val(compteur,n);

incrementer(i) ->
   val(add(i,1),x)
   assign(i,x);

age(a,n-aiss,m-ort) ->
   dif(n-aiss,nil)
   dif(m-ort,nil)
   val(sub(m-ort,n-aiss),a);

" ------------ "
" comparaisons "
" ------------ "

inferieur(a,b) ->
   dif(a,nil)
   dif(b,nil)
   val(inf(a,b),1);

inferieur-ou-egal(a,b) ->
   dif(a,nil)
   dif(b,nil)
   val(inf(b,a),0);

superieur(a,b) ->
   dif(a,nil)
   dif(b,nil)
   val(inf(b,a),1);

superieur-ou-egal(a,b) ->
   dif(a,nil)
   dif(b,nil)
   val(inf(a,b),0);

entre(x,v1,v2) ->
   superieur-ou-egal(x,v1)
   inferieur-ou-egal(x,v2);

" -------------------------- "
" traitements sur les listes "
" -------------------------- "

moyenne(nil,0) ->;
moyenne(l,m) ->
   taille-liste(l,t)
   somme-liste(l,s)
   val(div(s,t),m);

taille-liste(nil,0) ->;
taille-liste(x.l,t) ->
   taille-liste(l,t')
   val(add(t',1),t);

somme-liste(nil,0) ->;
somme-liste(x.l,s) ->
   somme-liste(l,s')
   val(add(s',x),s);

minimum(x.nil,x) ->;
minimum(<c,v>.l,m) ->
   dif(l,nil)
   minimum(l,<c',v'>)
   Si-Alors-Sinon(val(inf(c,c'),1),egal(m,<c,v>),egal(m,<c',v'>));

maximum(x.nil,x) ->;
maximum(<c,v>.l,m) ->
   dif(l,nil)
   maximum(l,<c',v'>)
   Si-Alors-Sinon(val(inf(c,c'),1),egal(m,<c',v'>),egal(m,<c,v>));

Si-Alors-Sinon(c,p,q) ->
   c
   p
   /;
Si-Alors-Sinon(c,p,q) ->
   q;

" ------------------------------------------------- "
" recuperation des solutions x de p dans la liste l "
" ------------------------------------------------- "

lister(x,p,l) ->
   new-buffer(lister-bis(x,p,l));

lister-bis(x,p,l) ->
   effacer-liste(p)
   output-is(o)
   output("buffer")
   out(x)
   outm(".")
   output(o)
   impasse;
lister-bis(x,p,l) ->
   output-is(o)
   output("buffer")
   outm("nil;")
   output(o)
   input-is(i)
   input("buffer")
   in(l)
   input(i);

" ----------- "
" utilitaires "
" ----------- "

egal(x,x) ->;

;End world: nobel.pro