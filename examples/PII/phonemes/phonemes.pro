"-----------------------------------------------------------"
"             MINI-ANALYSEUR DE PHONEMES                    "
"                                                           "
"    Appliqué au cas des nombres de 0 à 999.999             "
"                                                           "
"                                                           "
"                  Christophe BISIERE                       "
"                                                           "
"-----------------------------------------------------------"
" Ou deterministe sur une liste de termes "

Ou(x,y) -> x /;
Ou(x,y) -> y;

rien(l,r,l,r) ->;

zero(ZZ.EI.RR.AU.nil,r,l,0.r) ->;

un(IN.l,r,l,1.r) ->;

deux(DD.EU.l,r,l,2.r) ->;

trois(TT.RR.WW.AA.l,r,l,3.r) ->;

quatre(KK.AA.TT.RR.l,r,l,4.r) ->;

quat(KK.AA.TT.l,r,l,4.r) ->;

cinq(SS.IN.KK.l,r,l,5.r) ->;

cin(SS.IN.l,r,l,5.r) ->;

six(SS.II.SS.l,r,l,6.r) ->;

si(SS.II.l,r,l,6.r) ->;

sept(SS.AI.TT.l,r,l,7.r) ->;

huit(YI.II.TT.l,r,l,8.r) ->;

hui(YI.II.l,r,l,8.r) ->;

neuf(NN.OE.FF.l,r,l,9.r) ->;

dis(DD.II.SS.l,r,l,10.r) ->;

diz(DD.II.ZZ.l,r,l,10.r) ->;

di(DD.II.l,r,l,10.r) ->;

onze(ON.ZZ.l,r,l,11.r) ->;

douze(DD.OU.ZZ.l,r,l,12.r) ->;

treize(TT.RR.AI.ZZ.l,r,l,13.r) ->;

quatorze(KK.AA.TT.OO.RR.ZZ.l,r,l,14.r) ->;

quinze(KK.IN.ZZ.l,r,l,15.r) ->;

seize(SS.EI.ZZ.l,r,l,16.r) ->;

vin(VV.IN.l,r,l,20.r) ->;

tren(TT.RR.AN.l,r,l,30.r) ->;

quaran(KK.AA.RR.AN.l,r,l,40.r) ->;

cinquan(SS.IN.KK.AN.l,r,l,50.r) ->;

soixan(SS.WW.AA.SS.AN.l,r,l,60.r) ->;

cent(SS.AN.l,r,l,100.r) ->;

mille(MM.II.LL.l,r,l,1000.r) ->;

et(EI.l,r,l,r) ->;

te(TT.l,r,l,r) ->;

eu(EU.l,r,l,r) ->;

nn(NN.l,r,l,r) ->;

" de deux à six "

deux-six(c,l,r,l',r') -> deux(l,r,l',r');
deux-six(c,l,r,l',r') -> trois(l,r,l',r');
deux-six(c,l,r,l',r') -> quatre(l,r,l',r');
deux-six(c,l,r,l',r') -> quat(l,r,l',r');
deux-six(c,l,r,l',r') -> cinq(l,r,l',r');
deux-six(c,l,r,l',r') -> cin(l,r,l',r');
deux-six(SansLien,l,r,l',r') -> six(l,r,l',r');
deux-six(AvecLien,l,r,l',r') -> si(l,r,l',r');

" de huit à neuf "

huit-neuf(SansLien,l,r,l',r') -> huit(l,r,l',r');
huit-neuf(AvecLien,l,r,l',r') -> hui(l,r,l',r');
huit-neuf(c,l,r,l',r') -> neuf(l,r,l',r');

" de sept à neuf "

sept-neuf(c,l,r,l',r') -> sept(l,r,l',r');
sept-neuf(c,l,r,l',r') -> huit-neuf(c,l,r,l',r');

" de deux à neuf "

deux-neuf(c,l,r,l',r') -> deux-six(c,l,r,l',r');
deux-neuf(c,l,r,l',r') -> sept-neuf(c,l,r,l',r');

" meme chose, optionnel "

opt-deux-neuf(c,l,r,l',r') -> deux-neuf(c,l,r,l',r');
opt-deux-neuf(c,l,r,l,r) ->;

" de un à neuf "

un-neuf(c,l,r,l',r') -> un(l,r,l',r');
un-neuf(c,l,r,l',r') -> deux-neuf(c,l,r,l',r');

" dis, avec ou sans laison "

dis-dis(SansLien,l,r,l',r') -> dis(l,r,l',r');
dis-dis(AvecLien,l,r,l',r') -> di(l,r,l',r');

" de douze à seize "

douze-seize(l,r,l',r') -> douze(l,r,l',r');
douze-seize(l,r,l',r') -> treize(l,r,l',r');
douze-seize(l,r,l',r') -> quatorze(l,r,l',r');
douze-seize(l,r,l',r') -> quinze(l,r,l',r');
douze-seize(l,r,l',r') -> seize(l,r,l',r');

" de onze à seize "

onze-seize(l,r,l',r') -> onze(l,r,l',r');
onze-seize(l,r,l',r') -> douze-seize(l,r,l',r');

" dix-sept "

disept(l,r,l',r') ->
   di(l,r,l1,r1)
   sept(l1,r1,l',r');

" de dix-huit à dix-neuf "

dizhuit-dizneuf(c,l,r,l',r') ->
   diz(l,r,l1,r1)
   huit-neuf(c,l1,r1,l',r');

" de dix-sept à dix-neuf "

disept-dizneuf(c,l,r,l',r') -> disept(l,r,l',r');
disept-dizneuf(c,l,r,l',r') -> dizhuit-dizneuf(c,l,r,l',r');

" de douze à dix-neuf "

douze-dizneuf(c,l,r,l',r') -> douze-seize(l,r,l',r');
douze-dizneuf(c,l,r,l',r') -> disept-dizneuf(c,l,r,l',r');

" de dix à dix-neuf "

dis-dizneuf(c,l,r,l',r') -> onze(l,r,l',r');
dis-dizneuf(c,l,r,l',r') -> douze-dizneuf(c,l,r,l',r');
dis-dizneuf(c,l,r,l',r') -> dis-dis(c,l,r,l',r');

" de un à dix-neuf "

un-dizneuf(c,l,r,l',r') -> un-neuf(c,l,r,l',r');
un-dizneuf(c,l,r,l',r') -> dis-dizneuf(c,l,r,l',r');

" racine des nombres de trente à soixante "

tren-soixan(l,r,l',r') -> tren(l,r,l',r');
tren-soixan(l,r,l',r') -> quaran(l,r,l',r');
tren-soixan(l,r,l',r') -> cinquan(l,r,l',r');
tren-soixan(l,r,l',r') -> soixan(l,r,l',r');

" racine des nombres de vingt à soixante "

vin-soixan(l,r,l',r') -> vin(l,r,l',r');
vin-soixan(l,r,l',r') -> tren-soixan(l,r,l',r');

" de vingt à soixante "

vingt-soixante(c,l,r,l',r') ->
   vin-soixan(l,r,l1,r1)
   te(l1,r1,l2,r2)
   et(l2,r2,l3,r3)
   un(l3,r3,l',r');
vingt-soixante(c,l,r,l',r') ->
   vin-soixan(l,r,l1,r1)
   nn(l1,r1,l2,r2)
   deux-neuf(c,l2,r2,l',r');
vingt-soixante(c,l,r,l',r') ->
   vin-soixan(l,r,l1,r1)
   te(l1,r1,l2,r2)
   Ou(eu(l2,r2,l3,r3),rien(l2,r2,l3,r3))
   deux-neuf(c,l3,r3,l',r');
vingt-soixante(c,l,r,l',r') ->
   vin(l,r,l',r');
vingt-soixante(c,l,r,l',r') ->
   tren-soixan(l,r,l1,r1)
   te(l1,r1,l',r');

" de vingt à quatre-vingt-dix-neuf "

vingt-cent(c,l,r,l',r') ->
   soixan(l,r,l1,r1)
   te(l1,r1,l2,r2)
   et(l2,r2,l3,r3)
   onze(l3,r3,l',r');
vingt-cent(c,l,r,l',r') ->
   soixan(l,r,l1,r1)
   te(l1,r1,l2,r2)
   Ou(eu(l2,r2,l3,r3),rien(l2,r2,l3,r3))
   Ou(douze-dizneuf(c,l3,r3,l',r'),dis-dis(c,l3,r3,l',r'));
vingt-cent(c,l,r,l',r') ->
   soixan(l,r,l1,r1)
   nn(l1,r1,l2,r2)
   Ou(douze-dizneuf(c,l2,r2,l',r'),dis-dis(c,l3,r3,l',r'));
vingt-cent(c,l,r,l',r') -> vingt-soixante(c,l,r,l',r');
vingt-cent(c,l,r,l',r') ->
   quatre(l,r,l1,r1)
   eu(l1,r1,l2,r2)
   vin(l2,r2,l3,r3)
   Ou(un-dizneuf(c,l3,r3,l',r'),rien(l3,r3,l',r'));

" de deux à quatre-vingt-dix-neuf "

deux-cent(c,l,r,l',r') -> dis-dizneuf(c,l,r,l',r');
deux-cent(c,l,r,l',r') -> vingt-cent(c,l,r,l',r');
deux-cent(c,l,r,l',r') -> deux-neuf(c,l,r,l',r');

" de un à quatre-vingt-dix-neuf "

un-cent(c,l,r,l',r') -> un(l,r,l',r');
un-cent(c,l,r,l',r') -> deux-cent(c,l,r,l',r');

"-----------------------------------------------------------------"
" dizaines, optionnelles "

opt-dizaine(c,l,r,l',r') ->
   un-cent(c,l,r,l',r');
opt-dizaine(c,l,r,l,r) ->;

" centaines, optionnelles "

opt-centaine(l,r,l',r') ->
   opt-deux-neuf(AvecLien,l,r,l1,r1)
   cent(l1,r1,l',r');
opt-centaine(l,r,l,r) ->;

" milliers, optionnels "

opt-millier(l,r,l',r') ->
   opt-centaine(l,r,l1,r1)
   opt-dizaine(AvecLien,l1,r1,l2,r2)
   mille(l2,r2,l',r');
opt-millier(l,r,l,r) ->;

"-----------------------------------------------------------------"
" de 0 à 999.999 "

nombre(l,r,l',r') -> zero(l,r,l',r');
nombre(l,r,l',r') ->
   opt-millier(l,r,l1,r1)
   opt-centaine(l1,r1,l2,r2)
   opt-dizaine(SansLien,l2,r2,l',r');

" utilitaire pour calculs "

dans(x,x.l) ->;
dans(x,y.l) -> dif(x,y) dans(x,l);

"__________________________________________"
"        calcul du resultat final          "
"__________________________________________"
" de 1 à 99 "

calcul0010(l,l',v) ->
   cal0001(l,l1,v1)
   cal0010(l1,l',v2)
   val(add(v1,mul(v2,10)),v);

" de 1 à 999 "

calcul0100(l,l',v) ->
   calcul0010(l,l1,v1)
   cal0100(l1,l',v2)
   val(add(v1,mul(v2,100)),v);

" de 1 à 999.999 "

calcul1000(l,l',v) ->
   calcul0100(l,l1,v1)
   cal1000(l1,l',v2)
   val(add(v1,mul(v2,1000)),v);

" milliers "

cal1000(1000.l,l',v) -> calcul0100(l,l',v) dif(v,0);
cal1000(1000.l,l,1) ->;
cal1000(l,l,0) ->;

" centaines "

cal0100(100.l,l',v) -> cal0001(l,l',v) dif(v,0);
cal0100(100.l,l,1) ->;
cal0100(l,l,0) ->;

" dizaines "

cal0010(10.20.4.l,l,9) ->;
cal0010(20.4.l,l,8) ->;
cal0010(10.60.l,l,7) ->;
cal0010(x.l,l,v) -> dans(x,10.20.30.40.50.60.nil) val(div(x,10),v);
cal0010(l,l,0) ->;

" unités "

cal0001(x.l,l,x) -> dans(x,1.2.3.4.5.6.7.8.9.nil);
cal0001(x.l,10.l,v) -> dans(x,11.12.13.14.15.16.nil) val(sub(x,10),v);
cal0001(l,l,0) ->;

calcul-tout(0.l,l,0) ->;
calcul-tout(l,l',v) -> calcul1000(l,l',v);

calcul(l,v) -> calcul-tout(l,nil,v);

analyse(l,r,v) -> nombre(l,nil,nil,r) calcul(r,v) /;

run1 ->
   page
   outm("---------- Mini Analyseur de Phonèmes ----------")
   line
   line
   outm("Liste des phonèmes : ")
   in(l)
   in-char(";")
   line
   analyse(l,r,v)
   outm(" ==> ")
   out(r)
   line
   outm(" ==> ")
   out(v)
   line;
run1 -> outm(" ===> ?") line;

run -> run1 /;

;End world: Normal