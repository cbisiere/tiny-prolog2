
solution(S.E.N.D,M.O.R.E,M.O.N.E.Y) ->
  sans-repetition(S.E.N.D.M.O.R.Y.vide)
  admissible(r1,0,0,M,0)
  admissible(r2,S,M,O,r1)
  admissible(r3,E,O,N,r2)
  admissible(r4,N,R,E,r3)
  admissible( 0,D,E,Y,r4)  { S <> 0 , M <> 0 } ;

admissible(0,u1,u2,u3,r) -> plus(0.u1,0.u2,r.u3);
admissible(1,u1,u2,u3,r) -> plus(0.u1,0.u2,x)
                            plus-un(x,r.u3);

sans-repetition(vide) -> ;
sans-repetition(u.l) -> hors-de(u,l)
                        sans-repetition(l);

hors-de(u,vide) ->;
hors-de(u,v.l)  -> hors-de(u,l)  { u <> v } ;

plus(0.0,x,x) -> inferieur-a-vingt(x);
plus(x',y,z') -> plus-un(x,x')
                 plus(x,y,z)
                 plus-un(z,z');

inferieur-a-vingt(0.0) ->;
inferieur-a-vingt(y) -> plus-un(x,y) ;

plus-un(0.0,0.1) ->;
plus-un(0.1,0.2) ->;
plus-un(0.2,0.3) ->;
plus-un(0.3,0.4) ->;
plus-un(0.4,0.5) ->;
plus-un(0.5,0.6) ->;
plus-un(0.6,0.7) ->;
plus-un(0.7,0.8) ->;
plus-un(0.8,0.9) ->;
plus-un(0.9,1.0) ->;
plus-un(1.0,1.1) ->;
plus-un(1.1,1.2) ->;
plus-un(1.2,1.3) ->;
plus-un(1.3,1.4) ->;
plus-un(1.4,1.5) ->;
plus-un(1.5,1.6) ->;
plus-un(1.6,1.7) ->;
plus-un(1.7,1.8) ->;
plus-un(1.8,1.9) ->;

-> solution(x,y,z);
