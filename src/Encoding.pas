{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Encoding.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            E N C O D I N G   O F   C O M P O U N D   T E R M S             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }


{ encode and decode predicates, lists, etc., using the functional symbol F }

{-----------------------------------------------------------------------}
{                                                                       }
{   The functional symbol F is used to encode a predicate               }
{    name(arg1,arg2,...,argN), or equivalently a tuple                  }
{    <name,arg1,arg2,...,argN>, as follows:                             }
{                                                                       }
{           F                                                           }
{          / \                 - tuple: the name of the predicate is    }
{       name  F                  the first argument: <name,arg1,...>    }
{            / \                                                        }
{         arg1  F              -  arg1 .. argN can also be predicates   }
{              / \                                                      }
{           arg2   ...                                                  }
{                   \          - a '.' (Prolog list) is considered as   }
{                    F           a regular predicate.                   }
{                  /  \                                                 }
{               argN    Nil                                             }
{                                                                       }
{-----------------------------------------------------------------------}

Unit Encoding;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjDef,
  PObjProg,
  Tuple;

Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    glob : Boolean ) : TermPtr;

Function NewEmptyList( P : ProgPtr ) : TermPtr;
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Function IsNil( T : TermPtr ) : Boolean;
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
Function ListToTuple( L : TermPtr ) : TermPtr;

Function ProtectedGetTupleHead( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleQueue( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetFunc1( T : TermPtr; ident : TString; Var T1 : TermPtr; 
    Reduce : Boolean ) : Boolean;

Function ProtectedGetList( T : TermPtr; Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Function ProtectedIsList( T : TermPtr; Reduce : Boolean ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ functions                                                                  }
{----------------------------------------------------------------------------}

{ return a new 1 or 2-argument predicate with identifier given as a Pascal 
 string: "ident: "ident(T1)" or "ident(T1,T2)" }
Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    glob : Boolean ) : TermPtr;
Var
  U : TermPtr;
Begin
  CheckCondition(T1 <> Nil,'NewFunc2: first argument is Nil');
  U := NewTuple(EmitShortIdent(P,ident,glob));
  SetTupleQueueTerm(U,T1);
  If T2 <> Nil Then
    SetTupleQueueTerm(TupleQueue(U),T2);
  NewFunc2 := U
End;


{----------------------------------------------------------------------------}
{ lists                                                                      }
{----------------------------------------------------------------------------}

{ return the head of a list: a.b.nil => a }
Function ListHead( L : TermPtr ) : TermPtr;
Begin
  ListHead := TupleArgN(2,L)
End;

{ return the queue of a list: a.b.nil => b.nil }
Function ListQueue( L : TermPtr ) : TermPtr;
Begin
  ListQueue := TupleArgN(3,L)
End;

{ return an empty list "nil" }
Function NewEmptyList( P : ProgPtr ) : TermPtr;
Begin
  NewEmptyList := EmitShortIdent(P,'nil',True)
End;

{ return a term "a.b", viewed as '.'(a,b)" (equivalent to <'.',a,b>) and thus 
 implemented as "F('.',F(a,F(b,Nil)))"; if b is Nil, replace it with 
 "F('nil',Nil)", that is, add ".nil" at the end of the dotted list; this helps 
 implementing the following equivalences:
  [a|b] <=> a.b
  [a,b] <=> a.b.nil
 (see PII+ syntax (cf. pdf doc p.45) }
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Begin
  NewList2 := Nil;
  If T2 = Nil Then
    T2 := NewEmptyList(P);
  NewList2 := NewFunc2(P,'.',T1,T2,True)
End;

{ return True if term T is 'nil' }
Function IsNil( T : TermPtr ) : Boolean;
Begin
  IsNil := TermIsIdentifierEqualToShortString(T,'nil')
End;


{----------------------------------------------------------------------------}
{ conversions                                                                }
{----------------------------------------------------------------------------}

{ create a new list foo.a.b.nil from a tuple <foo,a,b> }
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
Var
  L : TermPtr;
Begin
  If U = Nil Then
    TupleToList := Nil
  Else
  Begin
    L := TupleToList(P,TupleQueue(U));
    TupleToList := NewList2(P,TupleHead(U),L)
  End
End;

{ create a new tuple <foo,a,b>  from a list foo.a.b.nil }
Function ListToTuple( L : TermPtr ) : TermPtr;
Var
  T : TermPtr;
Begin
  If L = Nil Then
    ListToTuple := Nil
  Else
  Begin
    T := ListToTuple(ListQueue(L));
    ListToTuple := NewTuple2(ListHead(L),T)
  End
End;


{----------------------------------------------------------------------------}
{ navigate the term tree, possibly through the reduced system                }
{----------------------------------------------------------------------------}

{ tuple head }
Function GetTupleHead( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  T := TupleHead(T);
  T := RepresentativeOf(T,True,Reduce,g);
  GetTupleHead := T
End;

{ tuple queue }
Function GetTupleQueue( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  T := TupleQueue(T);
  T := RepresentativeOf(T,True,Reduce,g);
  GetTupleQueue := T
End;

{ tuple first arg, advancing U to the queue }
Function GetTupleArg( Var U : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  GetTupleArg := GetTupleHead(U,Reduce,g);
  U := GetTupleQueue(U,Reduce,g)
End;


{----------------------------------------------------------------------------}
{ loop-protected functions, which uses their own serial number               }
{----------------------------------------------------------------------------}

{ tuple head }
Function ProtectedGetTupleHead( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleHead := GetTupleHead(U,Reduce,NewSerial)
End;

{ tuple queue }
Function ProtectedGetTupleQueue( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleQueue := GetTupleQueue(U,Reduce,NewSerial)
End;

{ tuple first arg, advancing U to the queue }
Function ProtectedGetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleArg := GetTupleArg(U,Reduce,NewSerial)
End;

{ return True if term T is a 1-argument predicate with name ident, that is,
 a tuple "<ident,t1>"; retrieve the argument; Reduce means reducing
 the children, not the term itself }
Function ProtectedGetFunc1( T : TermPtr; ident : TString; Var T1 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Var
  g : TSerial;
  T0 : TermPtr;
Begin
  ProtectedGetFunc1 := False;
  If Not IsTuple(T) Then
    Exit;
  g := NewSerial;
  T0 := GetTupleArg(T,Reduce,g);
  If Not IsTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualToShortString(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce,g);
  If T <> Nil Then
    Exit;
  ProtectedGetFunc1 := True
End;

{ return True if term T is a 2-argument predicate with name ident, that is,
 a tuple "<ident,t1,t2>"; retrieve the two arguments; Reduce means reducing
 the children, not the term itself }
Function ProtectedGetFunc2( T : TermPtr; ident : TString; 
    Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Var
  g : TSerial;
  T0 : TermPtr;
Begin
  ProtectedGetFunc2 := False;
  If Not IsTuple(T) Then
    Exit;
  g := NewSerial;
  T0 := GetTupleArg(T,Reduce,g);
  If Not IsTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualToShortString(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce,g);
  If Not IsTuple(T) Then
    Exit;
  T2 := GetTupleArg(T,Reduce,g);
  If T <> Nil Then
    Exit;
  ProtectedGetFunc2 := True
End;

{ return True if term T is a non-empty list: "a.b"; retrieve both arguments }
Function ProtectedGetList( T : TermPtr; Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Begin
  ProtectedGetList := ProtectedGetFunc2(T,'.',T1,T2,Reduce)
End;

{ return True if term T is a non-empty list: "a.b" }
Function ProtectedIsList( T : TermPtr; Reduce : Boolean ) : Boolean;
Var 
  T1,T2 : TermPtr;
Begin
  ProtectedIsList := ProtectedGetList(T,T1,T2,Reduce)
End;

End.
