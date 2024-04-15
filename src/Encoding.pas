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


{ encode and decode tuples, predicates, lists, etc., using the functional 
 symbol F }

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
  Errs,
  Memory,
  PObj,
  PObjTerm,
  PObjDef,
  PObjProg;

Function NewTuple( T : TermPtr ) : TermPtr;
Function NewEmptyTuple : TermPtr;
Function TupleQueue( U : TermPtr ) : TermPtr;
Procedure SetTupleQueueTerm( U,T : TermPtr );
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Function TupleArgCount( U : TermPtr ) : Integer;
Function TupleArgN( N : Integer; U : TermPtr ) : TermPtr;

Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    glob : Boolean ) : TermPtr;

Function NewEmptyList( P : ProgPtr ) : TermPtr;
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Function IsNil( T : TermPtr ) : Boolean;
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
Function ListToTuple( L : TermPtr ) : TermPtr;

Function GetTupleHead( T : TermPtr; Reduce : Boolean ) : TermPtr;
Function GetTupleQueue( T : TermPtr; Reduce : Boolean ) : TermPtr;
Function GetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;

Function GetFunc1( T : TermPtr; ident : TString; 
    Var T1 : TermPtr; Reduce : Boolean ) : Boolean;

Function GetList( T : TermPtr; Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Function IsList( T : TermPtr; Reduce : Boolean ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ tuples                                                                     }
{----------------------------------------------------------------------------}

{ create a new tuple containing a single term T: <a> }
Function NewTuple( T : TermPtr ) : TermPtr;
Begin
  NewTuple := NewF(T,Nil)
End;

{ create the empty tuple: <> }
Function NewEmptyTuple : TermPtr;
Begin
  NewEmptyTuple := NewTuple(Nil)
End;

{ return true if U is a tuple }
Function IsTuple( U : TermPtr ) : Boolean;
Begin
  IsTuple := TypeOfTerm(U) = FuncSymbol
End;

{ return the first element of a tuple: <a,b,c> => a; <> => Nil }
Function TupleHead( U : TermPtr ) : TermPtr;
Var 
  FU : FuncPtr Absolute U;
Begin
  TupleHead := FLeftArg(FU)
End;

{ return a tuple containing all the elements in the tuple but the first:
 <a,b,c> => <b,c>, <a,b> => <b>, <a> => Nil, <> => error }
Function TupleQueue( U : TermPtr ) : TermPtr;
Var 
  FU : FuncPtr Absolute U;
Begin
  TupleQueue := FRightArg(FU)
End;

{ replace the queue of tuple U1 with tuple U2: <a,b,c> + <d,e,f> => <a,d,e,f>; 
 mostly used to add an single element to a tuple: <a> + <b> => <a,b> }
Procedure SetTupleQueue( U1,U2 : TermPtr );
Var 
  FU : FuncPtr Absolute U1;
Begin
  FSetRightArg(FU,U2)
End;

{ replace the queue of tuple U with term T }
Procedure SetTupleQueueTerm( U,T : TermPtr );
Begin
  SetTupleQueue(U,NewTuple(T))
End;

{ create a new tuple <T1,T2>, or <T1> if T2 is Nil }
Function NewTuple2( T1,T2 : TermPtr ) : TermPtr;
Var
  U : TermPtr;
Begin
  U := NewTuple(T1);
  If T2 <> Nil Then
    SetTupleQueueTerm(U,T2);
  NewTuple2 := U
End;

{ return true if U is the empty tuple }
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Begin
  IsEmptyTuple := (TupleHead(U) = Nil) And (TupleQueue(U) = Nil)
End;

{ return the number of elements of a tuple }
Function TupleArgCount( U : TermPtr ) : Integer;
Begin
  If TupleQueue(U) = Nil  Then
    TupleArgCount := 1
  Else
    TupleArgCount := TupleArgCount(TupleQueue(U)) + 1
End;

{ return the N-th element of a tuple U }
Function TupleArgN( N : Integer; U : TermPtr ) : TermPtr;
Begin
  If N = 1 Then
    TupleArgN := TupleHead(U)
  Else
    TupleArgN := TupleArgN(N-1,TupleQueue(U))
End;

{ return the first element of a tuple U, setting U to the tuple queue, which 
 will be Nil if there are no more elements) }
Function TupleArg( Var U : TermPtr ) : TermPtr;
Begin
  TupleArg := TupleHead(U);
  U := TupleQueue(U)
End;

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
  IsNil := TermIsIdentifierEqualTo(T,'nil')
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
{ navigating the term tree, possibly through the reduced system              }
{----------------------------------------------------------------------------}

{ tuple head }
Function GetTupleHead( T : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  T := TupleHead(T);
  If Reduce Then
    T := RepresentativeOf(T);
  GetTupleHead := T
End;

{ tuple queue }
Function GetTupleQueue( T : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  T := TupleQueue(T);
  If Reduce Then
    T := RepresentativeOf(T);
  GetTupleQueue := T
End;

{ tuple first arg, advancing U to the queue }
Function GetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  GetTupleArg := GetTupleHead(U,Reduce);
  U := GetTupleQueue(U,Reduce)
End;

{ return True if term T is a 1-argument predicate with name ident, that is,
 a tuple "<ident,t1>"; retrieve the argument }
Function GetFunc1( T : TermPtr; ident : TString; 
    Var T1 : TermPtr; Reduce : Boolean ) : Boolean;
Var
  T0 : TermPtr;
Begin
  GetFunc1 := False;
  If Not IsTuple(T) Then
    Exit;
  T0 := GetTupleArg(T,Reduce);
  If Not IsTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualTo(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce);
  If T <> Nil Then
    Exit;
  GetFunc1 := True
End;

{ return True if term T is a 2-argument predicate with name ident, that is,
 a tuple "<ident,t1,t2>"; retrieve the two arguments }
Function GetFunc2( T : TermPtr; ident : TString; 
    Var T1,T2 : TermPtr; Reduce : Boolean ) : Boolean;
Var
  T0 : TermPtr;
Begin
  GetFunc2 := False;
  If Not IsTuple(T) Then
    Exit;
  T0 := GetTupleArg(T,Reduce);
  If Not IsTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualTo(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce);
  If Not IsTuple(T) Then
    Exit;
  T2 := GetTupleArg(T,Reduce);
  If T <> Nil Then
    Exit;
  GetFunc2 := True
End;

{ return True if term T is a non-empty list: "a.b"; retrieve both arguments }
Function GetList( T : TermPtr; Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Begin
  GetList := GetFunc2(T,'.',T1,T2,Reduce)
End;

{ return True if term T is a non-empty list: "a.b" }
Function IsList( T : TermPtr; Reduce : Boolean ) : Boolean;
Var 
  T1,T2 : TermPtr;
Begin
  IsList := GetList(T,T1,T2,Reduce)
End;

End.
