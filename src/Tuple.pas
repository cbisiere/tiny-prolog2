{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Tuple.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   E N C O D I N G   O F   T U P L E S                      }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }


{ encode and decode tuples using the functional symbol F }

{-----------------------------------------------------------------------}
{                                                                       }
{   The functional symbol F is used to encode tuples                    }
{    <arg1,arg2,...,argN>, as follows:                                  }
{                                                                       }
{             F                                                         }
{            / \                                                        }
{         arg1  F                                                       }
{              / \                                                      }
{           arg2   .                                                    }
{                   .                                                   }
{                    .                                                  }
{                     F                                                 }
{                   /  \                                                }
{                argN    F  = <>                                        }
{                      /  \                                             }
{                   Nil    Nil                                          }
{                                                                       }
{-----------------------------------------------------------------------}

Unit Tuple;

Interface

Uses
  Num,
  Serial,
  PObjTerm,
  PObjFCVI;

Type
  TTupleArgNumber = PosInt; { tuple argument index or count }

{ constructors: tuples }
Function NewEmptyTuple : TermPtr;
Function NewTuple1( T : TermPtr ) : TermPtr;
Function NewTuple2( T1,T2 : TermPtr ) : TermPtr;
Function NewTuple3( T1,T2,T3 : TermPtr ) : TermPtr;
Function NewTuple( T,U : TermPtr ) : TermPtr;

{ constructors: predicates }
Function NewFunc1( Id : IdPtr; T : TermPtr ) : TermPtr;
Function NewFunc2( Id : IdPtr; T1,T2 : TermPtr ) : TermPtr;

{ methods 1 }
Function IsTuple( T : TermPtr ) : Boolean;
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Function TupleHead( U : TermPtr ) : TermPtr;
Procedure SetTupleQueue( U1,U2 : TermPtr );
Procedure SetTupleHeadTerm( U,T : TermPtr );
Function TupleQueue( U : TermPtr ) : TermPtr;
Procedure SetTupleQueueTerm( U,T : TermPtr );
Function TupleArgCount( U : TermPtr ) : TTupleArgNumber;
Function TupleArgN( N : TTupleArgNumber; U : TermPtr ) : TermPtr;
Function TupleArg( Var U : TermPtr ) : TermPtr;

{ methods 2 (reduced system) }
Function GetTupleHead( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Function GetTupleQueue( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Function GetTupleArgN( N : TTupleArgNumber; T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Function GetTupleArg( Var U : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;

{ methods 2 (loop detection) }

Function ProtectedGetIsTuple( U : TermPtr; Reduce : Boolean ) : Boolean;
Function ProtectedGetTupleHead( U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleQueue( U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleArgCount( U : TermPtr; 
    Reduce : Boolean ) : TTupleArgNumber;
Function ProtectedGetTupleArgN( N : TTupleArgNumber; U : TermPtr; 
    Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Procedure ProtectedGetTupleMetaData( T : TermPtr; 
    Var I : IdPtr; Var A : TArity );

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ constructors: tuples                                                       }
{----------------------------------------------------------------------------}

{ create the empty tuple: <> }
Function NewEmptyTuple : TermPtr;
Begin
  NewEmptyTuple := Func_NewAsTerm(Nil,Nil)
End;

{ create a new tuple containing a single term T: <a> }
Function NewTuple1( T : TermPtr ) : TermPtr;
Begin
  NewTuple1 := NewTuple(T,NewEmptyTuple)
End;

{ create a new tuple containing two terms T1,T2: <a,b> }
Function NewTuple2( T1,T2 : TermPtr ) : TermPtr;
Begin
  NewTuple2 := NewTuple(T1,NewTuple1(T2))
End;

{ create a new tuple containing three terms T1,T2,T3: <a,b,c> }
Function NewTuple3( T1,T2,T3 : TermPtr ) : TermPtr;
Var
  T,Q : TermPtr;
Begin
  T := NewTuple2(T1,T2); { T is <T1,T2> }
  Q := TupleQueue(T); { Q is <T2> }
  SetTupleQueue(Q,NewTuple1(T3)); { T is <T1,T2,T3> }
  NewTuple3 := T
End;

{ create a new tuple from a head term T and a queue tuple U }
Function NewTuple( T,U : TermPtr ) : TermPtr;
Begin
  NewTuple := Func_NewAsTerm(T,U)
End;

{----------------------------------------------------------------------------}
{ constructors: predicates                                                   }
{----------------------------------------------------------------------------}

{ predicates (a.k.a. functions) are encoded as tuples }

{ return a new 1-argument predicate with identifier Id and argument T: Id(T) 
 (that is, tuple <Id,T>) }
Function NewFunc1( Id : IdPtr; T : TermPtr ) : TermPtr;
Begin
  NewFunc1 := NewTuple2(TermPtr(Id),T);
End;

{ return a new 2-argument predicate with identifier Id and arguments T1,T2: 
 Id(T1,T2) (that is, tuple <Id,T1,T2>) }
Function NewFunc2( Id : IdPtr; T1,T2 : TermPtr ) : TermPtr;
Begin
  NewFunc2 := NewTuple3(TermPtr(Id),T1,T2)
End;

{----------------------------------------------------------------------------}
{ methods 1: tuples                                                          }
{----------------------------------------------------------------------------}

{ return true if U is a tuple }
Function IsTuple( T : TermPtr ) : Boolean;
Begin
  IsTuple := (T <> Nil) And (TypeOfTerm(T) = FuncSymbol)
End;

{ return true if U is the empty tuple }
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Begin
  IsEmptyTuple := (TupleHead(U) = Nil) And (TupleQueue(U) = Nil)
End;

{ return the first element of a tuple: <a,b,c> => a; <> => Nil }
Function TupleHead( U : TermPtr ) : TermPtr;
Begin
  TupleHead := Func_GetLeft(FuncPtr(U))
End;

{ replace the head of tuple U with term T }
Procedure SetTupleHeadTerm( U,T : TermPtr );
Begin
  Func_SetLeft(FuncPtr(U),T)
End;

{ return a tuple containing all the elements in the tuple but the first:
 <a,b,c> => <b,c>, <a,b> => <b>, <a> => Nil, <> => error }
Function TupleQueue( U : TermPtr ) : TermPtr;
Begin
  TupleQueue := Func_GetRight(FuncPtr(U))
End;

{ replace the queue of tuple U1 with tuple U2: <a,b,c> + <d,e,f> => <a,d,e,f>; 
 mostly used to add an single element to a tuple: <a> + <b> => <a,b> }
Procedure SetTupleQueue( U1,U2 : TermPtr );
Begin
  Func_SetRight(FuncPtr(U1),U2)
End;

{ replace the queue of tuple U with term T }
Procedure SetTupleQueueTerm( U,T : TermPtr );
Begin
  SetTupleQueue(U,NewTuple1(T))
End;

{ return the number of elements of a tuple }
Function TupleArgCount( U : TermPtr ) : TTupleArgNumber;
Begin
  If IsEmptyTuple(U) Then { <> }
    TupleArgCount := 0
  Else
    TupleArgCount := TupleArgCount(TupleQueue(U)) + 1 { <1,2,..> }
End;

{ return the N-th element of a tuple U }
Function TupleArgN( N : TTupleArgNumber; U : TermPtr ) : TermPtr;
Begin
  If N = 1 Then
    TupleArgN := TupleHead(U)
  Else
    TupleArgN := TupleArgN(N-1,TupleQueue(U))
End;

{ return the first element of a tuple U, setting U to the tuple queue, which 
 will be the empty tuple if there are no more elements) }
Function TupleArg( Var U : TermPtr ) : TermPtr;
Begin
  TupleArg := TupleHead(U);
  U := TupleQueue(U)
End;

{----------------------------------------------------------------------------}
{ methods 2: tuples (reduced system)                                         }
{----------------------------------------------------------------------------}

{ tuple? }
Function GetIsTuple( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : Boolean;
Begin
  T := RepresentativeOf(T,Reduce,g);
  GetIsTuple := IsTuple(T)
End;

{ empty tuple? }
Function GetIsEmptyTuple( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : Boolean;
Begin
  T := RepresentativeOf(T,Reduce,g);
  GetIsEmptyTuple := IsEmptyTuple(T)
End;

{ tuple head }
Function GetTupleHead( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  T := TupleHead(T);
  T := RepresentativeOf(T,Reduce,g);
  GetTupleHead := T
End;

{ tuple queue }
Function GetTupleQueue( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  T := TupleQueue(T);
  T := RepresentativeOf(T,Reduce,g);
  GetTupleQueue := T
End;

{ tuple length }
Function GetTupleArgCount( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TTupleArgNumber;
Begin
  If GetIsEmptyTuple(T,Reduce,g) Then
    GetTupleArgCount := 0
  Else
    GetTupleArgCount := GetTupleArgCount(GetTupleQueue(T,Reduce,g),Reduce,g) + 1
End;

{ tuple's n-th arg }
Function GetTupleArgN( N : TTupleArgNumber; T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  If N = 1 Then
    GetTupleArgN := GetTupleHead(T,Reduce,g)
  Else
    GetTupleArgN := GetTupleArgN(N-1,GetTupleQueue(T,Reduce,g),Reduce,g)
End;

{ tuple first arg, advancing U to the queue }
Function GetTupleArg( Var U : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Begin
  GetTupleArg := GetTupleHead(U,Reduce,g);
  U := GetTupleQueue(U,Reduce,g)
End;

{----------------------------------------------------------------------------}
{ methods 3: tuples (with loop detection)                                    }
{----------------------------------------------------------------------------}

{ tuple head }
Function ProtectedGetIsTuple( U : TermPtr; Reduce : Boolean ) : Boolean;
Begin
  ProtectedGetIsTuple := GetIsTuple(U,Reduce,NewSerial)
End;

{ tuple head }
Function ProtectedGetTupleHead( U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleHead := GetTupleHead(U,Reduce,NewSerial)
End;

{ tuple queue }
Function ProtectedGetTupleQueue( U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleQueue := GetTupleQueue(U,Reduce,NewSerial)
End;

{ tuple length }
Function ProtectedGetTupleArgCount( U : TermPtr; 
    Reduce : Boolean ) : TTupleArgNumber;
Begin
  ProtectedGetTupleArgCount := GetTupleArgCount(U,Reduce,NewSerial)
End;

{ tuple's n-th arg }
Function ProtectedGetTupleArgN( N : TTupleArgNumber; U : TermPtr; 
    Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleArgN := GetTupleArgN(N,U,Reduce,NewSerial)
End;

{ tuple first arg, advancing U to the queue }
Function ProtectedGetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedGetTupleArg := GetTupleArg(U,Reduce,NewSerial)
End;

{ return the access identifier and the arity of a tuple, using the reduced 
 system; 
 aaa(bbb) = <aaa,bbb> => (aaa,1)
 <1,2> => (Nil,2)
 <aaa> => (aaa,0) }
Procedure ProtectedGetTupleMetaData( T : TermPtr; 
    Var I : IdPtr; Var A : TArity );
Begin
  I := Nil;
  T := ProtectedRepOf(T);
  A := ProtectedGetTupleArgCount(T,True);
  If A > 0 Then
  Begin
    T := ProtectedGetTupleHead(T,True);
    If IsIdentifier(T) Then
    Begin
      I := IdPtr(T);
      A := A - 1
    End
  End
End;

End.