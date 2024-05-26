{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Tuple.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   E N C O D I N G   O F   T U P L E S                      }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }


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
{                argN    Nil                                            }
{                                                                       }
{-----------------------------------------------------------------------}

Unit Tuple;

Interface

Uses
  PObjTerm,
  PObjFCVI;

Function NewTuple( T : TermPtr ) : TermPtr;
Function NewTuple2( T1,T2 : TermPtr ) : TermPtr;
Function NewEmptyTuple : TermPtr;

Function IsTuple( T : TermPtr ) : Boolean;
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Function TupleHead( U : TermPtr ) : TermPtr;
Function TupleQueue( U : TermPtr ) : TermPtr;
Procedure SetTupleQueueTerm( U,T : TermPtr );
Function TupleArgCount( U : TermPtr ) : Integer;
Function TupleArgN( N : Integer; U : TermPtr ) : TermPtr;


Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ constructors                                                               }
{----------------------------------------------------------------------------}

{ create a new tuple containing a single term T: <a> }
Function NewTuple( T : TermPtr ) : TermPtr;
Begin
  NewTuple := Func_NewAsTerm(T,Nil)
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

{ create the empty tuple: <> }
Function NewEmptyTuple : TermPtr;
Begin
  NewEmptyTuple := NewTuple(Nil)
End;

{----------------------------------------------------------------------------}
{ methods                                                                    }
{----------------------------------------------------------------------------}

{ return true if U is a tuple, possibly through its representative }
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
  SetTupleQueue(U,NewTuple(T))
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

End.