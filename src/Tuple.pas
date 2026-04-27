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
  PObjTerm,
  PObjFCVI;

Type
  TTupleArgNumber = PosInt; { tuple argument index or count }

Function NewEmptyTuple : TermPtr;
Function NewTuple1( T : TermPtr ) : TermPtr;
Function NewTuple( T,U : TermPtr ) : TermPtr;

Function IsTuple( T : TermPtr ) : Boolean;
Function IsEmptyTuple( U : TermPtr ) : Boolean;
Function TupleHead( U : TermPtr ) : TermPtr;
Procedure SetTupleQueue( U1,U2 : TermPtr );
Procedure SetTupleHeadTerm( U,T : TermPtr );
Function TupleQueue( U : TermPtr ) : TermPtr;
Procedure SetTupleQueueTerm( U,T : TermPtr );
Function TupleArgCount( U : TermPtr ) : TTupleArgNumber;
Function TupleArgN( N : TTupleArgNumber; U : TermPtr ) : TermPtr;


Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ constructors                                                               }
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

{ create a new tuple from a head term T and a queue tuple U }
Function NewTuple( T,U : TermPtr ) : TermPtr;
Begin
  NewTuple := Func_NewAsTerm(T,U)
End;

{----------------------------------------------------------------------------}
{ methods                                                                    }
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

End.