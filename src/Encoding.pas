{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Encoding.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                    E N C O D I N G   O F   T E R M S                       }
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
  U := NewTuple(EmitIdent(P,ident,glob));
  SetTupleQueueTerm(U,T1);
  If T2 <> Nil Then
    SetTupleQueueTerm(TupleQueue(U),T2);
  NewFunc2 := U
End;

{ return a term "a.b", viewed as '.'(a,b)" and thus implemented as 
 "F('.',F(a,F(b,Nil)))"; if b is Nil, replace it with "F('nil',Nil)", that is,
 add ".nil" at the end of the dotted list; this helps implementing the 
 following equivalences:
  [a|b] <=> a.b
  [a,b] <=> a.b.nil
 (see PII+ syntax (cf. pdf doc p.45) }
Function EncodeDot( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Begin
  EncodeDot := Nil;
  If T2 = Nil Then
    T2 := EmitIdent(P,'nil',True);
  EncodeDot := NewFunc2(P,'.',T1,T2,True)
End;

