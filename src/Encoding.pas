{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Encoding.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{            E N C O D I N G   O F   C O M P O U N D   T E R M S             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

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
{               argN   F  = <>                                          }
{                     /  \                                              }
{                  Nil    Nil                                           }
{                                                                       }
{-----------------------------------------------------------------------}

Unit Encoding;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  Chars,
  Memory,
  PObj,
  PObjStr,
  PObjTerm,
  PObjBter,
  PObjFCVI,
  PObjDef,
  PObjProg,
  Tuple;

Type
  TListArgNumber = PosInt; { list element index or count }

Function NewFunc1( P : ProgPtr; ident : TString; T : TermPtr; 
    special,glob : Boolean ) : TermPtr;
Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    special,glob : Boolean ) : TermPtr;

Function NilTerm( P : ProgPtr ) : TermPtr;
Function IsNil( T : TermPtr ) : Boolean;

Function NewEmptyList( P : ProgPtr ) : TermPtr;
Function NewList1( P : ProgPtr; T : TermPtr ) : TermPtr;
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Function ListArgN( N : TListArgNumber; T : TermPtr ) : TermPtr;
Function ReverseList( P : ProgPtr; T : TermPtr ) : TermPtr;

Function IdentifierToString( P : ProgPtr; I : IdPtr ) : TermPtr;
Function StringToIdentifier( P : ProgPtr; C : ConstPtr ) : TermPtr;
Function CommaExpToList( P : ProgPtr; T : TermPtr ) : TermPtr;
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
Function StrToList( P : ProgPtr; s : StrPtr ) : TermPtr;
Function ListToStr( P : ProgPtr; T : TermPtr ) : StrPtr;
Function IdentifierToList( P : ProgPtr; I : IdPtr ) : TermPtr;
Function ListToIdentifier( P : ProgPtr; T : TermPtr ) : TermPtr;
Function NumToList( P : ProgPtr; C : ConstPtr ) : TermPtr;
Function ListToNum( P : ProgPtr; T : TermPtr ) : ConstPtr;
Function BTermsToList( P : ProgPtr; B : BTermPtr ) : TermPtr;
Function ListToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;
Function CommaExpToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;
Function RuleExpToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;

Function ProtectedGetTupleHead( U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleQueue( U : TermPtr; Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleArgCount( U : TermPtr; 
    Reduce : Boolean ) : TTupleArgNumber;
Function ProtectedGetTupleArgN( N : TTupleArgNumber; U : TermPtr; 
    Reduce : Boolean ) : TermPtr;
Function ProtectedGetTupleArg( Var U : TermPtr; Reduce : Boolean ) : TermPtr;

Function ProtectedListToTuple( L : TermPtr; Reduce : Boolean ) : TermPtr;

Function ProtectedGetFunc1( T : TermPtr; ident : TString; Var T1 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Function ProtectedGetFunc2( T : TermPtr; ident : TString; 
    Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;

Function ProtectedGetList( T : TermPtr; Var T1,T2 : TermPtr; 
    Reduce : Boolean ) : Boolean;
Function ProtectedIsList( T : TermPtr; Reduce : Boolean ) : Boolean;
Function ProtectedIsNil( T : TermPtr; Reduce : Boolean ) : Boolean;
Function ProtectedIsListOfKnownSize( T : TermPtr; Reduce : Boolean; 
    Var n : TListArgNumber ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ functions                                                                  }
{----------------------------------------------------------------------------}

{ return a new 1-argument predicate with identifier given as a Pascal 
 string made of 1-byte TChar: ident(T1) (that is, tuple <ident,T1>);
 this function is used to create evaluable unary expressions }
Function NewFunc1( P : ProgPtr; ident : TString; T : TermPtr; 
    special,glob : Boolean ) : TermPtr;
Var
  T1 : TermPtr;
Begin
  CheckCondition(T <> Nil,'NewFunc1: first argument is Nil');
  If special Then
    T1 := EmitSpecialIdent(P,ident,glob)
  Else
    T1 := EmitShortIdent(P,ident,glob);
  NewFunc1 := NewTuple(T1,NewTuple1(T));
End;

{ return 2-argument predicate with identifier given as a Pascal 
 string made of 1-byte TChar: ident(T1,T2) (that is, tuple <ident,T1,T2>);
 this function is used to create evaluable binary expressions and lists }
Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    special,glob : Boolean ) : TermPtr;
Var
  T,Q : TermPtr;
Begin
  T := NewFunc1(P,ident,T1,special,glob); { <ident,T1> }
  { append argument T2 }
  Q := TupleQueue(T); { <T1> }
  SetTupleQueue(Q,NewTuple1(T2)); { <ident,T1,T2> }
  NewFunc2 := T
End;


{----------------------------------------------------------------------------}
{ lists                                                                      }
{----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{                                                                       }
{   a.b.nil <=> '.'(a,'.'(b,'nil')) <=> <'.', a, <'.',b,'nil'>>         }
{                                                                       }
{           F                                                           }
{          / \                                                          }
{       '.'    F                                                        }
{            /  \                                                       }
{           a    F                                                      }
{              /   \                                                    }
{             F     F                                                   }
{          /  \    /  \                                                 }
{        '.'    F  Nil Nil                                              }
{             /  \                                                      }
{            b     F                                                    }
{                /   \                                                  }
{             'nil'   F                                                 }
{                    /  \                                               }
{                  Nil  Nil                                             }
{                                                                       }
{-----------------------------------------------------------------------}


{ return Nil }
Function NilTerm( P : ProgPtr ) : TermPtr;
Begin
  NilTerm := EmitShortIdent(P,'nil',True)
End;

{ return True if term T is 'nil' }
Function IsNil( T : TermPtr ) : Boolean;
Begin
  IsNil := TermIsIdentifierEqualToShortString(T,'nil')
End;

{ return the head of a list: a.b.nil => a }
Function ListHead( L : TermPtr ) : TermPtr;
Begin
  ListHead := TupleArgN(2,L)
End;

{ return the queue of a list: 
 a.b.nil => b.nil = <b,'nil'>
 a.nil => nil = 'nil' }
Function ListQueue( L : TermPtr ) : TermPtr;
Begin
  ListQueue := TupleArgN(3,L)
End;

{ return an empty list: 'nil' }
Function NewEmptyList( P : ProgPtr ) : TermPtr;
Begin
  NewEmptyList := NilTerm(P)
End;

{ return a list "a.nil", viewed as '.'(a,'nil') (equivalent to <'.',a,'nil'>) }
Function NewList1( P : ProgPtr; T : TermPtr ) : TermPtr;
Begin
  NewList1 := NewList2(P,T,NewEmptyList(P))
End;

{ return a list "a.b", viewed as '.'(a,b)" (equivalent to <'.',a,b>), where b
 can be 'nil' or another list }
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Begin
  NewList2 := NewFunc2(P,'.',T1,T2,True,True)
End;

{ return the N-th element of a list T, assumed to be at least of size N }
Function ListArgN( N : TListArgNumber; T : TermPtr ) : TermPtr;
Begin
  If N = 1 Then
    ListArgN := ListHead(T)
  Else
    ListArgN := ListArgN(N-1,ListQueue(T))
End;

{ return a nil-terminated list, reversed }
Function ReverseList( P : ProgPtr; T : TermPtr ) : TermPtr;
Var
  L : TermPtr;
Begin
  L := NewEmptyList(P);
  While Not IsNil(T) Do
  Begin
    L := NewList2(P,ListHead(T),L);
    T := ListQueue(T)
  End;
  ReverseList := L
End;


{----------------------------------------------------------------------------}
{ conversions                                                                }
{----------------------------------------------------------------------------}

{ create a string constant "abc" (as a term) from an identifier 'abc' }
Function IdentifierToString( P : ProgPtr; I : IdPtr ) : TermPtr;
Begin
  IdentifierToString := EmitConst(P,IdentifierGetStr(I),CS,False)
End;

{ create an identifier (as a term) from a string constant; the string is first 
 unquoted, as only simplified ident syntax is allowed: "abc" and "'abc'" are 
 allowed but "123" and "'123'" are not. return Nil if the unquoted string is 
 not a valid, simplified syntax identifier }
Function StringToIdentifier( P : ProgPtr; C : ConstPtr ) : TermPtr;
Begin
  StringToIdentifier := EmitIdentFromString(P,C,False)
End;

{ create a new list a.b.c.nil from a comma-expression ','(a,','(b,c)) }
Function CommaExpToList( P : ProgPtr; T : TermPtr ) : TermPtr;
Var
  L : TermPtr;
  T1,T2 : TermPtr;
Begin
  If ProtectedGetFunc2(T,',',T1,T2,True) Then
  Begin
    L := CommaExpToList(P,T2);
    CommaExpToList := NewList2(P,T1,L)
  End
  Else
    CommaExpToList := NewList1(P,T)
End;

{ create a new list foo.a.b.nil from a tuple <foo,a,b> }
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
Var
  L : TermPtr;
Begin
  If IsEmptyTuple(U) Then
    TupleToList := NewEmptyList(P)
  Else
  Begin
    L := TupleToList(P,TupleQueue(U));
    TupleToList := NewList2(P,TupleHead(U),L)
  End
End;

{ create a list of chars 'a'.'b'.'c'.nil from a Str 'abc' }
Function StrToList( P : ProgPtr; s : StrPtr ) : TermPtr;
Var
  L,T : TermPtr;
  Iter : StrIter;
  Enc : TEncoding;
  cc : TChar;
Begin
  L := NewEmptyList(P);
  Enc := Str_GetEncodingContext(s);
  StrIter_ToEnd(Iter,s);
  While StrIter_PrevChar(Iter,cc) Do
  Begin
    T := EmitChar(P,Enc,cc);
    CheckCondition(T <> Nil,'StrToList: unable to create an identifier');
    L := NewList2(P,T,L)
  End;
  StrToList := L
End;

{ create a Str concatenating each individual character in a list; return Nil if 
 the term is not a list of characters }
Function ListToStr( P : ProgPtr; T : TermPtr ) : StrPtr;
Var
  s,sc : StrPtr;
  Th,Tq : TermPtr;
  y : TSyntax;
Begin
  ListToStr := Nil;
  y := GetSyntax(P);
  s := Str_New(ENC_UNDECIDED);
  While Not IsNil(T) Do
  Begin
    If Not ProtectedGetList(T,Th,Tq,True) Then { not a list }
      Exit;
    Case TypeOfTerm(Th) Of
    Identifier:
      Begin
        If y <> Edinburgh Then { 'a' form allowed in Edinburgh }
          Exit;
        sc := GetIdentAsStr(IdPtr(Th),False)
      End;
    Constant:
      Begin
        If ConstType(ConstPtr(Th)) <> QString Then { not a constant string }
          Exit;
        sc := ConstGetStr(ConstPtr(Th)) 
      End;
    Else
      Exit { wrong type }
    End;
    If Str_Length(sc) <> 1 Then { not a one-char string }
      Exit;
    { ok, append the new individual character to the result string }
    Str_Concat(s,sc);
    { now analyze the queue }
    T := Tq
  End;
  ListToStr := s
End;

{ create a list of chars 'a'.'b'.'c'.nil from an identifier 'abc' }
Function IdentifierToList( P : ProgPtr; I : IdPtr ) : TermPtr;
Begin
  IdentifierToList := StrToList(P,IdentifierGetStr(I))
End;

{ create an identifier concatenating each character in a list; return Nil if 
 the term is not a list of characters or if no valid identifier can be created
 from the list of characters }
Function ListToIdentifier( P : ProgPtr; T : TermPtr ) : TermPtr;
Var
  s : StrPtr;
Begin
  ListToIdentifier := Nil;
  s := ListToStr(P,T);
  If s <> Nil Then
    ListToIdentifier := EmitIdent(P,s,True,False)
End;

{ create a list of chars '1'.'2'.'3'.nil from a numerical constant 123; no
 conversion is necessary since a canonical string representation is available }
Function NumToList( P : ProgPtr; C : ConstPtr ) : TermPtr;
Begin
  NumToList := StrToList(P,ConstGetStr(C))
End;

{ create a numerical constant from a list of characters; return Nil if 
 the term is not a list of characters or does not contain a valid numerical
 value }
Function ListToNum( P : ProgPtr; T : TermPtr ) : ConstPtr;
Var
  s : StrPtr;
Begin
  ListToNum := Nil;
  s := ListToStr(P,T);
  If s = Nil Then
    Exit;
  If NormalizeConstant(s,IntegerNumber) Then
    ListToNum := ConstPtr(EmitConst(P,s,CI,False))
  Else If NormalizeConstant(s,RealNumber) Then
    ListToNum := ConstPtr(EmitConst(P,s,CR,False))
End;

{ create a new list from a list of BTerms }
Function BTermsToList( P : ProgPtr; B : BTermPtr ) : TermPtr;
Var
  L : TermPtr;
Begin
  If B = Nil Then
    BTermsToList := NewEmptyList(P)
  Else
  Begin
    L := BTermsToList(P,BTerms_GetNext(B));
    BTermsToList := NewList2(P,BTerm_GetTerm(B),L)
  End
End;

{ create a new list of BTerms from a list }
Function ListToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;
Var
  Bh,Bq : BTermPtr;
Begin
  If IsNil(T) Then
    ListToBTerms := Nil
  Else
  Begin
    Bq := ListToBTerms(P,ListQueue(T));
    Bh := BTerm_New(ListHead(T));
    BTerms_SetNext(Bh,Bq);
    ListToBTerms := Bh
  End
End;

{ create a new list of BTerms from a term or a comma-expression:
 ','(a,','(b,c)) => a,b,c
 a => a }
Function CommaExpToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;
Var
  T1,T2 : TermPtr;
  Bh,Bq : BTermPtr;
Begin
  If Not ProtectedGetFunc2(T,',',T1,T2,True) Then
    CommaExpToBTerms := BTerm_New(T)
  Else
  Begin
    Bq := CommaExpToBTerms(P,T2);
    Bh := BTerm_New(T1);
    BTerms_SetNext(Bh,Bq);
    CommaExpToBTerms := Bh
  End
End;

{ create a new list of BTerms from a rule-expression:
 ':-'(h,','(a,','(b,c))) => h,a,b,c 
 h => h }
Function RuleExpToBTerms( P : ProgPtr; T : TermPtr ) : BTermPtr;
Var
  T1,T2 : TermPtr;
  Bh,Bq : BTermPtr;
Begin
  If Not ProtectedGetFunc2(T,':-',T1,T2,True) Then
    RuleExpToBTerms := BTerm_New(T)
  Else
  Begin
    Bq := CommaExpToBTerms(P,T2);
    Bh := BTerm_New(T1);
    BTerms_SetNext(Bh,Bq);
    RuleExpToBTerms := Bh
  End
End;

{----------------------------------------------------------------------------}
{ navigate the term tree, possibly through the reduced system                }
{----------------------------------------------------------------------------}

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

{ list head }
Function GetListHead( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Var
  Th : TermPtr;
Begin
  Th := GetTupleArg(T,Reduce,g);
  GetListHead := GetTupleHead(T,Reduce,g)
End;

{ list queue }
Function GetListQueue( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : TermPtr;
Var
  Th : TermPtr;
Begin
  Th := GetTupleArg(T,Reduce,g);
  Th := GetTupleArg(T,Reduce,g);
  GetListQueue := GetTupleHead(T,Reduce,g)
End;

{ return True if term T is 'nil' }
Function GetIsNil( T : TermPtr; 
    Reduce : Boolean; g : TSerial ) : Boolean;
Begin
  T := RepresentativeOf(T,Reduce,g);
  GetIsNil := IsNil(T)
End;

{ create a new tuple <foo,a,b>  from a list foo.a.b.nil, that is [foo,a,b];
 note also that foo.nil => <foo>, nil => <> }
Function ListToTuple( L : TermPtr; 
    Reduce : Boolean; g : TSerial  ) : TermPtr;
Var
  T : TermPtr;
Begin
  If GetIsNil(L,Reduce,g) Then
    ListToTuple := NewEmptyTuple
  Else
  Begin
    T := NewTuple1(GetListHead(L,Reduce,g));
    SetTupleQueue(T,ListToTuple(GetListQueue(L,Reduce,g),Reduce,g));
    ListToTuple := T
  End
End;


{----------------------------------------------------------------------------}
{ loop-protected functions, which use their own serial number                }
{----------------------------------------------------------------------------}

{ tuple head }
Function ProtectedListToTuple( L : TermPtr; Reduce : Boolean ) : TermPtr;
Begin
  ProtectedListToTuple := ListToTuple(L,Reduce,NewSerial)
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
  If Not IsTuple(T) Or IsEmptyTuple(T) Then
    Exit;
  g := NewSerial;
  T0 := GetTupleArg(T,Reduce,g);
  If IsEmptyTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualToShortString(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce,g);
  If Not IsEmptyTuple(T) Then
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
  If Not IsTuple(T) Or IsEmptyTuple(T) Then
    Exit;
  g := NewSerial;
  T0 := GetTupleArg(T,Reduce,g);
  If IsEmptyTuple(T) Then
    Exit;
  If Not TermIsIdentifierEqualToShortString(T0,ident) Then
    Exit;
  T1 := GetTupleArg(T,Reduce,g);
  If IsEmptyTuple(T) Then
    Exit;
  T2 := GetTupleArg(T,Reduce,g);
  If Not IsEmptyTuple(T) Then
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

{ return True if term T is 'nil' }
Function ProtectedIsNil( T : TermPtr; Reduce : Boolean ) : Boolean;
Begin
  T := RepresentativeOf(T,Reduce,NewSerial);
  ProtectedIsNil := IsNil(T)
End;

{ return True if term T is a list of known size }
Function ProtectedIsListOfKnownSize( T : TermPtr; Reduce : Boolean; 
    Var n : TListArgNumber ) : Boolean;
Var 
  T1,T2 : TermPtr;
  m : TListArgNumber;
Begin
  If ProtectedIsNil(T,Reduce) Then
  Begin
    ProtectedIsListOfKnownSize := True;
    n := 0
  End
  Else
    If Not ProtectedGetList(T,T1,T2,Reduce) Then
      ProtectedIsListOfKnownSize := False
    Else
    Begin
      ProtectedIsListOfKnownSize := ProtectedIsListOfKnownSize(T2,Reduce,m);
      n := m + 1
    End
End;

End.
