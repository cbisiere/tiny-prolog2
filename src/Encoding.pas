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
{               argN    Nil                                             }
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

Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    special,glob : Boolean ) : TermPtr;

Function NewEmptyList( P : ProgPtr ) : TermPtr;
Function NewList2( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Function IsNil( T : TermPtr ) : Boolean;
Function IdentifierToString( P : ProgPtr; I : IdPtr ) : TermPtr;
Function StringToIdentifier( P : ProgPtr; C : ConstPtr ) : TermPtr;
Function CommaExpToList( P : ProgPtr; T : TermPtr ) : TermPtr;
Function TupleToList( P : ProgPtr; U : TermPtr ) : TermPtr;
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
Function ProtectedIsListOfKnownSize( T : TermPtr; Reduce : Boolean ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ functions                                                                  }
{----------------------------------------------------------------------------}

{ return a new 1 or 2-argument predicate with identifier given as a Pascal 
 string made of 1-byte TChar: "ident: "ident(T1)" or "ident(T1,T2)" }
Function NewFunc2( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    special,glob : Boolean ) : TermPtr;
Var
  U,T : TermPtr;
Begin
  CheckCondition(T1 <> Nil,'NewFunc2: first argument is Nil');
  If special Then
    T := EmitSpecialIdent(P,ident,glob)
  Else
    T := EmitShortIdent(P,ident,glob);
  U := NewTuple(T);
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
  NewList2 := NewFunc2(P,'.',T1,T2,True,True)
End;

{ return True if term T is 'nil' }
Function IsNil( T : TermPtr ) : Boolean;
Begin
  IsNil := TermIsIdentifierEqualToShortString(T,'nil')
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
    CommaExpToList := NewList2(P,T,Nil)
End;

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

{ create a list of chars 'a'.'b'.'c'.nil from a Str 'abc' }
Function StrToList( P : ProgPtr; s : StrPtr ) : TermPtr;
Var
  sc : StrPtr;
  L,T : TermPtr;
  Iter : StrIter;
  cc : TChar;
Begin
  L := NewEmptyList(P);
  StrIter_ToEnd(Iter,s);
  While StrIter_PrevChar(Iter,cc) Do
  Begin
    sc := Str_New(Str_GetEncodingContext(s));
    Str_AppendChar(sc,cc);
    T := EmitIdent(P,sc,True,True);
    CheckCondition(T <> Nil,'StrToList: unable to create an identifier');
    L := NewList2(P,T,L)
  End;
  StrToList := L
End;

{ create a Str concatenating each character in a list; return Nil if 
 the term is not a list of characters }
Function ListToStr( P : ProgPtr; T : TermPtr ) : StrPtr;
Var
  s : StrPtr;
  Th,Tq : TermPtr;
Begin
  ListToStr := Nil;
  s := Str_New(UNDECIDED);
  While Not IsNil(T) Do
  Begin
    If Not ProtectedGetList(T,Th,Tq,True) Then
      Exit;
    If TypeOfTerm(Th) <> Identifier Then
      Exit;
    Str_Concat(s,GetIdentAsStr(IdPtr(Th),False));
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
    ListToTuple := Nil
  Else
  Begin
    T := NewTuple(GetListHead(L,Reduce,g));
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

{ return True if term T is 'nil' }
Function ProtectedIsNil( T : TermPtr; Reduce : Boolean ) : Boolean;
Begin
  T := RepresentativeOf(T,Reduce,NewSerial);
  ProtectedIsNil := IsNil(T)
End;

{ return True if term T is a list of known size }
Function ProtectedIsListOfKnownSize( T : TermPtr; Reduce : Boolean ) : Boolean;
Var 
  T1,T2 : TermPtr;
Begin
  If ProtectedIsNil(T,Reduce) Then
    ProtectedIsListOfKnownSize := True
  Else
    If Not ProtectedGetList(T,T1,T2,Reduce) Then
      ProtectedIsListOfKnownSize := False
    Else
      ProtectedIsListOfKnownSize := ProtectedIsListOfKnownSize(T2,Reduce)
End;

End.
