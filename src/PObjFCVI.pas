{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjFCVI.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         P R O L O G   T E R M S :                          }
{                                                                            }
{                  F U N C,  C O N S T,  V A R,  I D E N T                   }
{                                                                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjFCVI;

Interface

Uses
  Common,
  ShortStr,
  Num,
  Errs,
  Trace,
  Memory,
  PObj,
  PObjList,
  PObjTerm,
  PObjRest,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjSys;

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

Type
  TTerm = (Variable,Identifier,Constant,FuncSymbol,Dummy); { type of term }

{ constant: identifier, number or quoted string; list of constants  }
Type
  TConst = (IntegerNumber,RealNumber,QString);

  ConstPtr = ^TObjConst; { term: constant }
  TObjConst = Record
    PO_META : TObjMeta;
    TT_META : TermMetaPtr;
    { not deep copied: }
    TC_DCON : DictPtr;
  End;

{ binary functional symbol }
Type 
  FuncPtr = ^TObjFunc;
  TObjFunc = Record
    PO_META : TObjMeta;
    TT_META : TermMetaPtr;
    { deep copied: }
    TF_TRED : TermPtr; { right member of the equation in the reduced system }
    TF_LTER : TermPtr; { left term }
    TF_RTER : TermPtr
End;

{ array; not a term in itself; must contain only pointers, as the size of the
 object is set a creation time }
Type
  TArrayIndex = 1..MaxPosInt;
  TArraySize = PosInt;
  ArrayPtr = ^TObjArray;
  TObjArray = Record
    PO_META : TObjMeta;
    { deep copied: }
    AR_DATA : Array[TArrayIndex] Of TermPtr
  End;

Type 
  TermsPtr = ListPtr;

{ identifier }
Type
  IdPtr = ^TObjId;
  TObjId = Record
    PO_META : TObjMeta;
    TT_META : TermMetaPtr;
    { deep copied: }
    TI_ARRA : ArrayPtr; { array elements or Nil if not an array }
    TI_VALU : TermPtr; { term assigned to the identifier }
    { not deep copied: }
    TI_DVAR : DictPtr; { dictionary entry }
    { extra data: }
    TI_SIZE : TArraySize; { number or array elements or zero if not an array }
    TI_QUOT : Boolean; { True if the identifier must be quoted to be valid }
    TI_ASSI : Boolean { true if the term is an identifier that has been assigned }
  End;

{ variable }
Type
  VarPtr = ^TObjVar;
  TObjVar = Record
    PO_META : TObjMeta;
    TT_META : TermMetaPtr;
    { deep copied: }
    TV_TRED : TermPtr; { right member of the equation in the reduced system }
    TV_FWAT : EqPtr; { first inequation this variable watches }
    TV_GOAL : TermsPtr; { frozen terms this variable control, or nil }
    { not deep copied: }
    TV_DVAR : DictPtr; { dictionary entry }
    TV_IRED : IdPtr; { identifier this variable as been initially bound to }
    { extra data: }
    TV_ANON : Boolean { True if the object is an anonymous variable }
  End;


Function Array_New( n : TArraySize ) : ArrayPtr;
Function Array_GetElement( A : ArrayPtr; i : TArrayIndex ) : TermPtr;

Function Func_NewAsTerm( T1,T2 : TermPtr ) : TermPtr;
Function TypeOfTerm( T : TermPtr ) : TTerm;
Function TypeOfTermAsShortString( T : TermPtr ) : TString;
Function ObjectTypeToConstType( typ : TypePrologObj ) : TConst;
Function ConstType( C : ConstPtr ) : TConst;
Function ConstGetStr( C : ConstPtr ) : StrPtr;
Function ConstGetShortString( C : ConstPtr ) : TString;
Function GetConstAsStr( C : ConstPtr; Quotes : Boolean ) : StrPtr;

Function VariableGetName( V : VarPtr ) : StrPtr;
Function GetFrozenTerms( V : VarPtr ) : TermsPtr;
Procedure SetFrozenTerms( V : VarPtr; M : TermsPtr );
Procedure SetFrozenTermsWithUndo( V : VarPtr; M : TermsPtr; Undo : Boolean; 
    Var L : RestPtr );
Procedure AddFrozenTermsWithUndo( V : VarPtr; M : TermsPtr; Undo : Boolean; 
    Var L : RestPtr );

Function IdentifierGetStr( I : IdPtr ) : StrPtr;
Function IdentifierGetShortString( I : IdPtr ) : TString;
Function IdentifierEqualToShortString( I : IdPtr; ps : TString ) : Boolean;
Function TermIsIdentifierEqualToShortString( T : TermPtr; ident : TString ) : Boolean;
Function IdentifierIsCut( I : IdPtr ) : Boolean;
Function TermIsCut( T : TermPtr ) : Boolean;
Function GetIdentAsStr( I : IdPtr; Quotes : Boolean ) : StrPtr;

Function Func_GetLeft( F : FuncPtr ) : TermPtr;
Function Func_GetRight( F : FuncPtr ) : TermPtr;
Function Func_GetLeftAddr( F : FuncPtr ) : TermPtrAddr;
Function Func_GetRightAddr( F : FuncPtr ) : TermPtrAddr;
Procedure Func_SetLeft( F : FuncPtr; T : TermPtr );
Procedure Func_SetRight( F : FuncPtr; T : TermPtr );
Procedure Func_SetLeftWithUndo( F : FuncPtr; T : TermPtr; 
    Undo : Boolean; Var L : RestPtr );
Procedure Func_SetRightWithUndo( F : FuncPtr; T : TermPtr; 
    Undo : Boolean; Var L : RestPtr );

Procedure UnbindVar( V : VarPtr );
Function WatchIneq( V : VarPtr ) : EqPtr;
Function IsWatching( V : VarPtr; E : EqPtr ) : Boolean;
Procedure AddWatchWithUndo( V : VarPtr; E : EqPtr; 
    Undo : Boolean; Var L : RestPtr );
Function VRed( V : VarPtr ) : TermPtr;
Function FRed( F : FuncPtr ) : TermPtr;
Function Red( T : TermPtr ) : TermPtr;
Function RepresentativeOf( T : TermPtr; Reduce : Boolean; 
    g : TSerial ) : TermPtr;
Function ProtectedRepOf( T : TermPtr ) : TermPtr;

Function IsBound( T : TermPtr ) : Boolean;
Function IsFree( T : TermPtr ) : Boolean;
Function CopyTerm( T : TermPtr; Assigned : Boolean ) : TermPtr;

Function EvaluateToInteger( T : TermPtr ) : ConstPtr;
Function EvaluateToReal( T : TermPtr ) : ConstPtr;
Function EvaluateToString( T : TermPtr ) : ConstPtr;
Function EvaluateToIdentifier( T : TermPtr ) : IdPtr;

Function AccessIdentifier( T : TermPtr ) : IdPtr;
Function ArgCount( T : TermPtr ) : PosInt;
Function Arity( T : TermPtr ) : PosInt;

Function GetValue( I : IdPtr ) : TermPtr;
Procedure SetValue( I : IdPtr; T : TermPtr );
Function GetArray( I : IdPtr ) : ArrayPtr;
Procedure SetArray( I : IdPtr; n : TArraySize; A : ArrayPtr );
Procedure SetArrayElement( I : IdPtr; j : TArrayIndex; T : TermPtr );
Function GetArraySize( I : IdPtr ) : TArraySize;
Function IsArray( I : IdPtr ) : Boolean;

Function IsConstant( T : TermPtr ) : Boolean;
Procedure SetAsAssigned( I : IdPtr );
Function IsIdentifier( T : TermPtr ) : Boolean;
Function IsAtomic( T : TermPtr ) : Boolean;
Function IsAssigned( I : IdPtr ) : Boolean;
Function IsVariable( T : TermPtr ) : Boolean;
Function IsAnonymous( V : VarPtr ) : Boolean;
Function IsAnonymousVariable( T : TermPtr ) : Boolean;
Procedure OrderTerms( Var T1,T2: TermPtr );
Procedure TrackAssignment( T1,T2: TermPtr );
Function NormalizeConstant( Var s : StrPtr; typ : TConst ) : Boolean;

Function InstallConst( Var D : DictPtr; str : StrPtr; 
    ty : TypePrologObj; glob : Boolean ) : ConstPtr;
Function InstallVariable( Var D : DictPtr; str : StrPtr; 
    anonymous : Boolean; glob : Boolean ) : VarPtr;
Function InstallIdentifier( Var D : DictPtr; str : StrPtr; Quoted : Boolean;
    glob : Boolean  ) : IdPtr;


Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ array                                                                 }
{-----------------------------------------------------------------------}

{ create a new array object for n integer constants, initialized to Nil for 
 now; each element must then be initialized to constant zero }
Function Array_New( n : TArraySize ) : ArrayPtr;
Var 
  A : ArrayPtr;
  Size : TArraySize;
  i : TArrayIndex;
Begin
  Size := SizeOf(TObjMeta) + n*SizeOf(TermPtr); { CHECK: alignment issue? }
  A := ArrayPtr(NewRegisteredPObject(AR,Size,n,False,n));
  With A^ Do
  Begin
    For i := 1 To n Do
      AR_DATA[i] := Nil
  End;
  Array_New := A
End;

{ get A[i] }
Function Array_GetElement( A : ArrayPtr; i : TArrayIndex ) : TermPtr;
Begin
  Array_GetElement := A^.AR_DATA[i]
End; 

{ set A[i] }
Procedure Array_SetElement( A : ArrayPtr; i : TArrayIndex; T : TermPtr );
Begin
  A^.AR_DATA[i] := T
End; 

{-----------------------------------------------------------------------}
{ term: constructors                                                    }
{-----------------------------------------------------------------------}

{ create a new constant; constants are never copied during deep copies, in 
 order not to break the invariant "1 constant value = 1 constant object" that 
 allows to test for equality by testing for equality of objects' addresses }
Function Const_New : ConstPtr;
Var 
  C : ConstPtr;
Begin
  C := ConstPtr(NewRegisteredPObject(CO,SizeOf(TObjConst),
      1 + 1,False,1 + 0));
  With C^ Do
  Begin
    TT_META := TermMeta_New;
    TC_DCON := Nil
  End;
  Const_New := C
End;

{ create a (potentially) assignable identifier; an identifier must remain 
 unique, and thus cannot be copied }
Function Ident_New( Quoted : Boolean) : IdPtr;
Var 
  I : IdPtr;
Begin
  I := IdPtr(NewRegisteredPObject(ID,SizeOf(TObjId),1 + 3,False,1 + 2));
  With I^ Do
  Begin
    TT_META := TermMeta_New;
    TI_ARRA := Nil;
    TI_VALU := Nil;
    TI_DVAR := Nil;
    TI_SIZE := 0;
    TI_QUOT := Quoted;
    TI_ASSI := False
  End;
  Ident_New := I
End;

{ create a new variable; a variable can be deep copied }
Function Var_New( anonymous : Boolean) : VarPtr;
Var 
  V : VarPtr;
Begin
  V := VarPtr(NewRegisteredPObject(VA,SizeOf(TObjVar),1 + 5,True,1 + 3));
  With V^ Do
  Begin
    TT_META := TermMeta_New;
    TV_TRED := Nil;
    TV_FWAT := Nil;
    TV_GOAL := Nil;
    TV_DVAR := Nil;
    TV_IRED := Nil;
    TV_ANON := anonymous
  End;
  Var_New := V
End;

{ create a new binary functional symbol }
Function Func_New( T1,T2 : TermPtr ) : FuncPtr;
Var 
  F : FuncPtr;
Begin
  F := FuncPtr(NewRegisteredPObject(FU,SizeOf(TObjFunc),
      1 + 3,True,1 + 3));
  With F^ Do
  Begin
    TT_META := TermMeta_New;
    TF_TRED := Nil;
    TF_LTER := T1;
    TF_RTER := T2
  End;
  Func_New := F
End;

{ return a new "F(a,b)" construct as a term, for convenience }
Function Func_NewAsTerm( T1,T2 : TermPtr ) : TermPtr;
Var
  F : FuncPtr;
  TF : TermPtr Absolute F;
Begin
  F := Func_New(T1,T2);
  Func_NewAsTerm := TF
End;


{-----------------------------------------------------------------------}
{ methods: terms                                                        }
{-----------------------------------------------------------------------}

{ type of term }
Function TypeOfTerm( T : TermPtr ) : TTerm;
Begin
  TypeOfTerm := Dummy; { FIXME: get rid of this }
  If T <> Nil Then
    Case PObjectType(TObjectPtr(T)) Of
    CO:
      TypeOfTerm := Constant;
    ID:
      TypeOfTerm := Identifier;
    VA:
      TypeOfTerm := Variable;
    FU:
      TypeOfTerm := FuncSymbol
    End
End;

{ type of term as a Pascal string }
Function TypeOfTermAsShortString( T : TermPtr ) : TString;
Var
  s : TString;
Begin
  s := 'Unknown';
  Case TypeOfTerm(T) Of
  Constant:
    s := 'Constant';
  Identifier:
    Begin
      s := 'Identifier';
      If IsAssigned(IdPtr(T)) Then
        s := 'Assigned ' + s
    End;
  Variable:
    s := 'Variable';
  FuncSymbol:
    s := 'Symbol'
  End;
  TypeOfTermAsShortString := s
End;

{-----------------------------------------------------------------------}
{ methods: constants                                                    }
{-----------------------------------------------------------------------}

{ object type to constant type }
Function ObjectTypeToConstType( typ : TypePrologObj ) : TConst;
Var 
  t : TConst;
Begin
  Case typ  Of
  CS: t := QString;
  CI: t := IntegerNumber;
  CR: t := RealNumber;
  End;
  ObjectTypeToConstType := t
End;

{ return the type of a constant }
Function ConstType( C : ConstPtr ) : TConst;
Begin
  ConstType := ObjectTypeToConstType(Dict_GetType(C^.TC_DCON))
End;

{ return the string value of a constant; not cloning }
Function ConstGetStr( C : ConstPtr ) : StrPtr;
Begin
  ConstGetStr := Dict_GetStr(C^.TC_DCON)
End;

{ return the Pascal string value of a constant, shortening the
  result if the string object is longer than 255 characters }
Function ConstGetShortString( C : ConstPtr ) : TString;
Var s : StrPtr;
Begin
  s := ConstGetStr(C);
  ConstGetShortString := Str_GetShortStringTruncate(s)
End;

{ is a constant equal to a given Pascal string? }
Function ConstEqualTo( C : ConstPtr; ps : TString ) : Boolean;
Begin
  ConstEqualTo := Dict_StrIsEqualToShortString(C^.TC_DCON,ps)
End;

{ is a constant starting with a char in a given set? not UTF-8 aware }
Function ConstStartWith( C : ConstPtr; E : CharSet ) : Boolean;
Begin
  ConstStartWith := Dict_StrStartsWithShortString(C^.TC_DCON,E)
End;

{ return a constant as a string, with or without double quotes; a double-quoted
 string is escaped; the constant's string is cloned to prevent further damages }
Function GetConstAsStr( C : ConstPtr; Quotes : Boolean ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_Clone(ConstGetStr(C));
  If Quotes And (ConstType(C)=QString) Then
    Str_DoubleQuoteAndEscape(s);
  GetConstAsStr := s
End;

{-----------------------------------------------------------------------}
{ methods: name                                                         }
{-----------------------------------------------------------------------}

{ return the name of a variable; not cloning }
Function VariableGetName( V : VarPtr ) : StrPtr;
Begin
  VariableGetName := Dict_GetStr(V^.TV_DVAR)
End;

{-----------------------------------------------------------------------}
{ methods: freeze                                                       }
{-----------------------------------------------------------------------}

{ get the list of frozen terms }
Function GetFrozenTerms( V : VarPtr ) : TermsPtr;
Begin
  GetFrozenTerms := V^.TV_GOAL
End;

{ set the list of frozen terms (no undo) }
Procedure SetFrozenTerms( V : VarPtr; M : TermsPtr );
Begin
  V^.TV_GOAL := M
End;

{ set the list of frozen terms }
Procedure SetFrozenTermsWithUndo( V : VarPtr; M : TermsPtr; 
    Undo : Boolean; Var L : RestPtr );
Begin
  Rest_SetMem(L,TObjectPtr(V),TObjectPtr(V^.TV_GOAL),TObjectPtr(M),Undo)
End;

{ add a list of frozen terms M to the frozen list of variable V }
Procedure AddFrozenTermsWithUndo( V : VarPtr; M : TermsPtr; 
    Undo : Boolean; Var L : RestPtr );
Var 
  Mf,Ml : TermsPtr; { first and last frozen terms of V }
Begin
  Mf := GetFrozenTerms(V);
  If Mf = Nil Then { no frozen terms yet: M is the new list of frozen terms }
    SetFrozenTermsWithUndo(V,M,Undo,L)
  Else
  Begin { append the frozen list M at the end of the current list }
    Ml := List_Last(Mf);
    Rest_SetMem(L,TObjectPtr(Ml),TObjectPtr(Ml^.LL_NEXT),TObjectPtr(M),Undo)
  End
End;

{-----------------------------------------------------------------------}
{ methods: identifiers                                                  }
{-----------------------------------------------------------------------}

{ return True if the identifier must be quoted to be a valid identifier }
Function IdentifierMustBeQuoted( I : IdPtr ) : Boolean;
Begin
  IdentifierMustBeQuoted := I^.TI_QUOT
End;

{ return the string value of an identifier; not cloning }
Function IdentifierGetStr( I : IdPtr ) : StrPtr;
Begin
  IdentifierGetStr := Dict_GetStr(I^.TI_DVAR)
End;

{ return the Pascal string value of an identifier, shortening the
  result if the string object is longer than 255 characters }
Function IdentifierGetShortString( I : IdPtr ) : TString;
Var s : StrPtr;
Begin
  s := IdentifierGetStr(I);
  IdentifierGetShortString := Str_GetShortStringTruncate(s)
End;

{ is an identifier equal to a given Pascal string? }
Function IdentifierEqualToShortString( I : IdPtr; ps : TString ) : Boolean;
Begin
  IdentifierEqualToShortString := Dict_StrIsEqualToShortString(I^.TI_DVAR,ps)
End;

{ is an identifier and is equal to a given Pascal string }
Function TermIsIdentifierEqualToShortString( T : TermPtr; ident : TString ) : Boolean;
Var
  I : IdPtr Absolute T;
Begin
  TermIsIdentifierEqualToShortString := False;
  If Not IsIdentifier(T) Then
    Exit;
  If Not IdentifierEqualToShortString(I,ident) Then
    Exit;
  TermIsIdentifierEqualToShortString := True
End;

{ is an identifier the cut? }
Function IdentifierIsCut( I : IdPtr ) : Boolean;
Begin
  IdentifierIsCut := IdentifierEqualToShortString(I,'!')
End;

{ is a term the cut? }
Function TermIsCut( T : TermPtr ) : Boolean;
Begin
  TermIsCut := TermIsIdentifierEqualToShortString(T,'!')
End;

{ return an identifier as a (new) string; if Quotes is False, a quoted 
 identifier is returned unquoted, otherwise it is return quoted if it must be
 quoted to be a valid identifier }
Function GetIdentAsStr( I : IdPtr; Quotes : Boolean ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_Clone(IdentifierGetStr(I));
  If Quotes And IdentifierMustBeQuoted(I) Then
    Str_SingleQuoteAndEscape(s);
  GetIdentAsStr := s
End;

{-----------------------------------------------------------------------}
{ methods: functional symbol                                            }
{-----------------------------------------------------------------------}

{ left term of a functional symbol }
Function Func_GetLeft( F : FuncPtr ) : TermPtr;
Var
  T : TermPtr Absolute F;
Begin
  CheckCondition(F <> Nil,'Func_GetLeft of Nil');
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'Func_GetLeft: not a FU');
  Func_GetLeft := F^.TF_LTER
End;

{ right term of a functional symbol }
Function Func_GetRight( F : FuncPtr ) : TermPtr;
Var
  T : TermPtr Absolute F;
Begin
  CheckCondition(F <> Nil,'Func_GetRight of Nil');
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'Func_GetRight: not a FU');
  Func_GetRight := F^.TF_RTER
End;

{ address of left term of a functional symbol }
Function Func_GetLeftAddr( F : FuncPtr ) : TermPtrAddr;
Var
  T : TermPtr Absolute F;
Begin
  CheckCondition(F <> Nil,'Func_GetLeftAddr of Nil');
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'Func_GetLeftAddr: not a FU');
  Func_GetLeftAddr := Addr(F^.TF_LTER)
End;

{ address of right term of a functional symbol }
Function Func_GetRightAddr( F : FuncPtr ) : TermPtrAddr;
Var
  T : TermPtr Absolute F;
Begin
  CheckCondition(F <> Nil,'Func_GetRightAddr of Nil');
  CheckCondition(TypeOfTerm(T) = FuncSymbol,'Func_GetRightAddr: not a FU');
  Func_GetRightAddr := Addr(F^.TF_RTER)
End;

{ set the left term of a functional symbol (no undo) }
Procedure Func_SetLeft( F : FuncPtr; T : TermPtr );
Begin
  CheckCondition(F <> Nil,'Func_SetLeft of Nil');
  F^.TF_LTER := T
End;

{ set the right term of a functional symbol (no undo) }
Procedure Func_SetRight( F : FuncPtr; T : TermPtr );
Begin
  CheckCondition(F <> Nil,'Func_SetRight of Nil');
  F^.TF_RTER := T
End;

{ set the left term of a functional symbol }
Procedure Func_SetLeftWithUndo( F : FuncPtr; T : TermPtr; 
    Undo : Boolean; Var L : RestPtr );
Begin
  CheckCondition(F <> Nil,'Func_SetLeftWithUndo of Nil');
  Rest_SetMem(L,TObjectPtr(F),TObjectPtr(F^.TF_LTER),TObjectPtr(T),Undo)
End;

{ set the right term of a functional symbol }
Procedure Func_SetRightWithUndo( F : FuncPtr; T : TermPtr; 
    Undo : Boolean; Var L : RestPtr );
Begin
  CheckCondition(F <> Nil,'Func_SetRightWithUndo of Nil');
  Rest_SetMem(L,TObjectPtr(F),TObjectPtr(F^.TF_RTER),TObjectPtr(T),Undo)
End;

{-----------------------------------------------------------------------}
{ methods: reduced system                                               }
{-----------------------------------------------------------------------}

{ remove bindings for a functional symbol in the reduced system }
Procedure UnbindFunc( F : FuncPtr );
Begin
  F^.TF_TRED := Nil
End;

{ remove all bindings for an assignable in the reduced system }
Procedure UnbindVar( V : VarPtr );
Begin
  V^.TV_TRED := Nil;
  V^.TV_FWAT := Nil;
  V^.TV_GOAL := Nil
End;

{ remove all bindings of a term }
Procedure Unbind( T : TermPtr );
Begin
  Case TypeOfTerm(T) Of
  Constant,Identifier:
    Pass;
  Variable:
    UnbindVar(VarPtr(T));
  FuncSymbol:
    UnbindFunc(FuncPtr(T))
  End
End;

{ return the first inequation V is watching in the reduced system }
Function WatchIneq( V : VarPtr ) : EqPtr;
Begin
  WatchIneq := V^.TV_FWAT
End;

{ return True if var V already watch an equation equivalent to E }
Function IsWatching( V : VarPtr; E : EqPtr ) : Boolean;
Var
  Ei : EqPtr;
  Found : Boolean;
Begin
  Found := False;
  Ei := WatchIneq(V);
  While (Ei <> Nil) And Not Found Do
  Begin
    Found := Eq_SameAs(Ei,E);
    Ei := Eqs_GetNext(Ei)
  End;
  IsWatching := Found
End;

{ add a list of inequation E to the watch list of variable V }
Procedure AddWatchWithUndo( V : VarPtr; E : EqPtr; 
    Undo : Boolean; Var L : RestPtr );
Var 
  OV : TObjectPtr Absolute V;
  Ec : EqPtr;
  OEc : TObjectPtr Absolute Ec;
Begin
  If WatchIneq(V) = Nil Then { first watch }
    Eq_SetMem(L,OV,V^.TV_FWAT,E,Undo)
  Else
  Begin { add a watch list }
    Ec := WatchIneq(V);
    While (Eqs_GetNext(Ec) <> Nil) Do 
      Ec := Eqs_GetNext(Ec);
    Eq_SetMem(L,OEc,Ec^.EQ_NEXT,E,Undo)
  End
End;

{ return T if equation V = T is in the reduced system, or Nil }
Function VRed( V : VarPtr ) : TermPtr;
Begin
  VRed := V^.TV_TRED
End;

{ return T if equation F = T is in the reduced system, or Nil }
Function FRed( F : FuncPtr ) : TermPtr;
Begin
  FRed := F^.TF_TRED
End;

{ return T2 if equation T = T2 is in the reduced system, Nil otherwise }
Function Red( T : TermPtr ) : TermPtr;
Var 
  FT : FuncPtr Absolute T;
  VT : VarPtr Absolute T;
  IT : IdPtr Absolute T;
  T2 : TermPtr;
Begin
  Case TypeOfTerm(T) Of
  Constant,Identifier:
    T2 := Nil;
  Variable :
    T2 := VRed(VT);
  FuncSymbol:
    T2 := FRed(FT)
  End;
  Red := T2
End;


{ representative of a term t in a reduced system S

  See Colmeraurer (1984, p.93)
  
  S is a reduced system, that is, a system having the following two 
  properties: (i) the left hand sides of its equations are distinct 
  variables, (ii) it does not contain and endless subsystem;
  
  Rep[t,S], the representative of term t in S is defined as follows:

  Rep[t,S] = 
    Rep[t',S] if S contains an equation of the form t=t'
    t otherwise 

  In addition, for convenience:

  Rep[Nil,S] = Nil

  If Reduce is False, the representative is the term itself, even if it has a
   representative 

  If g <> 0, g is used to prevent loops: Mark the term and the RHS of all 
  the equations we go through to compute the representative of T; Stop 
  on the first marked term; this could be the term T itself (either because it
  has no RHS or because a loop points to it).
  }

Function RepresentativeOf( T : TermPtr; Reduce : Boolean; 
    g : TSerial ) : TermPtr;
Var
  T2 : TermPtr;
Begin
  RepresentativeOf := T;
  If (T = Nil) Or (Not Reduce) Or (g <> 0) And Term_GetSeen(T,2,g) Then
    Exit;
  If (g <> 0) And Term_GetSeen(T,2,g) Then 
  Begin
    CWriteWarning('unhandled infinite tree detected!');
    Exit;
  End;
  Term_SetSeen(T,2,g);
  Case TypeOfTerm(T) Of 
  Identifier:
    Exit;
  Variable: { return the identifier is has been assigned to, if any }
    If VarPtr(T)^.TV_IRED <> Nil Then
    Begin
      RepresentativeOf := TermPtr(VarPtr(T)^.TV_IRED);
      Exit
    End
  End;
  T2 := Red(T);
  If T2 = Nil Then
    Exit;
  T2 := RepresentativeOf(T2,Reduce,g);
  RepresentativeOf := T2
End;

{ compute representative, ignoring identifier assignments; to be used in most
 cases }
Function ProtectedRepOf( T : TermPtr ) : TermPtr;
Begin
  ProtectedRepOf := RepresentativeOf(T,True,NewSerial)
End;

{ return True if a term is bound; see pII p60 }
Function IsBound( T : TermPtr ) : Boolean;
Begin
  T := ProtectedRepOf(T);
  IsBound := TypeOfTerm(T) <> Variable
End;

{ return True if a term a free variable }
Function IsFree( T : TermPtr ) : Boolean;
Begin
  IsFree := Not IsBound(T)
End;

{ base operation for CopyTerm }
Function CopyT( T : TermPtr; Assigned : Boolean; g : TSerial ) : TermPtr;
Begin
  CopyT := T;
  If T = Nil Then
    Exit;
  { make a copy of its representative }
  T := ProtectedRepOf(T); { FIXME: better handle infinite trees }
  T := TermPtr(DeepCopyObject(TObjectPtr(T),False,g));
  { delete all bindings }
  Unbind(T);
  { copy children }
  If TypeOfTerm(T) = FuncSymbol Then
    With FuncPtr(T)^ Do
    Begin
      TF_LTER := CopyT(TF_LTER,Assigned,g);
      TF_RTER := CopyT(TF_RTER,Assigned,g)
    End;
  CopyT := T
End;

{ deep copy a term, using the reduced system, ignoring assignments if 
 Assigned is False, getting rid of any assignment, watched inequations and 
 frozen terms; remaining variables are thus duplicated }
Function CopyTerm( T : TermPtr; Assigned : Boolean ) : TermPtr;
Begin
  CopyTerm := CopyT(T,Assigned,NewMemorySerial)
End;


{ return the constant term (number or string constant) the term T is 
  equal to, possibly going through the reduced system, of Nil if T is 
  not equal to a constant term; }
Function EvaluateToConstant( T : TermPtr ) : ConstPtr;
Var 
  CT : ConstPtr Absolute T;
Begin
  T := ProtectedRepOf(T);
  If Not IsConstant(T) Then
    T := Nil;
  EvaluateToConstant := CT
End;

{ return the integer the term T is equal to, Nil otherwise }
Function EvaluateToInteger( T : TermPtr ) : ConstPtr;
Var 
  C : ConstPtr;
Begin
  C := EvaluateToConstant(T);
  If C <> Nil Then
    If ConstType(C) <> IntegerNumber Then
      C := Nil;
  EvaluateToInteger := C
End;

{ return the real the term T is equal to, Nil otherwise }
Function EvaluateToReal( T : TermPtr ) : ConstPtr;
Var 
  C : ConstPtr;
Begin
  C := EvaluateToConstant(T);
  If C <> Nil Then
    If ConstType(C) <> RealNumber Then
      C := Nil;
  EvaluateToReal := C
End;

{ return the string the term T is equal to, Nil otherwise }
Function EvaluateToString( T : TermPtr ) : ConstPtr;
Var 
  C : ConstPtr;
Begin
  C := EvaluateToConstant(T);
  If C <> Nil Then
    If ConstType(C) <> QString Then
      C := Nil;
  EvaluateToString := C
End;

{ return the identifier the term T is equal to, Nil otherwise;
 when looking at the reduced system, ignore global assignments of 
 identifiers, otherwise reassignments would fail: considering two goals
 "assign(ident,1) assign(ident,2))" the ident in the second goal 
 would resolve to 2, not to the ident itself }
Function EvaluateToIdentifier( T : TermPtr ) : IdPtr;
Var
  IT : IdPtr Absolute T;
Begin
  T := ProtectedRepOf(T);
  If Not IsIdentifier(T) Then
    T := Nil;
  EvaluateToIdentifier := IT
End;

{ return True if T evaluates to identifier (TString) ident }
Function EvaluatesToIdentifier( T : TermPtr; ident : TString ) : Boolean;
Var
  I : IdPtr;
Begin 
  I := EvaluateToIdentifier(T);
  If I = Nil Then
    EvaluatesToIdentifier := False
  Else
    EvaluatesToIdentifier := IdentifierEqualToShortString(I,ident)
End;

{ return the access identifier of a term, using the reduced system, or Nil if
 the term has no access identifier }
Function AccessIdentifier( T : TermPtr ) : IdPtr;
Var 
  FT : FuncPtr Absolute T;
  IT : IdPtr Absolute T;
Begin
  If T <> Nil Then
    Case TypeOfTerm(T) Of
    Constant:
      IT := Nil;
    Identifier:
      Begin { ignore assignment, if any }
      End;
    FuncSymbol:
      IT := AccessIdentifier(Func_GetLeft(FT)); { ident(arg1,...) }
    Variable:
      IT := AccessIdentifier(Red(T)) { x = ident }
    End;
  AccessIdentifier := IT
End;

{ return the number of arguments of a tuple, using the reduced system }
Function ArgCount( T : TermPtr ) : PosInt;
Var 
  n : PosInt;
Begin
  n := 0;
  If T = Nil Then
    Exit;
  Case TypeOfTerm(T) Of
  FuncSymbol:
    Begin
      n := 1 + ArgCount(Func_GetRight(FuncPtr(T)))
    End;
  Variable:
    Begin
      n := ArgCount(Red(T))
    End;
  End;
  ArgCount := n
End;

{ return the arity of a term, using the reduced system, or Nil if
 the term has no arity }
Function Arity( T : TermPtr ) : PosInt;
Var 
  a : PosInt;
Begin
  a := 0;
  If T = Nil Then
    Exit;
  Case TypeOfTerm(T) Of
  FuncSymbol:
    Begin
      If AccessIdentifier(Func_GetLeft(FuncPtr(T))) <> Nil Then
        a := ArgCount(Func_GetRight(FuncPtr(T)))
    End;
  Variable:
    Begin
      a := Arity(Red(T))
    End;
  End;
  Arity := a
End;

{-----------------------------------------------------------------------}
{ methods: assignable identifiers                                       }
{-----------------------------------------------------------------------}

{ value or nil if the identifier is not an assigned identifier }
Function GetValue( I : IdPtr ) : TermPtr;
Begin
  GetValue := I^.TI_VALU
End;

{ set the data array }
Procedure SetValue( I : IdPtr; T : TermPtr );
Begin
  I^.TI_VALU := T
End;

{ array or nil if the identifier is not an array }
Function GetArray( I : IdPtr ) : ArrayPtr;
Begin
  GetArray := I^.TI_ARRA
End;

{ set the data array }
Procedure SetArray( I : IdPtr; n : TArraySize; A : ArrayPtr );
Begin
  I^.TI_SIZE := n;
  I^.TI_ARRA := A
End;

{ set I[j]  }
Procedure SetArrayElement( I : IdPtr; j : TArrayIndex; T : TermPtr );
Begin
  Array_SetElement(GetArray(I),j,T)
End;

{ array size, or zero if the identifier is not an array }
Function GetArraySize( I : IdPtr ) : TArraySize;
Begin
  GetArraySize := I^.TI_SIZE
End;

{ Is an identifier an array? }
Function IsArray( I : IdPtr ) : Boolean;
Begin
  IsArray := GetArraySize(I) > 0
End;

{ Set an identifier as assigned }
Procedure SetAsAssigned( I : IdPtr );
Begin
  I^.TI_ASSI := True;
  { identifier's dict entry is now persistent }
  Dict_SetGlobal(I^.TI_DVAR,True);
End;

{ is term T a constant? }
Function IsConstant( T : TermPtr ) : Boolean;
Begin
  IsConstant := TypeOfTerm(T) = Constant
End;

{ is term T an identifier? }
Function IsIdentifier( T : TermPtr ) : Boolean;
Begin
  IsIdentifier := TypeOfTerm(T) = Identifier
End;

{ is term T atomic? }
Function IsAtomic( T : TermPtr ) : Boolean;
Begin
  IsAtomic := IsIdentifier(T) Or IsConstant(T)
End;

{ Is an identifier assigned? (even if it may not be bound yet) }
Function IsAssigned( I : IdPtr ) : Boolean;
Begin
  IsAssigned := I^.TI_ASSI
End;

{ is term T a variable, that is, is or may be the left-hand side of an equation 
  in the reduced system }
Function IsVariable( T : TermPtr ) : Boolean;
Begin
  IsVariable := TypeOfTerm(T) = Variable
End;

{ True if variable V is anonymous }
Function IsAnonymous( V : VarPtr ) : Boolean;
Begin
  IsAnonymous := V^.TV_ANON
End;

{ True if term V is an anonymous variable }
Function IsAnonymousVariable( T : TermPtr ) : Boolean;
Begin
  IsAnonymousVariable := IsVariable(T) And IsAnonymous(VarPtr(T))
End;

{ return a rank used to order a term, as required by the reduction algorithm: 
 variables, anonymous variables, assigned identifiers; we also rank 
 unassigned identifiers and constants }
Function TermRank( T : TermPtr ) : Byte;
Var
  Rank : Byte;
Begin
  Rank := 4; { lowest rank }
  Case TypeOfTerm(T) Of 
  Variable:
    If Not IsAnonymous(VarPtr(T)) Then
      Rank := 1
    Else 
      Rank := 2;
  Identifier:
    If IsAssigned(IdPtr(T)) Then
      Rank := 3
  End;
  TermRank := Rank
End;

{ order two terms }
Procedure OrderTerms( Var T1,T2: TermPtr );
Begin
  If (TermRank(T1) > TermRank(T2)) Or (TermRank(T1) = TermRank(T2)) And 
      Not Term_OrderedWith(T1,T2) Then
    SwapTerms(T1,T2)
End;

{ keep track of "var = assigned ident" unification; this is needed because
  when clearing "assign(test,1) assign(test,2)" the identifier "test" in the 
  second goal will be equal to 1 in the reduced system, and the goal will fail  }
Procedure TrackAssignment( T1,T2: TermPtr );
Var 
  VT1 : VarPtr Absolute T1;
  IT2 : IdPtr Absolute T2;
  VT2 : VarPtr Absolute T2;
Begin        
  OrderTerms(T1,T2);
  CheckCondition(IsVariable(T1),'assigned identifier: not a variable');
  If (TypeOfTerm(T1) = Variable) Then 
  Begin
    Case TypeOfTerm(T2) Of  { var = ident }
    Identifier: 
      Begin
        If (VT1^.TV_IRED = Nil) And IsAssigned(IT2) Then { var = assigned_ident }
          VT1^.TV_IRED := IT2
      End; 
    Variable: { var = var: propagate }
      Begin
        CheckCondition((VT1^.TV_IRED=Nil) Or (VT2^.TV_IRED=Nil) 
            Or (VT1^.TV_IRED=VT2^.TV_IRED),'assigned identifier: unhandled case');
        If (VT1^.TV_IRED = Nil) Then
          VT1^.TV_IRED := VT2^.TV_IRED
        Else If (VT2^.TV_IRED = Nil) Then
          VT2^.TV_IRED := VT1^.TV_IRED
      End
    End
  End
End;

{-----------------------------------------------------------------------}
{ methods: install                                                      }
{-----------------------------------------------------------------------}

{ replace a constant string with ist canonical form, depending on its type;
 return false if the canonical form cannot be computed; FIXME: far from
 perfect, as converting to string is likely to change or round the value }
Function NormalizeConstant( Var s : StrPtr; typ : TConst ) : Boolean;
Var
  i : LongInt;
  r : LongReal;
  code : Integer;
Begin
  NormalizeConstant := False;
  CheckCondition(s <> Nil,'cannot normalize a nul string');
  Case typ Of
  IntegerNumber,
  RealNumber:
    Begin
      If Str_Length(s) > StringMaxSize Then
        Exit;
      Case typ Of
      IntegerNumber:
        i := ShortStringToLongInt(Str_AsShortString(s), code);
      RealNumber:
        r := ShortStringToLongReal(Str_AsShortString(s), code);
      End;
      If code <> 0 Then
        Exit;
      Case typ Of
      IntegerNumber:
        s := Str_NewFromShortString(LongIntToShortString(i));
      RealNumber: 
        s := Str_NewFromShortString(LongRealToShortString(r));
      End;
    End
  End;
  NormalizeConstant := True
End;

{ create a new constant if it does not exist in a list; 
  invariant: 
  - there is a 1:1 relationship between constant terms and constant values, 
  that is, any constant like "123" appearing several times in a program
  of queries only exist through a single constant term (and a single
  dictionary entry as well), and everyone points to that term; consequently
  constant terms and dictionary entries are not copied when they are
  reached through a deep copy;
  - this requires constants to be stored in canonical representation  
  FIXME: use canonical representation for real numbers }
Function InstallConst( Var D : DictPtr; str : StrPtr; ty : TypePrologObj; 
    glob : Boolean ) : ConstPtr;
Var 
  C : ConstPtr;
  TC : TermPtr Absolute C;
  e : DictPtr;
Begin
  e := Dict_Lookup(D,str,[ty],glob);
  If e = Nil Then
  Begin
    C := Const_New;
    e := Dict_Append(D,str,TC,ty,glob);
    C^.TC_DCON := e
  End
  Else
    TC := Dict_GetTerm(e);
  InstallConst := C
End;

{ create a variable if it does not exist in dictionary }
Function InstallVariable( Var D : DictPtr; str : StrPtr; 
    anonymous : Boolean; glob : Boolean ) : VarPtr;
Var
  V : VarPtr;
  e : DictPtr;
Begin
  { each anonymous variable, despite having the same name as the other 
   anonymous variables, is a different variable; accordingly, we do not lookup 
   anonymous variable's name }
  If anonymous Then
    e := Nil
  Else
    e := Dict_Lookup(D,str,[VA],glob);
  If e = Nil Then
  Begin
    V := Var_New(anonymous);
    e := Dict_Append(D,str,TermPtr(V),VA,glob);
    V^.TV_DVAR := e
  End
  Else
    V := VarPtr(Dict_GetTerm(e));
  InstallVariable := V
End;

{ create an identifier if it does not exist in a dictionary; 
 - an identifier is not deep-copyable; 
 - it is assumed that the identifier in str had its single quotes removed if 
   any, and is already in canonical version, so that e.g. eq('abc',abc) 
   succeeds since both refer to the same identifier;
 - the fact that the identifier must be quoted to be valid depends on the 
   current syntax; thus, dynamic changes of syntax do not affect this behavior;
   this is fine as long as the is not used as a syntax converter }
Function InstallIdentifier( Var D : DictPtr; str : StrPtr; Quoted : Boolean;
    glob : Boolean  ) : IdPtr;
Var
  I : IdPtr;
  e : DictPtr;
Begin
  e := Dict_Lookup(D,str,[ID],glob);
  If e = Nil Then
  Begin
    I := Ident_New(Quoted);
    e := Dict_Append(D,str,TermPtr(I),ID,glob);
    I^.TI_DVAR := e
  End
  Else
    I := IdPtr(Dict_GetTerm(e));
  InstallIdentifier := I
End;

End.