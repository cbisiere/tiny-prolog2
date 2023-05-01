{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjTerm.pas                                               }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                P R O L O G   O B J E C T S :   T E R M S                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ constant: identifier, number or quoted string; list of constants  }
Type
  TConst = (Number,QString);

  ConstPtr = ^TObjConst; { term: constant }
  TObjConst = Record
    PO_META : TObjMeta;
    { not deep copied: }
    TC_DCON : DictPtr 
    
  End;

{ binary functional symbol }
Type 
  FuncPtr = ^TObjFunc;
  TObjFunc = Record
    PO_META : TObjMeta;
    { deep copied: }
    TF_TRED : TermPtr; { right member of the equation in the reduced system }
    TF_LTER : TermPtr; { left term }
    TF_RTER : TermPtr { right term }
  End;

{ variable or (assignable) identifier; only variables are subject to deep-copy }
Type 
  AssPtr = ^TObjAss;
  VarPtr = AssPtr; { variable }
  IdPtr = AssPtr; { identifier: TV_TRED is set when the identifier has been assigned }

  TObjAss = Record
    PO_META : TObjMeta;
    { VA: deep copied: }
    TV_TRED : TermPtr; { right member of the equation in the reduced system }
    TV_FWAT : EqPtr; { first inequation this variable watches }
    { VA: not deep copied: }
    TV_DVAR : DictPtr; { dictionary entry }
    TV_IRED : IdPtr; { VA: identifier this variable as been initially bound to }
    { extra data: }
    TV_ASSI : Boolean { true if the term is an identifier that can be assigned }
  End;



{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new constant }
Function NewConst : ConstPtr;
Var 
  C : ConstPtr;
  ptr : TPObjPtr Absolute C;
Begin
  ptr := NewPrologObject(CO,SizeOf(TObjConst),1,False,0);
  With C^ Do
  Begin
    TC_DCON := Nil
  End;
  NewConst := C
End;

{ create a (potentially) assignable object: variable or identifier }
Function NewAssignable( ty : TypePrologObj; CanCopy : Boolean) : AssPtr;
Var 
  V : AssPtr;
  ptr : TPObjPtr Absolute V;
Begin
  ptr := NewPrologObject(ty,SizeOf(TObjAss),4,CanCopy,2);
  With V^ Do
  Begin
    TV_TRED := Nil;
    TV_FWAT := Nil;
    TV_DVAR := Nil;
    TV_IRED := Nil;
    TV_ASSI := False
  End;
  NewAssignable := V
End;

{ create a new binary functional symbol }
Function NewSymbol( T1,T2 : TermPtr ) : FuncPtr;
Var 
  F : FuncPtr;
  ptr : TPObjPtr Absolute F;
Begin
  ptr := NewPrologObject(FU, SizeOf(TObjFunc),3,True,3);
  With F^ Do
  Begin
    TF_TRED := Nil;
    TF_LTER := T1;
    TF_RTER := T2
  End;
  NewSymbol := F
End;


{-----------------------------------------------------------------------}
{ methods: terms                                                        }
{-----------------------------------------------------------------------}

Type TTerm = (Variable,Identifier,Constant,FuncSymbol,Dummy);

{ type of term }
Function TypeOfTerm( T : TermPtr ) : TTerm;
Begin
  TypeOfTerm := Dummy; { FIXME: get rid of this }
  If T <> Nil Then
    Case T^.PO_META.PO_TYPE Of
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


{ return true if T1 xor T2 is Nil }
Function OneIsNil( T1,T2 : TermPtr ) : Boolean;
Begin
  OneIsNil := (T1=Nil) Xor (T2=Nil)
End;

{ swap two terms }
Procedure SwapTerms( Var T1, T2 : TermPtr );
Var Tmp : TermPtr;
Begin
  Tmp := T1;
  T1 := T2;
  T2 := Tmp
End;

{-----------------------------------------------------------------------}
{ methods: constants                                                    }
{-----------------------------------------------------------------------}

{ return the type of a constant }
Function ConstType( C : ConstPtr ) : TConst;
Var t : TConst;
Begin
  Case C^.TC_DCON^.DE_TYPE Of
  CS: t := QString;
  CN: t := Number;
  End;
  ConstType := t
End;

{ return the string value of a constant; not cloning }
Function ConstGetStr( C : ConstPtr ) : StrPtr;
Begin
  ConstGetStr := C^.TC_DCON^.DE_STRI
End;

{ return the Pascal string value of a constant, shortening the
  result if the string object is longer than 255 characters }
Function ConstGetPStr( C : ConstPtr ) : AnyStr;
Var s : StrPtr;
Begin
  s := ConstGetStr(C);
  ConstGetPStr := StrGetFirstData(s)
End;

{ is a constant equal to a given Pascal string? }
Function ConstEqualTo( C : ConstPtr; ps : AnyStr ) : Boolean;
Begin
  ConstEqualTo := DictStrEqualTo(C^.TC_DCON,ps)
End;

{ is a constant starting with a char in a given set? not UTF-8 aware }
Function ConstStartWith( C : ConstPtr; E : CharSet ) : Boolean;
Begin
  ConstStartWith := DictStrStartWith(C^.TC_DCON,E)
End;

{-----------------------------------------------------------------------}
{ methods: identifiers                                                  }
{-----------------------------------------------------------------------}

{ return the string value of an identifier; not cloning }
Function IdentifierGetStr( I : IdPtr ) : StrPtr;
Begin
  IdentifierGetStr := I^.TV_DVAR^.DE_STRI
End;

{ return the Pascal string value of an identifier, shortening the
  result if the string object is longer than 255 characters }
Function IdentifierGetPStr( I : IdPtr ) : AnyStr;
Var s : StrPtr;
Begin
  s := IdentifierGetStr(I);
  IdentifierGetPStr := StrGetFirstData(s)
End;

{ is an identifier equal to a given Pascal string? }
Function IdentifierEqualTo( I : IdPtr; ps : AnyStr ) : Boolean;
Begin
  IdentifierEqualTo := DictStrEqualTo(I^.TV_DVAR,ps)
End;

{-----------------------------------------------------------------------}
{ methods: functional symbol                                            }
{-----------------------------------------------------------------------}

{ left term of a functional symbol }
Function FLeftArg( F : FuncPtr ) : TermPtr;
Begin
  FLeftArg := F^.TF_LTER
End;

{ right term of a functional symbol }
Function FRightArg( F : FuncPtr ) : TermPtr;
Begin
  FRightArg := F^.TF_RTER
End;

{ return the number of arguments of a f(a,b,c) or <a,b,c> construct;
  in the former case, the number of argument is 4 }
Function NbArguments( F : FuncPtr ) : Integer;
Var 
  T : TermPtr;
  FT : FuncPtr Absolute T;
Begin
  CheckCondition(F <> Nil,'NbArguments of Nil does not make sense');
  If FRightArg(F) = Nil  Then
    NbArguments := 1
  Else
  Begin
    T := FRightArg(F);
    NbArguments := NbArguments(FT) + 1
  End
End;

{ return the N-th argument of a f(a,b,c) or <a,b,c> construct;
  in the former case, the 1st argument is f }
Function Argument( N : Integer; F : FuncPtr ) : TermPtr;
Var 
  T : TermPtr;
  FT : FuncPtr Absolute T;
Begin
  CheckCondition(F <> Nil,'Argument of Nil does not make sense');
  If N = 1 Then
    Argument := FLeftArg(F)
  Else
  Begin
    T := FRightArg(F);
    Argument := Argument(N-1,FT)
  End
End;

{-----------------------------------------------------------------------}
{ methods: reduced system                                               }
{-----------------------------------------------------------------------}

{ return the first inequation V is watching in the reduced system }
Function WatchIneq( V : VarPtr ) : EqPtr;
Begin
  WatchIneq := V^.TV_FWAT
End;

{ add an inequation E to the watch list of variable V }
Procedure AddWatch( V : VarPtr; E : EqPtr; Backtrackable : Boolean; Var L : RestorePtr );
Var Ec : EqPtr;
Begin
  If WatchIneq(V) = Nil Then { first watch }
    SetMemEq(L,V^.TV_FWAT,E,Backtrackable)
  Else
  Begin { add a watch }
    Ec := WatchIneq(V);
    While (Ec^.EQ_NEXT <> Nil) Do Ec := Ec^.EQ_NEXT;
    SetMemEq(L,Ec^.EQ_NEXT,E,Backtrackable)
  End
End;

{ return T if equation V = T is in the reduced system, or Nil }
Function VRed( V : VarPtr ) : TermPtr;
Begin
  VRed := V^.TV_TRED
End;

{ return T if equation I = T is in the reduced system, or Nil }
Function IRed( I : IdPtr ) : TermPtr;
Begin
  IRed := I^.TV_TRED
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
  Constant:
    T2 := Nil;
  Identifier:
    T2 := IRed(IT);
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
    t otherwise }

Function RepresentativeOf( T : TermPtr ) : TermPtr;
Var
  T2 : TermPtr;
Begin
  If T <> Nil Then
  Begin
    T2 := Red(T);
    If T2<>Nil Then
      T := RepresentativeOf(T2)
  End;
  RepresentativeOf := T
End;

{ return the constant term (number or string constant) the term T is 
  equal to, possibly going through the reduced system, of Nil if T is 
  not equal to a constant term; 
  TODO: make sure this cannot loop }
Function EvaluateToConstant( T : TermPtr ) : ConstPtr;
Var 
  CT : ConstPtr Absolute T;
Begin
  T := RepresentativeOf(T);
  If TypeOfTerm(T) <> Constant Then
    T := Nil;
  EvaluateToConstant := CT
End;

{ return the identifier term the term T is equal to, possibly going 
  through the reduced system, of Nil if T is not equal to an 
  identifier term; note: when looking at the reduced system we do
  not consider the global assignments of identifiers, otherwise
  reassignments would fail: considering two goals
  "assign(ident,1) assign(ident,2))" the ident in the second goal 
  would resolve to 2, not to the ident itself 
  TODO: take into account evaluable functions: add, mul, div...
  TODO: make sure this cannot loop }
Function EvaluateToIdentifier( T : TermPtr ) : IdPtr;
Var
  IT : IdPtr Absolute T;
Begin
  If T <> Nil Then
    If TypeOfTerm(T) <> Identifier Then
      IT := EvaluateToIdentifier(Red(T));
  EvaluateToIdentifier := IT
End;

{ return the access identifier of a term, or Nil }
Function AccessIdentifier( T : TermPtr ) : IdPtr;
Var 
  FT : FuncPtr Absolute T;
  IT : IdPtr Absolute T;
Begin
  If TypeOfTerm(T) = FuncSymbol Then
    T := FLeftArg(FT);
  IT := EvaluateToIdentifier(T);
  AccessIdentifier := IT
End;


{-----------------------------------------------------------------------}
{ methods: assignable identifiers                                       }
{-----------------------------------------------------------------------}

{ Is an identifier assigned? (even if it may not be bound yet) }
Function IsAssigned( I : IdPtr ) : Boolean;
Begin
  IsAssigned := I^.TV_ASSI
End;

{ is term T a variable, that is, is or may be the left-hand side of an equation 
  in the reduced system }
Function IsVariable( T : TermPtr ) : Boolean;
Var 
  Ok : Boolean;
  IT : IdPtr Absolute T;
Begin
  Ok := (TypeOfTerm(T)=Variable) Or (TypeOfTerm(T)=Identifier);
  If Ok And (TypeOfTerm(T)=Identifier) Then
    Ok := IsAssigned(IT);
  IsVariable := Ok
End;

{ order two terms if one of them happens to be an identifier, the other one
  not being a variable; in that case, an assigned identifier should come 
  first; if both are assigned identifiers, the order is defined as in the 
  memory management system; this ordering is necessary for the  reduction
  algorithm to work properly, as an identifier, once assigned, behaves as a
  variable }
Procedure OrderIdentifiers( Var T1,T2: TermPtr );
Var 
  IT1 : IdPtr Absolute T1;
  IT2 : IdPtr Absolute T2;
Begin
  If TypeOfTerm(T2) = Identifier Then 
    If IsAssigned(IT2) Then
      If TypeOfTerm(T1) = Identifier Then
        If Not (IsAssigned(IT1) And ObjectsAreOrdered(T1,T2)) Then
          SwapTerms(T1,T2)
End;

{ order two terms, as required by the reduction algorithm: 
  (i) variables, assigned identifiers, in that order 
  (ii) withing those two types, order is defined as in the memory management system 
  having variable first is required by the reduction algorithm; this implementation 
  also takes into account (dynamically) assigned identifiers }
Procedure OrderTerms( Var T1,T2: TermPtr );
Begin
  If (TypeOfTerm(T2) = Variable) And 
    Not ((TypeOfTerm(T1) = Variable) And ObjectsAreOrdered(T1,T2)) Then
    SwapTerms(T1,T2)
  Else 
    OrderIdentifiers(T1,T2)
End;

{ keep track that of "var = assigned ident" unification; this is needed because
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
    CheckCondition(VT1^.TV_IRED = Nil,'assigned identifier: unhandled case 2');
    If (TypeOfTerm(T2) = Identifier) Then 
    Begin
      If (VT1^.TV_IRED = Nil) And IsAssigned(IT2) Then { var = assigned_ident }
        VT1^.TV_IRED := IT2
    End 
    Else If (TypeOfTerm(T2) = Variable) Then { var = var: propagate }
      If (VT1^.TV_IRED = Nil) And (VT2^.TV_IRED <> Nil) Then
        VT1^.TV_IRED := VT2^.TV_IRED
  End 
End;

{-----------------------------------------------------------------------}
{ methods: install                                                      }
{-----------------------------------------------------------------------}

{ create a new constant if it does not exist in a list; 
  invariant: 
  - there is a 1:1 relationship between constant terms and constant values, 
  that is, any constant like "123" appearing several times in a program
  of queries only exist through a single constant term (and a single
  dictionary entry as well), and everyone points to that term; consequently
  constant terms and dictionary entries are not copied when they are
  reached through a deep copy }
Function InstallConst( Var list : DictPtr; str : StrPtr; ty : TypePrologObj ) : ConstPtr;
Var 
  C : ConstPtr;
  TC : TermPtr Absolute C;
  e : DictPtr;
Begin
  e := DictLookup(list,Nil,str);
  If e = Nil Then
  Begin
    C := NewConst;
    e := DictAppend(list,str,TC,ty);
    C^.TC_DCON := e
  End
  Else
    TC := e^.DE_TERM;
  InstallConst := C
End;

{ create an assignable object (variable or identifier) if it does not 
  exist in list (up to stop, excluded); return it }
Function InstallAssignable( Var list : DictPtr; stop : DictPtr; 
    str : StrPtr; ty : TypePrologObj; CanCopy : Boolean) : AssPtr;
Var
  V : AssPtr;
  TV : TermPtr Absolute V;
  e : DictPtr;
Begin
  CheckCondition((ty=VA) Or (ty=ID),'InstallAssignable: VA or ID expected');
  e := DictLookup(list,stop,str);
  If e = Nil Then
  Begin
    V := NewAssignable(ty,CanCopy);
    e := DictAppend(list,str,TV,VA);
    V^.TV_DVAR := e
  End
  Else
    TV := e^.DE_TERM;
  InstallAssignable := V
End;

{ create a variable if it does not exist in list (up to stop, excluded);
  a variable is subject to deep copy }
Function InstallVariable( Var list : DictPtr; stop : DictPtr; str : StrPtr ) : VarPtr;
Begin
  InstallVariable := InstallAssignable(list,stop,str,VA,True)
End;

{ create an identifier if it does not exist in list; an identifier is
  not deep-copyable }
Function InstallIdentifier( Var list : DictPtr; str : StrPtr ) : IdPtr;
Begin
  InstallIdentifier := InstallAssignable(list,Nil,str,ID,False)
End;
