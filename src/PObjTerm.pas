{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Objects.pas                                                }
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

{ constant: identifier, number or string; list of constants  }
Const
  MaxSizeConst = 40;  { maximum length of a constant }

Type
  StrConst   = String[MaxSizeConst];

  ConstPtr = ^TObjConst; { term: constant }
  DictConstPtr = ^TObjDictConst; { list of unique constant values }

  TObjConst = Record
    PO_META : TObjMeta;
    TC_DCON : DictConstPtr
  End;

  TObjDictConst = Record
    DC_META : TObjMeta;
    DC_NEXT : DictConstPtr;
    DC_CVAL : StrConst
  End;


{ binary functional symbol }
Type 
  FuncPtr = ^TObjFunc;
  TObjFunc = Record
    PO_META : TObjMeta;
    TF_TRED : TermPtr; { right member of the equation in the reduced system }
    TF_LTER : TermPtr; { left term }
    TF_RTER : TermPtr { right term }
  End;


{ variable }
Const
  MaxSizeIdent = 40;  { maximum length of a variable identifier }

Type 
  StrIdent   = String[MaxSizeIdent];

  VarPtr = ^TObjVar;
  DictVarPtr = ^TObjDictVar; { list of unique constant values }

  TObjVar = Record
    PO_META : TObjMeta;
    { deep copied: }
    TV_TRED : TermPtr; { right member of the equation in the reduced system }
    TV_FWAT : EqPtr; { first inequation this variable watches }
    { not deep copied: }
    TV_DVAR : DictVarPtr
  End;

  TObjDictVar = Record
    DV_META : TObjMeta;
    { deep copied: }
    DV_NEXT : DictVarPtr;
    DV_PVAR : VarPtr;
    { extra data: }
    DV_NAME : StrIdent
  End;


{-----------------------------------------------------------------------}
{ create / destroy                                                      }
{-----------------------------------------------------------------------}

{ create a new constant }
Function NewConst : ConstPtr;
Var 
  C : ConstPtr;
  ptr : TPObjPtr Absolute C;
Begin
  ptr := NewPrologObject(CO, SizeOf(TObjConst), 1, 0);
  With C^ Do
  Begin
    TC_DCON := Nil
  End;
  NewConst := C
End;

{ create a new constant value object }
Function NewConstValue : DictConstPtr;
Var 
  C : DictConstPtr;
  ptr : TPObjPtr Absolute C;
Begin
  ptr := NewPrologObject(CV, SizeOf(TObjDictConst), 1, 0);
  With C^ Do
  Begin
    DC_NEXT := Nil;
    DC_CVAL := ''
  End;
  NewConstValue := C
End;

{ create a new variable }
Function NewVar : VarPtr;
Var 
  V : VarPtr;
  ptr : TPObjPtr Absolute V;
Begin
  ptr := NewPrologObject(VA, SizeOf(TObjVar), 3, 2);
  With V^ Do
  Begin
    TV_TRED := Nil;
    TV_FWAT := Nil;
    TV_DVAR := Nil
  End;
  NewVar := V
End;

{ create a new variable identifier }
Function NewVarIdentifier : DictVarPtr;
Var 
  V : DictVarPtr;
  ptr : TPObjPtr Absolute V;
Begin
  ptr := NewPrologObject(VV, SizeOf(TObjDictConst), 2, 2);
  With V^ Do
  Begin
    DV_NEXT := Nil;
    DV_PVAR := Nil;
    DV_NAME := ''
  End;
  NewVarIdentifier := V
End;

{ create a new binary functional symbol }
Function NewSymbol( T1,T2 : TermPtr ) : FuncPtr;
Var 
  F : FuncPtr;
  ptr : TPObjPtr Absolute F;
Begin
  ptr := NewPrologObject(FU, SizeOf(TObjFunc), 3, 3);
  With F^ Do
  Begin
    TF_TRED := Nil;
    TF_LTER := T1;
    TF_RTER := T2
  End;
  NewSymbol := F
End;


{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Type TTerm = (Variable,Constant,FuncSymbol,Dummy);

{ type of term }
Function TypeOfTerm( T : TermPtr ) : TTerm;
Begin
  TypeOfTerm := Dummy;
  If T <> Nil Then
    Case T^.PO_META.PO_TYPE Of
    CO :
      TypeOfTerm := Constant;
    VA :
      TypeOfTerm := Variable;
    FU :
      TypeOfTerm := FuncSymbol
    End
End;

{ return the access constant of a term, or Nil; use for quick pre-unification }
Function Access( T : TermPtr ) : ConstPtr;
Var 
  C : ConstPtr;
  FT : FuncPtr Absolute T;
  CT : ConstPtr Absolute T;
  LeftT : TermPtr;
  CLeftT : ConstPtr Absolute LeftT;
Begin
  C := Nil;
  Case TypeOfTerm(T) Of
  Constant :
    C := CT;
  FuncSymbol  :
    Begin
      LeftT := FT^.TF_LTER;
      Case TypeOfTerm(LeftT) Of
      Constant :
        C := CLeftT;
      FuncSymbol  :
        Begin
        End
      End
    End
  End;
  Access := C
End;

{ left term of a functional symbol }
Function LeftArg( F : FuncPtr ) : TermPtr;
Begin
  LeftArg := F^.TF_LTER
End;

{ right term of a functional symbol }
Function RightArg( F : FuncPtr ) : TermPtr;
Begin
  RightArg := F^.TF_RTER
End;

{ return the number of arguments of a f(a,b,c) or <a,b,c> construct;
  in the former case, the number of argument is 4 }
Function NbArguments( F : FuncPtr ) : Integer;
Var 
  T : TermPtr;
  FT : FuncPtr Absolute T;
Begin
  CheckCondition(F <> Nil,'NbArguments of Nil does not make sense');
  If F^.TF_RTER = Nil  Then
    NbArguments := 1
  Else
  Begin
    T := F^.TF_RTER;
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
    Argument := F^.TF_LTER
  Else
  Begin
    T := F^.TF_RTER;
    Argument := Argument(N-1,FT)
  End
End;


{ look in a list for a constant value; append it to the list if not found;
  return a pointer to the list entry }
Function LookupConst( Var list : DictConstPtr; str : StrConst ) : DictConstPtr;
Var
  e : DictConstPtr;
  Found : Boolean;
Begin
  e := list;
  Found := False;
  While (e<>Nil) And Not Found Do
  Begin
    If e^.DC_CVAL = str Then
      Found := True
    Else
      e := e^.DC_NEXT
  End;
  If Not Found Then
  Begin
    e := NewConstValue;
    With e^ Do
    Begin
      DC_CVAL := str;
      DC_NEXT := list
    End;
    list := e
  End;
  LookupConst := e
End;


{ create a new constant }
Function InstallConst( Var list : DictConstPtr; Ch : StrConst ) : ConstPtr;
Var C : ConstPtr;
Begin
  C := NewConst;
  With C^ Do
  Begin
    TC_DCON := LookupConst(list,Ch)
  End;
  InstallConst := C
End;


{ look in a list for an identifier, from start to stop (excluding stop); 
  return a pointer to the list entry, or Nil }
Function LookupVarIdentifier( start,stop : DictVarPtr; str : StrIdent ) : DictVarPtr;
Var
  e : DictVarPtr;
  Found : Boolean;
Begin
  e := start;
  Found := False;
  While (e<>Nil) And (e<>stop) And Not Found Do
  Begin
    If e^.DV_NAME = str Then
      Found := True
    Else
      e := e^.DV_NEXT
  End;
  If Not Found Then
    e := Nil;
  LookupVarIdentifier := e
End;

{ append a variable to the dictionary }
Function AppendVarIdentifier( Var list : DictVarPtr; str : StrIdent; V : VarPtr ) : DictVarPtr;
Var e : DictVarPtr;
Begin
  e := NewVarIdentifier;
  With e^ Do
  Begin
    DV_NEXT := list;
    DV_NAME := str;
    DV_PVAR := V
  End;
  list := e;
  AppendVarIdentifier := e
End;

{ create a variable if it does not exist in list (up to stop, excluded); return it }
Function InstallVariable( Var list : DictVarPtr; stop : DictVarPtr; Ch : StrIdent ) : VarPtr;
Var
  V : VarPtr;
  DV : DictVarPtr;
Begin
  DV := LookupVarIdentifier(list,stop,Ch);
  If DV = Nil Then
  Begin
    V := NewVar;
    DV := AppendVarIdentifier(list,Ch,V);
    V^.TV_DVAR := DV
  End
  Else
    V := DV^.DV_PVAR;
  InstallVariable := V
End;
