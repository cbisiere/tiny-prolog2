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

{ constant: identifier, number or string  }
Type 
  ConstPtr = ^TObjConst;
  TObjConst = Record
    PO_META : TObjMeta;
    TC_CONS : Integer { index in the dictionary }
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
Type 
  VarPtr = ^TObjVar;
  TObjVar = Record
    PO_META : TObjMeta;
    TV_TRED : TermPtr; { right member of the equation in the reduced system }
    TV_FWAT : EqPtr; { first inequation this variable watches }
    TV_NVAR : Integer { index in the dictionary }
  End;


{-----------------------------------------------------------------------}
{ create / destroy                                                      }
{-----------------------------------------------------------------------}

{ create a new constant }
Function NewConst : ConstPtr;
Var C : ConstPtr;
Begin
  C := ConstPtr(NewPrologObject(CO, SizeOf(TObjConst), 0));
  With C^ Do
  Begin
    TC_CONS := 0
  End;
  NewConst := C
End;

{ create a new variable }
Function NewVar : VarPtr;
Var V : VarPtr;
Begin
  V := VarPtr(NewPrologObject(VA, SizeOf(TObjVar), 2));
  With V^ Do
  Begin
    TV_NVAR := 0;
    TV_TRED := Nil;
    TV_FWAT := Nil
  End;
  NewVar := V
End;

{ create a new binary functional symbol }
Function NewSymbol( T1,T2 : TermPtr ) : FuncPtr;
Var F : FuncPtr;
Begin
  F := FuncPtr(NewPrologObject(FU, SizeOf(TObjFunc), 3));
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
Var LeftT : TermPtr;
Begin
  Access := Nil;
  Case TypeOfTerm(T) Of
  Constant :
    Access := ConstPtr(T);
  FuncSymbol  :
    Begin
      LeftT := FuncPtr(T)^.TF_LTER;
      Case TypeOfTerm(LeftT) Of
      Constant :
        Access := ConstPtr(LeftT);
      FuncSymbol  :
        Begin
        End
      End
    End
  End
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
Begin
  CheckCondition(F <> Nil,'NbArguments of Nil does not make sense');
  If F^.TF_RTER = Nil  Then
    NbArguments := 1
  Else
    NbArguments := NbArguments(FuncPtr(F^.TF_RTER)) + 1
End;

{ return the N-th argument of a f(a,b,c) or <a,b,c> construct;
  in the former case, the 1st argument is f }
Function Argument( N : Integer; F : FuncPtr ) : TermPtr;
Begin
  CheckCondition(F <> Nil,'Argument of Nil does not make sense');
  If N = 1 Then
    Argument := F^.TF_LTER
  Else
    Argument := Argument(N-1,FuncPtr(F^.TF_RTER))
End;





