{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dict.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         D i C T I O N A R I E S                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Const
  MaxConst     = 150; { maximum number of constants }
  MaxVar       = 150; { maximum number of variables }
  MaxSizeConst = 40;  { maximum length of a constant }
  MaxSizeIdent = 40;  { maximum length of a variable identifier }

Type
  StrIdent   = String[MaxSizeIdent];
  StrConst   = String[MaxSizeConst];
  TDictConst = Array[1..MaxConst] Of StrConst; 
  TDictVar   = Array[1..MaxVar] Of
    Record
      Name : StrIdent; { variable name           }
      Ptr : VarPtr     { pointer to the variable }
    End;

Var
  DictVar   : TDictVar;   { dictionary of variables     }
  DictConst : TDictConst; { dictionary of constants     }
  NbVar     : Integer;    { current number of variables }
  NbConst   : Integer;    { current number of constants }

{ look for a constant in the dictionary; append it if not found }
Function IndexConst( C : StrConst ) : Integer;
Var
  I,Po   : Integer;
  Found  : Boolean;
Begin
  Found := False;
  I      := 1;
  While (I<=NbConst) And Not Found Do
  Begin
    If DictConst[I] = C Then
    Begin
      Found := True;
      Po     := I;
    End
    Else
      I := I + 1
  End;
  If Not Found Then
  Begin
    NbConst := NbConst + 1;
    CheckCondition(NbConst <= MaxConst,'Maximum number of constants reached');
    DictConst[NbConst] := C;
    Po := NbConst
  End;
  IndexConst := Po
End;

{ return the index of a variable name in the dictionary, starting from position
  Start; return 0 if the name is not found } 
Function Position( Start  : Integer; Elt : StrIdent ) : Integer;
Var
  I,Pos   : Integer;
  Found  : Boolean;
Begin
  Pos := 0;
  Found := False;
  I := Start;
  While (I<=NbVar) And Not Found Do
  Begin
    If DictVar[I].Name = Elt Then
    Begin
      Found := True;
      Pos    := I;
    End
    Else
      I := I + 1
  End;
  Position := Pos
End;

{ append a variable to the dictionary }
Function AddVarToDict( VarName : StrIdent; Addr : VarPtr ) : Integer;
Begin
  NbVar := NbVar + 1;
  CheckCondition(NbVar <= MaxVar,'Maximum number of variables reached');
  DictVar[NbVar].Name := VarName;
  DictVar[NbVar].Ptr := Addr;
  AddVarToDict := NbVar
End;

{ return the variable stored at index Pos }
Function GetVarPtr( Pos : Integer ) : VarPtr;
Begin
  GetVarPtr := DictVar[Pos].Ptr
End;


{ create a new constant; 
  return its index in the dictionary  }
Function InstallConst( Ch : StrConst ) : ConstPtr;
Var C : ConstPtr;
Begin
  C := NewConst;
  With C^ Do
  Begin
    TC_CONS := IndexConst(Ch)
  End;
  InstallConst := C
End;


{ create a variable if it does not exist in the current rule yet; 
  return its index in the dictionary }
Function InstallVariable( Ch : StrIdent; Start : Integer ) : VarPtr;
Var
  V : VarPtr;
  PosInDictVar : Integer;
Begin
  PosInDictVar := Position(Start,Ch);
  If PosInDictVar = 0 Then
  Begin
    V := NewVar;
    With V^ Do
    Begin
      PosInDictVar := AddVarToDict(Ch,V);
      TV_NVAR := PosInDictVar
    End;
    InstallVariable := V
  End
  Else
    InstallVariable := GetVarPtr(PosInDictVar)
End;
