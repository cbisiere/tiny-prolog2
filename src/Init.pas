{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Init.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     I N I T I A L I S A T I O N S                          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ syntax switches; each switch is also the filename part of the start program }
Type 
  TStartFile = Array[TSyntax] Of String[4];
Const 
  StartFile : TStartFile = ('PII','PIIc','PIIp','E');

{ file suffix: for PII+ and Edinburgh, see PII+ doc p297 }
Type 
  TFileExt = Array[TSyntax] Of String[3];
Const 
  FileExt : TFileExt = ('pro','p2c','p2','p2E');

{ parse and process command line parameters }
Procedure ProcessParameters( P : ProgPtr );
Var  
  i : Byte;
  par,filename : TString;
  y : TSyntax;
  Found, HasFile : Boolean;
  s : StrPtr;
  os : TPObjPtr Absolute s;
Begin
  i := 1; { index of the parameter to process }

  { syntax parameter }
  Found := False;
  If ParamCount >= 1 Then
  Begin
    par := ParamStr(1);
    If par[1] = '-' Then
    Begin
      Delete(par,1,1);
      For y := PrologII To Edinburgh Do
      Begin
        If StartFile[y] = par Then
        Begin
          SetSyntax(P,y);
          Found := True
        End
      End;
      If Not Found Then
        RaiseError('Syntax parameter: Unknown syntax identifier');
      i := i + 1
    End
  End;

  { user file }
  HasFile := False;
  If (Not Error) And (ParamCount = i) Then
  Begin
    HasFile := True;
    filename := ParamStr(i);
    { detect format from file ext if no syntax parameter }
    If Not Found Then
      If EndsWith(par,'.pl') Then
        SetSyntax(P,Edinburgh)
      Else
      For y := PrologII To Edinburgh Do
        If EndsWith(par,'.' + FileExt[y]) Then
          SetSyntax(P,y)
  End;

  { load the startup file }
  If Not Error Then
  Begin
    y := GetSyntax(P);
    s := NewStringFrom('start/' + StartFile[y] + '.' + FileExt[y]);
    AddGCRoot(os); { protect this string from GC }
    SetRuleType(P,RTYPE_AUTO);
    LoadProgram(P,s)
  End;

  { from now on, all rules are user rules }
  SetRuleType(P,RTYPE_USER);

  { load the user file }
  If (Not Error) And (HasFile) Then
  Begin
    s := NewStringFrom(filename);
    AddGCRoot(os); { protect this string from GC }
    LoadProgram(P,s)
  End
End;

{ initialize various subsystems }
Procedure Initialize;
Begin
  MMInit;
  InitIO;
  InitCrt;
  InitTrace;
  InitReadline;
  OngoingCoreDump := False;
  Error := False
End;

{ reset the Prolog engine }
Function CreateProgram : ProgPtr;
Var 
  P : ProgPtr;
  OP : TPObjPtr Absolute P;
Begin
  P := NewProgram;
  AddGCRoot(OP);
  CurrentProgram := P; 
  RegisterPredefined(P);
  CreateProgram := P
End;
