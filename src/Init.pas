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

{ parse and process command line parameters }
Procedure ProcessParameters( P : ProgPtr );
Var  
  i : 1..2;
  par : TString;
  y : TSyntax;
  Found : Boolean;
  s : StrPtr;
  os : TPObjPtr Absolute s;
Begin
  i := 1; { index of the parameter to process }

  { syntax switch }
  If ParamCount >= 1 Then
  Begin
    par := ParamStr(1);
    If par[1] = '-' Then
    Begin
      Delete(par,1,1);
      Found := False;
      For y := PrologII To Edinburgh Do
      Begin
        If StartFile[y] = par Then
        Begin
          SetSyntax(P,y);
          Found := True
        End
      End;
      If Not Found Then
        RaiseError('Syntax switch: Unknown syntax identifier');
      i := i + 1
    End
  End;

  { load the startup file }
  If Not Error Then
  Begin
    s := NewStringFrom('start/' + StartFile[GetSyntax(P)]+'.pro');
    AddGCRoot(os); { protect this string from GC }
    SetRuleType(P,RTYPE_AUTO);
    LoadProgram(P,s)
  End;

  { from now on, all rules are user rules }
  SetRuleType(P,RTYPE_USER);

  { user file }
  If (Not Error) And (ParamCount = i) Then
  Begin
    s := NewStringFrom(ParamStr(i));
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
