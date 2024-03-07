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

Unit Init;

Interface

Uses 
  Strings,
  Errs,
  Files,
  Memory,
  PObjStr,
  PObjProg,
  Debug,
  Predef,
  Engine;

Procedure ProcessParameters( P : ProgPtr );
Function CreateProgram : ProgPtr;

Implementation
{-----------------------------------------------------------------------------}

{ syntax switches; each switch is also the filename part of the start program }
Type 
  TStartFile = Array[TSyntax] Of String[4];
Const 
  StartFile : TStartFile = ('PII','PIIc','PIIp','E');

{ parse and process command line parameters }
Procedure ProcessParameters( P : ProgPtr );
Var  
  i : Byte;
  par,filename : TString;
  y : TSyntax;
  s : StrPtr;
  os : TObjectPtr Absolute s;
  KnownPar, HasFilePar, HasSyntaxPar, SkipStartFile : Boolean;
Begin

  HasSyntaxPar := False;
  SkipStartFile := False;
  HasFilePar := False;

  For i := 1 To ParamCount Do
  Begin
    par := ParamStr(i);
    If par[1] = '-' Then
    Begin
      KnownPar := False;
      Delete(par,1,1);
      { syntax parameter }
      For y := PrologII To Edinburgh Do
      Begin
        If StartFile[y] = par Then
        Begin
          If HasSyntaxPar Then
            RaiseError('Syntax parameter cannot be used more than once');
          SetSyntax(P,y);
          HasSyntaxPar := True;
          KnownPar := True
        End
      End;
      { other parameters }
      If Not KnownPar Then
        If par = 'D' Then
        Begin
          SkipStartFile := True;
          KnownPar := True
        End;
      If Not KnownPar Then
        RaiseError('Parameter: Unknown option')
    End
    Else
    Begin
      { user file }
      If HasFilePar Then
        RaiseError('Parameter: file already set');
      HasFilePar := True;
      filename := par;
    End
  End;

  { detect syntax from file ext if no syntax parameter is set }
  If Not Error And HasFilePar And Not HasSyntaxPar Then
  Begin
    If EndsWith(filename,'.pl') Then
      SetSyntax(P,Edinburgh)
    Else
    For y := PrologII To Edinburgh Do
      If EndsWith(filename,'.' + FileExt[y]) Then
        SetSyntax(P,y)
  End;

  { load the startup file }
  If Not Error And Not SkipStartFile Then
  Begin
    y := GetSyntax(P);
    s := NewStringFrom('start/' + StartFile[y] + '.' + FileExt[y]);
    AddGCRoot(os); { protect this string from GC }
    SetRuleType(P,RTYPE_AUTO);
    LoadProgram(P,Nil,s,False)
  End;

  { from now on, all rules are user rules }
  SetRuleType(P,RTYPE_USER);

  { load the user file }
  If Not Error And HasFilePar Then
  Begin
    SetProgramPath(P,ExtractPath(filename));
    s := NewStringFrom(filename);
    AddGCRoot(os); { protect this string from GC }
    LoadProgram(P,Nil,s,False)
  End
End;

{ reset the Prolog engine }
Function CreateProgram : ProgPtr;
Var 
  P : ProgPtr;
  OP : TObjectPtr Absolute P;
Begin
  P := NewProgram;
  AddGCRoot(OP);
  SetCurrentProgram(P); 
  RegisterPredefined(P);
  CreateProgram := P
End;

End.