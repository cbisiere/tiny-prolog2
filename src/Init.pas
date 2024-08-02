{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Init.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
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
  ShortStr,
  Errs,
  Chars,
  Files,
  Memory,
  PObjStr,
  PObjDef,
  PObjProg,
  PObjIO,
  Debug,
  Predef,
  Engine;

Function CreateProgram : ProgPtr;

Implementation
{-----------------------------------------------------------------------------}

{ syntax switches; each switch is also the Filename part of the start program }
Type 
  TStartFile = Array[TSyntax] Of String[5];
Const 
  StartFile : TStartFile = ('PIIv1','PII','PIIp','E'); { must be ASCII only }

Const
  DEFAULT_PROLOG_SYNTAX : TSyntax = PrologII;


{ parse the command line parameters }
Procedure ParseCL( Var Syntax : TSyntax; Var SkipStartFile : Boolean;
    Var HasUserFilePar : Boolean; Var Filename : TShortPath );
Var  
  y : TSyntax;
  i : Byte;
  par : TString;
  KnownPar, HasSyntaxPar : Boolean;
Begin
  Syntax := DEFAULT_PROLOG_SYNTAX;
  HasSyntaxPar := False;
  SkipStartFile := False;
  HasUserFilePar := False;

  For i := 1 To ParamCount Do
  Begin
    par := ParamStr(i);
    If par[1] = '-' Then
    Begin
      KnownPar := False;
      Delete(par,1,1);
      { syntax parameter }
      For y := PrologIIc To Edinburgh Do
      Begin
        If StartFile[y] = par Then
        Begin
          If HasSyntaxPar Then
            RaiseError('Syntax parameter cannot be used more than once');
          Syntax := y;
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
      If HasUserFilePar Then
        RaiseError('Parameter: file already set');
      HasUserFilePar := True;
      Filename := par;
    End
  End;

  { detect syntax from file ext if no syntax parameter is set }
  If Not Error And HasUserFilePar And Not HasSyntaxPar Then
  Begin
    If EndsWith(Filename,'.pl') Then
      Syntax := Edinburgh
    Else
    For y := PrologIIc To Edinburgh Do
      If EndsWith(Filename,'.' + FileExt[y]) Then
        Syntax := y
  End
End;

{ load the startup file into the current world }
Procedure LoadStartFile( P : ProgPtr );
Var  
  y : TSyntax;
  s : StrPtr;
  os : TObjectPtr Absolute s;
Begin
  y := GetSyntax(P);
  s := Str_NewFromShortString('start/' + StartFile[y] + '.' + FileExt[y]);
  AddGCRoot(os); { protect this string from GC }
  LoadProgram(P,s,False)
End;

{ load the user file into the current world }
Procedure LoadUserFile( P : ProgPtr; Filename : StrPtr );
Begin
  SetProgramPath(P,Path_ExtractPath(Filename));
  LoadProgram(P,Filename,False)
End;

{ create the Prolog engine }
Function CreateProgram : ProgPtr;
Var 
  P : ProgPtr;
  y : TSyntax; 
  SkipStartFile : Boolean;
  HasUserFilePar : Boolean; 
  UserFilename : TShortPath;
  StrUserFilename : TPath;
  DummyOk : Boolean;
  UserWorldName : StrPtr;
Begin
  ParseCL(y,SkipStartFile,HasUserFilePar,UserFilename);
  If Error Then Exit;
  P := Prog_New(y);
  AddGCRoot(TObjectPtr(P));
  SetCurrentProgram(P);
  RegisterPredefined(P);
  { load the start file }
  If Not Error And Not SkipStartFile Then
    LoadStartFile(P);
  { create the default user world below the current world and move to it }
  UserWorldName := Str_NewFromShortString(WorldSetup[y].User);
  DummyOk := CreateNewSubWorld(P,UserWorldName,True);
  { load the user file }
  If Not Error And HasUserFilePar Then
  Begin
    StrUserFilename := Str_NewFromBytes(UserFilename,GetSystemCEncoding);
    AddGCRoot(TObjectPtr(StrUserFilename)); { protect this string from GC }
    LoadUserFile(P,StrUserFilename)
  End;
  CreateProgram := P
End;

End.