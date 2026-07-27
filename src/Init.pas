{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Init.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                     I N I T I A L I S A T I O N S                          }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Init;

Interface

Uses 
  ShortStr,
  Errs,
  Chars,
  Num,
  Files,
  Paper,
  Memory,
  PObjStr,
  PObjDef,
  PObjProg,
  PObjIO,
  Dumper,
  Expr,
  Predef,
  Engine;

Function CreateProgram : ProgPtr;

Implementation
{-----------------------------------------------------------------------------}

{ syntax switches; each switch is also the Filename part of the start program }
Type 
  TSyntaxPar = Array[TSyntax] Of String[5];
Const 
  SyntaxPar : TSyntaxPar = ('PIIv1','PII','PIIp','E'); { must be ASCII only }
  PARA_SYNTAX = 's';
  PARA_INSERT = 'f';
  PARA_CODEPAGE = 'c';
  PARA_RESTORE = 'r';
  PARA_DEBUG = 'd';

{ name of the paper file }
Type 
  TPaperFile = Array[TSyntax] Of String[15];
Const  
  PaperFile : TPaperFile = (
    'imprimante.text',
    'printer.txt', { FIXME: just a guess }
    'prolog.log',
    'prolog.log'
  );

Const
  DEFAULT_PROLOG_SYNTAX : TSyntax = PrologIIv2;


{ parse the command line parameters }
Procedure ParseCL( 
    Var LoadSavedState : Boolean; 
    Var CodePage : TCodePage; 
    Var Syntax : TSyntax; 
    Var SkipStartFile : Boolean; 
    Var HasUserFilePar : Boolean; Var Filename : TShortPath );
Var  
  code : Integer; { string to number result code }
  y : TSyntax;
  i : Byte;
  V : TString;
  HasSyntaxPar : Boolean;
  par : TString;

  { return the value of parameter -par; i is the current index in ParamStr, and
   is increased by 1 if a value is found; 
   raise an error and return '' if no value can be found }
  Function GetParValue( par : TString; Var i : Byte ) : TString;
  Begin
    GetParValue := '';
    If (i = ParamCount) Or (ParamStr(i+1)[1] = '-') Then
    Begin
      ParameterError(par,'value expected');
      Exit;
    End;
    i := i + 1;
    GetParValue := ParamStr(i)
  End;

Begin
  CodePage := 0;
  Syntax := DEFAULT_PROLOG_SYNTAX;
  HasSyntaxPar := False;
  LoadSavedState := False;
  SkipStartFile := False;
  HasUserFilePar := False;

  i := 0;
  While i < ParamCount Do
  Begin
    i := i + 1;
    par := ParamStr(i);
    { syntax, e.g. '-s PIIv1' }
    If par = '-' + PARA_SYNTAX Then
    Begin
      If HasSyntaxPar Then
      Begin
        ParameterError(PARA_SYNTAX,'can only be used once');
        Exit
      End;
      V := GetParValue(PARA_SYNTAX,i);
      If Error Then Exit;
      For y := PrologIIv1 To Edinburgh Do
      Begin
        If SyntaxPar[y] = V Then
        Begin
          Syntax := y;
          HasSyntaxPar := True
        End
      End;
      If Not HasSyntaxPar Then
      Begin
        ParameterError(PARA_SYNTAX,'incorrect value: ' + V);
        Exit
      End
    End
    { insert user file, e.g. '-i "./tests/file.pro" }
    Else If par = '-' + PARA_INSERT Then
    Begin
      If HasUserFilePar Then
      Begin
        ParameterError(PARA_INSERT,'can only be used once');
        Exit
      End;
      Filename := GetParValue(PARA_INSERT,i);
      If Error Then Exit;
      HasUserFilePar := True
    End
    { debug, '-r': restore the last saved state }
    Else If par = '-' + PARA_RESTORE Then
    Begin
      If LoadSavedState Then
      Begin
        ParameterError(PARA_RESTORE,'can only be used once');
        Exit
      End;
      LoadSavedState := True
    End
    { debug, '-d': do not load the start file }
    Else If par = '-' + PARA_DEBUG Then
    Begin
      If SkipStartFile Then
      Begin
        ParameterError(PARA_DEBUG,'can only be used once');
        Exit
      End;
      SkipStartFile := True
    End
    { codepage, e.g. '-c 850'; 0 means not set }
    Else If par = '-' + PARA_CODEPAGE Then
    Begin
      If CodePage <> 0 Then
      Begin
        ParameterError(PARA_CODEPAGE,'cannot be set more than once');
        Exit
      End;
      V := GetParValue(PARA_CODEPAGE,i);      
      If Error Then Exit;
      CodePage := ShortStringToPosInt(V,code);
      If code <> 0 Then
      Begin
        ParameterError(PARA_CODEPAGE,'invalid codepage: ''' + V + '''');
        Exit
      End;
    End
    Else
    Begin
      If (par[1] = '-') And (Length(Par) > 1) Then
      Begin
        Delete(par,1,1);
        ParameterError(par,'unknown parameter')
      End
      Else
        CommandLineError('unexpected value: ''' + par + '''');
      Exit
    End
  End;

  { detect syntax from file ext if no syntax parameter is set }
  If Not Error And HasUserFilePar And Not HasSyntaxPar Then
  Begin
    If EndsWith(Filename,'.pl') Then
      Syntax := Edinburgh
    Else
    For y := PrologIIv1 To Edinburgh Do
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
  s := Str_NewFromShortString('start/' + 'system' + '.' + FileExt[y]);
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
  CodePage : TCodePage;
  y : TSyntax;
  SkipStartFile : Boolean;
  LoadSavedState : Boolean;
  HasUserFilePar : Boolean; 
  UserFilename : TShortPath;
  StrUserFilename : TPath;
  StrSavedStateFilename : TPath;
  DummyOk : Boolean;
  UserWorldName : StrPtr;
Begin
  ParseCL(LoadSavedState,CodePage,y,SkipStartFile,HasUserFilePar,UserFilename);
  If CodePage <> 0 Then
    SetCodePage(CodePage);
  SetPaperFilename(PaperFile[y]);
  P := Prog_New(y);
  AddGCRoot(TObjectPtr(P));
  SetCurrentProgram(P);
  RegisterPredefinedIdentifiers(P);
  RegisterEvaluableFunctions(P);
  RegisterOperators(P);
  { load the system start file }
  If Not Error And Not SkipStartFile Then
    LoadStartFile(P);
  { create the default user world below the current world and move to it }
  UserWorldName := Str_NewFromShortString(WorldSetup[y].User);
  DummyOk := CreateNewSubWorld(P,Nil,UserWorldName,True);
  { restore the saved state }
  If Not Error And LoadSavedState Then
  Begin
    StrSavedStateFilename := Str_NewFromShortString('start/saved.' + FileExt[y]);
    AddGCRoot(TObjectPtr(StrSavedStateFilename)); { protect this string from GC }
    LoadUserFile(P,StrSavedStateFilename)
  End;
  { load the user file }
  If Not Error And HasUserFilePar Then
  Begin
    StrUserFilename := Str_NewFromBytes(UserFilename,
        GetSystemEncoding,GetSystemEolStyle);
    AddGCRoot(TObjectPtr(StrUserFilename)); { protect this string from GC }
    LoadUserFile(P,StrUserFilename)
  End;
  CreateProgram := P
End;

End.