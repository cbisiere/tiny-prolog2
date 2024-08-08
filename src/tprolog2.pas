{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : tprolog2.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N  P R O G R A M                           }
{                                                                            }
{----------------------------------------------------------------------------}

{$U-} { TP: Ctrl-C does not interrupt program execution }

{$S+} { Stack checking on }
{$R+} { Range checking on }
{$V-} { No strict type checking for strings }

Program TProlog2;

Uses 
  Crt, 
  Dos,
  ShortStr,
  Num,
  Errs,
  Chars,
  Crt2,
  Readline,
  Files,
  Trace,
  Common,
  IChar,
  Buffer,
  Memory,
  PObj,
  PObjLisA,
  PObjList,
  PObjIO,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjTerm,
  PObjFCVI,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjHead,
  PObjComm,
  PObjWrld,
  PObjStmt,
  PObjProg,
  Tuple,
  Encoding,
  Unparse,
  Reduc,
  Tokenize,
  Expr,
  Parse,
  Debug,
  Predef,
  Engine,
  Init;

{ cleanup and die }
Procedure Die;
Begin
  CWriteln;
  TerminateTrace;
  HaltProgram
End;

{ check for error (or/and request to quit) and handle it }
Procedure HandleErrorIfAny( P : ProgPtr );
Var
  f : StreamPtr;
Begin
  If Error Then
  Begin
    If ErrorState = SYNTAX_ERROR Then
    Begin
      f := CurrentInput(P);
      Stream_DisplayErrorMessage(f,GetErrorMessage);
      Stream_Close(f)
    End
    Else
    Begin
      CWrite(GetErrorMessage);
      CWriteln
    End
  End;
  If QuitRequested Then
    Die;
  ResetError
End;

{ Read Evaluate Print Loop }
Procedure REPL( P : ProgPtr );
Var
  Prompt : TString;
Begin
  Repeat
    HandleErrorIfAny(P);
    ResetIO(P);
    ReleaseMemory(P);
    Case GetSyntax(P) Of
    PrologIIc:
      Prompt := 'c> ';
    PrologII:
      Prompt := '> ';
    PrologIIp:
      Prompt := '+> ';
    Edinburgh:
      Prompt := '?- ';
    End;
    CWrite(Prompt);
    ReadFromConsole(P);
    If ErrorState = USER_INTERRUPT Then
      Die;
    ProcessCommandLine(P)
  Until False
End;

Var
  P : ProgPtr;

{ main }
Begin
  P := CreateProgram;
  REPL(P)
End.
