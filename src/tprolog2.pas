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

{$U-} { TP3: Ctrl-C does not interrupt program execution }

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ TP4: set stack size to the maximum instead of the 16k default }
{$IFDEF MSDOS}
{$M 65520,0,655360 }
{$ENDIF}

Program TProlog2;

Uses 
  Crt, 
  Dos,
{$IFDEF MSDOS}
  Turbo3,
{$ENDIF}
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
  PObjIO,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjHead,
  PObjComm,
  PObjWrld,
  PObjStmt,
  PObjProg,
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

{ check for error (or/and request to quit) and handle it }
Procedure HandleErrorIfAny( P : ProgPtr );
Var
  f : StreamPtr;
Begin
  If Error Then
  Begin
    f := CurrentInput(P);
    StreamDisplayErrorMessage(f,GetErrorMessage);
    StreamClose(f)
  End;
  If QuitRequested Then
  Begin
    TerminateTrace;
    HaltProgram
  End;
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
    PrologII:
      Prompt := '> ';
    PrologIIc:
      Prompt := 'c> ';
    PrologIIp:
      Prompt := '+> ';
    Edinburgh:
      Prompt := '?- ';
    End;
    CWrite(Prompt);
    ReadFromConsole(P);
    ProcessCommandLine(P)
  Until False
End;

Var
  P : ProgPtr;

{ main }
Begin
  P := CreateProgram;
  ProcessParameters(P);
  REPL(P)
End.
