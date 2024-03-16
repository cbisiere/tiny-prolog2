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
  OStream,
  OStack,
  Common,
  IChar,
  Buffer,
  Memory,
  PObj,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjProg,
  IStream,
  IStack,
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
Procedure HandleErrorIfAny;
Begin
  If Error Then
  Begin
    DisplayInputErrorMessage(GetErrorMessage);
    CloseCurrentInput
  End;
  If QuitRequested Then
  Begin
    TerminateTrace;
    HaltProgram
  End;
  ResetError
End;

{ compile the user program and solve each query }
Procedure Main;
Var
  P : ProgPtr;
  Prompt : TString;
Begin
  P := CreateProgram;
  ProcessParameters(P);
  Repeat
    HandleErrorIfAny;
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
    ReadFromConsole;
    While ParseCommandLineQuery(P) And Not Error Do
      AnswerQueries(P,False)
  Until False
End;

{ main }
Begin
  Main
End.
