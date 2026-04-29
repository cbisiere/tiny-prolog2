{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : tprolog2.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                           M A I N  P R O G R A M                           }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{$IFDEF TPC}
{$M 65520,0,589840 } { stack (max 65520), heap low, heap high (def 640k) }
{$ELSE}
{$M 4194304,0} { 4 MiB }
{$ENDIF}

Program TProlog2;

Uses 
  Crt,
  Dos,
  ShortStr,
  Num,
  DateTime,
  Errs,
  Chars,
  Files,
  Dump,
  CrtSize,
  Crt2,
  CEdit,
  CLI,
  Paper,
  Echo,
  CWrites,
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
  Dumper,
  Predef,
  Engine,
  Init;

{ cleanup and die }
Procedure Die( P : ProgPtr );
Begin
  CWriteLn;
  { turn off echo }
  SetEcho(P,False);
  { turn off debug }
  SetDebug(P,False);
  { shut down the paper system }
  SetPaper(P,False);
  ClosePaperFile;
  { close the dump file }
  CloseDumpFile;
  { do die }
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
      Stream_CloseFile(f)
    End
    Else
    Begin
      If Not CrtIsCursorOnFirstCol Then
        CWriteLn;
      CWrite(GetErrorMessage);
      CWriteLn
    End
  End;
  If QuitRequested Then
    Die(P);
  ResetError
End;

{ Read Evaluate Print Loop }
Procedure REPL( P : ProgPtr );
Begin
  Repeat
    { cleanup }
    HandleErrorIfAny(P);
    ResetIO(P);
    ReleaseMemory(P);
    { get input from CLI }
    DisplayPrompt(P);
    ReadFromConsole(P);
    { process input }
    If ErrorState = USER_INTERRUPT Then
    Begin
      CWrite('Bye!');
      Die(P);
    End;
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
