{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dict.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         S Y S T E M   C A L L S                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{----------------------------------------------------------------------------}
{ Execute a system call <SYSCALL,Code,Arg1,...ArgN>.                         }
{----------------------------------------------------------------------------}

Function ExecutionSysCallOk; (* ( F, P, Q : Integer ) : Boolean; *)
Var
  Fail : Boolean;
  Ident : StrIdent;
  NbArgs : Integer;
  SysCallCode : StrIdent;
  NbSysCallArgs : Integer;
  C : Integer;

  Function GetConstArg( N: Integer; F : Integer) : StrIdent;
  Begin
    GetConstArg := DictConst[Memory[Argument(N,F)+TC_CONS]]
  End;

Begin
  If TypeOfTerm(F) <> FuncSymbol Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  SysCallCode := GetConstArg(1,F);
  CheckCondition(SysCallCode = 'SYSCALL','Not a SYSCALL');
  NbArgs := NbArguments(F);
  If NbArgs < 2 Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  Ident := GetConstArg(2,F);
  NbSysCallArgs := NbArgs - 2;
  ExecutionSysCallOk := True;
  If Ident = 'QUIT' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      Halt
  Else If Ident = 'INSERT' Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
    Begin
      C := EvaluateToConstant(Argument(2+1,F));
      Fail := C = NULL;
      If Not Fail Then
      Begin
        LoadProgram(P,GetConstAsString(C,False),RTYPE_USER);
        Fail := Error
      End;
      ExecutionSysCallOk := Not Fail
    End
  Else If Ident = 'LIST' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      UnparseQuestionRules(Q,RTYPE_USER)
  Else If Ident = 'OUT' Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
      WriteTerm(Argument(2+1,F))
  Else If Ident = 'OUTM' Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
      WriteTermBis(Argument(2+1,F),False,False)
  Else If Ident = 'LINE' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      Writeln
  Else If Ident = 'BACKTRACE' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      DumpBacktrace
  Else If Ident = 'CLRSRC' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      ClrScr
End;
