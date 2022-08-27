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

Function ExecutionSysCallOk( F : Integer; Q : Integer ) : Boolean;
Var
  Fail : Boolean;
  Ident : StrIdent;
  NbArgs : Integer;
  SysCallCode : StrIdent;
  NbSysCallArgs : Integer;

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
  Else If Ident = 'LIST' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      UnparseQuestionRules(Q,RTYPE_USER)
  Else If Ident = 'OUTL' Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
    Begin
      WriteTerm(Argument(2+1,F));
      Writeln
    End
End;
