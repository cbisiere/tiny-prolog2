{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Dict.pas                                                   }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         S Y S T E M   C A L L S                            }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ predefined constants }
Const NbPredef = 1;
Type PredefArr = Array[1..NbPredef] Of StrConst;

Const Predef  : PredefArr = ('SYSCALL');         { Predefined constants    }

Procedure InstallPredefinedConstants;
Var
  C : ConstPtr;
  K : Integer;
Begin
  For K := 1 to NbPredef Do
    C := InstallConst(Predef[K]);
End;

{ execute a system call <SYSCALL,Code,Arg1,...ArgN> }
Function ExecutionSysCallOk; (* ( F : TermPtr; P : ProgPtr; Q : QueryPtr ) : Boolean; *)
Var
  Fail : Boolean;
  Ident : StrIdent;
  NbArgs : Integer;
  SysCallCode : StrIdent;
  NbSysCallArgs : Integer;
  C : ConstPtr;
  FF : FuncPtr Absolute F;

  Function GetConstArg( N : Integer; F : FuncPtr) : StrIdent;
  Var 
    T : TermPtr;
    CT : ConstPtr Absolute T;
  Begin
    T := Argument(N,F);
    CheckCondition(TypeOfTerm(T) = Constant,'GetConstArg: constant expected');
    GetConstArg := DictConst[CT^.TC_CONS]
  End;

Begin
  If TypeOfTerm(F) <> FuncSymbol Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  SysCallCode := GetConstArg(1,FF);
  CheckCondition(SysCallCode = 'SYSCALL','Not a SYSCALL');
  NbArgs := NbArguments(FF);
  If NbArgs < 2 Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  Ident := GetConstArg(2,FF);
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
      C := EvaluateToConstant(Argument(2+1,FF));
      Fail := C = Nil;
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
      WriteTerm(Argument(2+1,FF))
  Else If Ident = 'OUTM' Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
      WriteTermBis(Argument(2+1,FF),False,False)
  Else If Ident = 'LINE' Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      WriteLn
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
