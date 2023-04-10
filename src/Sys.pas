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

{ install all predefined constants }
Procedure RegisterPredefinedConstants( P : ProgPtr );
Var DC : DictConstPtr;
Begin
  DC := LookupConst(P^.PP_DCON,NewStringFrom('SYSCALL'),Identifier)
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
    GetConstArg := CT^.TC_DCON^.DC_CVAL
  End;

Begin
  If TypeOfTerm(F) <> FuncSymbol Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  SysCallCode := GetConstArg(1,FF);
  CheckCondition(StrEqualTo(SysCallCode,'SYSCALL'),'Not a SYSCALL');
  NbArgs := NbArguments(FF);
  If NbArgs < 2 Then
  Begin
    ExecutionSysCallOk := False;
    Exit
  End;
  Ident := GetConstArg(2,FF);
  NbSysCallArgs := NbArgs - 2;
  ExecutionSysCallOk := True;
  If StrEqualTo(Ident,'QUIT') Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      Halt
  Else If StrEqualTo(Ident,'INSERT') Then
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
  Else If StrEqualTo(Ident,'LIST') Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      OutQuestionRules(Q,RTYPE_USER)
  Else If StrEqualTo(Ident,'OUT') Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
      OutTerm(Argument(2+1,FF))
  Else If StrEqualTo(Ident,'OUTM') Then
    If NbSysCallArgs <> 1 Then
      ExecutionSysCallOk := False
    Else
      OutTermBis(Argument(2+1,FF),False,False)
  Else If StrEqualTo(Ident,'LINE') Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      WriteLn
  Else If StrEqualTo(Ident,'BACKTRACE') Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      DumpBacktrace
  Else If StrEqualTo(Ident,'CLRSRC') Then
    If NbSysCallArgs <> 0 Then
      ExecutionSysCallOk := False
    Else
      ClrScr
End;
