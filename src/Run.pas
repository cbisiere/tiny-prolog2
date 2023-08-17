{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Run.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{       L O A D   R U L E S   A N D   E X E C U T E   Q U E R I E S          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }


{ initialize the input/output system }
Procedure InitIO;
Begin
  InitIFileStack;
  InitOFileStack
End;

{ reset the input/output system, closing all open files, but preserving the
 input console buffer }
Procedure ResetIO;
Begin
  ResetIFileStack;
  ResetOFileStack
End;

{ execute query Q }
Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  If Echo Then
    OutOneQuery(Q,False);
  Clock(P,Q);
  ResetIO
End;

{ execute queries starting at the current insertion level in P }
Procedure AnswerQueries( P : ProgPtr; Echo : Boolean );
Var
  Q : QueryPtr;
Begin
  Q := FirstQueryToExecute(P);
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q,Echo);
    Q := NextQuery(Q)
  End;
  RemoveQueries(P)
End;

{ load rules and queries from a file, and execute the queries it contains,
  if any }
Procedure LoadProgram( P : ProgPtr; s : StrPtr );
Var 
  Filename : TString;
Begin
  If StrLength(s) <= StringMaxSize Then
  Begin
    FileName := StrGetString(s);
    If SetFileForInput(FileName) Then
    Begin
      BeginInsertion(P);
      ParseRulesAndQueries(P,GetRuleType(P));
      If Error Then Exit;
      CloseCurrentInput;
      If Error Then Exit;
      { clearing goals only after closing the input file is the right way to  
        do it, as calls to input_is, etc. must not consider the program file 
        as an input file }
      AnswerQueries(P,GetRuleType(P)=RTYPE_USER);
      EndInsertion(P)
    End
    Else
      RuntimeError('Cannot open file ')
  End
  Else
    RuntimeError('filename is too long');
End;
