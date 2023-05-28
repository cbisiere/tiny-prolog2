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

{ reset the input/output system, closing all open files }
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
  InitIO;
  Clock(P,Q);
  ResetIO
End;

{ execute all queries starting with list head Q }
Procedure AnswerQueries( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q,Echo);
    Q := NextQuery(Q)
  End
End;

{ load rules and queries from a file, and execute the queries it contains,
  if any }
Procedure LoadProgram( P : ProgPtr; s : StrPtr; RuleType : RuType );
Var 
  Filename : AnyStr;
  Q : QueryPtr;
Begin
  If StrLength(s)<=AnyStrMaxSize Then
  Begin
    FileName := StrGetString(s);
    If SetFileForInput(FileName) Then
    Begin
      Q := CompileRulesAndQueries(P,RuleType);
      If Not Error Then
        CloseCurrentInput;
      { clearing goals only after closing the input file is the right way to  
        do it, as calls to input_is, etc. must not consider the program file 
        as an input file }
      If (Not Error) And (Q <> Nil) Then
        AnswerQueries(P,Q,True);
    End
    Else
      RaiseError('Cannot open file ')
  End
  Else
    RaiseError('filename is too long');
End;
