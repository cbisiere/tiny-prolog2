{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Run.pas                                                    }
{   Author      : Christophe Bisière                                         }
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

{----------------------------------------------------------------------------}
{ Execute query Q.                                                           }
{----------------------------------------------------------------------------}

Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  If Echo Then
    OutOneQuery(Q);
  Clock(P,Q)
End;

{----------------------------------------------------------------------------}
{ Execute all queries starting with list head Q.                             }
{----------------------------------------------------------------------------}

Procedure AnswerQueries( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q,Echo);
    Q := Q^.QU_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ Load rules and queries from a file, and execute the queries it contains,   }
{ if any.                                                                    }
{----------------------------------------------------------------------------}

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
      if (Not Error) And (Q <> Nil) Then
        AnswerQueries(P,Q,True);
    End
    Else
      RaiseError('Cannot open file ')
  End
  Else
    RaiseError('filename is too long');
End;
