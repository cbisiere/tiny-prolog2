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

Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr );
Begin
  UnparseOneQuery(Q);
  Clock(P,Q)
End;

{----------------------------------------------------------------------------}
{ Execute all queries starting with list head Q.                             }
{----------------------------------------------------------------------------}

Procedure AnswerQueries( P : ProgPtr; Q : QueryPtr );
Begin
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q);
    Q := Q^.QU_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ Execute all queries in program P.                                          }
{----------------------------------------------------------------------------}

Procedure AnswerProgramQueries( P : ProgPtr );
Begin
  AnswerQueries(P,P^.PP_FQRY)
End;

{----------------------------------------------------------------------------}
{ Load rules and queries from a file, and execute the queries it contains,   }
{ if any.                                                                    }
{----------------------------------------------------------------------------}

Procedure LoadProgram( P : ProgPtr; FileName : AnyStr; RuleType : RuType );
Var Q : QueryPtr;
Begin
  If SetFileForInput(FileName) Then
  Begin
    Q := CompileRulesAndQueries(P,RuleType);
    if (Not Error) And (Q <> Nil) Then
      AnswerQueries(P,Q);
  End
  Else
    RaiseError('Cannot open file ' + FileName)
End;
