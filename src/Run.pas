{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Run.pas                                                    }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022                                                       }
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

Procedure AnswerQuery( P,Q : Integer );
Var Stopper : Integer;
Begin
  UnparseOneQuery(Q);
  Stopper := PtrRight;
  Clock(P,Q,Stopper)
End;

{----------------------------------------------------------------------------}
{ Execute all quesries starting with list head Q.                            }
{----------------------------------------------------------------------------}

Procedure AnswerQueries( P,Q : Integer );
Begin
  While Q <> NULL Do
  Begin
    AnswerQuery(P,Q);
    Q := Memory[Q+QU_NEXT]
  End
End;

{----------------------------------------------------------------------------}
{ Execute all queries in program P.                                          }
{----------------------------------------------------------------------------}

Procedure AnswerProgramQueries( P : Integer );
Begin
  AnswerQueries(P,Memory[P+PP_FQRY])
End;

{----------------------------------------------------------------------------}
{ Load rules and queries from a file, and execute the queries it contains,   }
{ if any.                                                                    }
{----------------------------------------------------------------------------}

Procedure LoadProgram( P : Integer; FileName : AnyStr; RuleType : Integer );
Var Q : Integer;
Begin
  If SetFileForInput(FileName) Then
  Begin
    Q := CompileRulesAndQueries(P,RuleType);
    if (Not Error) And (Q <> NULL) Then
      AnswerQueries(P,Q);
  End
  Else
    RaiseError('Cannot open file ' + FileName)
End;
