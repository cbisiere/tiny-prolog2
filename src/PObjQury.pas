{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjQury.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                   P R O L O G   O B J E C T :   Q U E R Y                  }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ Prolog query and list of queries }

Unit PObjQury;

Interface

Uses
  Errs,
  Memory,
  PObj,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef;

Function Query_New( level : TILevel; y : TSyntax ) : QueryPtr;

Function Query_GetSyntax( Q : QueryPtr ) : TSyntax;
Function Query_GetTerms( Q : QueryPtr ) : BTermPtr;
Procedure Query_SetTerms( Q : QueryPtr; B : BTermPtr );
Function Query_GetSys( Q : QueryPtr ) : EqPtr;
Procedure Query_SetSys( Q : QueryPtr; S : EqPtr );
Function Query_GetDict( Q : QueryPtr ) : DictPtr;
Procedure Query_SetDict( Q : QueryPtr; D : DictPtr );
Function Query_GetHead( Q : QueryPtr ) : HeadPtr;

Function Queries_GetNext( Q : QueryPtr ) : QueryPtr;
Procedure Queries_SetNext( Q,N : QueryPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new query w/ given file insertion level }
Function Query_New( level : TILevel; y : TSyntax ) : QueryPtr;
Var 
  Q : QueryPtr;
  ptr : TObjectPtr Absolute Q;
Begin
  ptr := NewRegisteredPObject(QU,SizeOf(TObjQuery),5,True,3);
  With Q^ Do
  Begin
    QU_NEXT := Nil;
    QU_FBTR := Nil;
    QU_SYST := Nil;
    QU_DVAR := Nil;
    QU_HEAD := Nil;
    QU_LEVL := level;
    QU_SYNT := y
  End;
  Query_New := Q
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ get query's syntax }
Function Query_GetSyntax( Q : QueryPtr ) : TSyntax;
Begin
  CheckCondition(Q <> Nil,'Query_GetSyntax: Nil');
  Query_GetSyntax := Q^.QU_SYNT
End;

{ get query's terms }
Function Query_GetTerms( Q : QueryPtr ) : BTermPtr;
Begin
  CheckCondition(Q <> Nil,'Query_GetTerms: Nil');
  Query_GetTerms := Q^.QU_FBTR
End;

{ set query's terms }
Procedure Query_SetTerms( Q : QueryPtr; B : BTermPtr );
Begin
  CheckCondition(Q <> Nil,'Query_SetTerms: Nil');
  Q^.QU_FBTR := B
End;

{ system of equations }
Function Query_GetSys( Q : QueryPtr ) : EqPtr;
Begin
  CheckCondition(Q <> Nil,'Query_GetSys: Nil');
  Query_GetSys := Q^.QU_SYST
End;

{ set query's system of equations }
Procedure Query_SetSys( Q : QueryPtr; S : EqPtr );
Begin
  CheckCondition(Q <> Nil,'Query_SetSys: Nil');
  Q^.QU_SYST := S
End;

{ dictionary of local variables }
Function Query_GetDict( Q : QueryPtr ) : DictPtr;
Begin
  CheckCondition(Q <> Nil,'Query_GetDict: Nil');
  Query_GetDict := Q^.QU_DVAR
End;

{ set dictionary of local variables }
Procedure Query_SetDict( Q : QueryPtr; D : DictPtr );
Begin
  CheckCondition(Q <> Nil,'Query_SetDict: Nil');
  Q^.QU_DVAR := D
End;

{ clock head }
Function Query_GetHead( Q : QueryPtr ) : HeadPtr;
Begin
  CheckCondition(Q <> Nil,'Query_GetHead: Nil');
  Query_GetHead := Q^.QU_HEAD
End;

{-----------------------------------------------------------------------}
{ list of queries                                                       }
{-----------------------------------------------------------------------}

{ next query }
Function Queries_GetNext( Q : QueryPtr ) : QueryPtr;
Begin
  CheckCondition(Q <> Nil,'Queries_GetNext: Nil');
  Queries_GetNext := Q^.QU_NEXT
End;

{ set next query }
Procedure Queries_SetNext( Q,N : QueryPtr );
Begin
  CheckCondition(Q <> Nil,'Queries_SetNext: Nil');
  Q^.QU_NEXT := N
End;

End.