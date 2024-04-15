{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjStmt.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{              P R O L O G   O B J E C T  :   S T A T E M E N T              }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ statement and list of statements }

Unit PObjStmt;

Interface

Uses
  Memory,
  PObj,
  PObjStr,
  PObjDef,
  PObjComm;

Type
  SetOfTStmt = Set Of TStmt;

Function Statement_New( ty : TStmt; O : TObjectPtr ) : StmtPtr;

Function Statement_GetType( S : StmtPtr ) : TStmt;
Function Statement_GetObject( S : StmtPtr ) : TObjectPtr;
Function Statement_GetWorld( S : StmtPtr ) : WorldPtr;
Procedure Statement_SetWorld( S : StmtPtr ; W : WorldPtr );
Function Statement_GetPrev( S : StmtPtr ) : StmtPtr;
Function Statement_GetNext( S : StmtPtr ) : StmtPtr;

Function Statement_FindPrevOfType( S : StmtPtr; types : SetOfTStmt ) : StmtPtr;
Function Statement_FindNextOfType( S : StmtPtr; types : SetOfTStmt ) : StmtPtr;

Procedure Statement_ChainWith( S1,S2 : StmtPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new statement pointing to object O }
Function Statement_New( ty : TStmt; O : TObjectPtr ) : StmtPtr;
Var 
  S : StmtPtr;
  ptr : TObjectPtr Absolute S;
Begin
  ptr := NewRegisteredPObject(SM,SizeOf(TObjStmt),4,True,4);
  With S^ Do
  Begin
    SM_PREV := Nil;
    SM_NEXT := Nil;
    SM_OBJC := O;
    SM_WRLD := Nil;
    SM_TYPE := ty
  End;
  Statement_New := S
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ type of statement }
Function Statement_GetType( S : StmtPtr ) : TStmt;
Begin
  Statement_GetType := S^.SM_TYPE
End;

{ statement's attached object or Nil }
Function Statement_GetObject( S : StmtPtr ) : TObjectPtr;
Begin
  Statement_GetObject := S^.SM_OBJC
End;

{ statement's world }
Function Statement_GetWorld( S : StmtPtr ) : WorldPtr;
Begin
  Statement_GetWorld := S^.SM_WRLD
End;

{ set the world the statement belongs to }
Procedure Statement_SetWorld( S : StmtPtr ; W : WorldPtr );
Begin
  S^.SM_WRLD := W
End;

{ previous statement }
Function Statement_GetPrev( S : StmtPtr ) : StmtPtr;
Begin
  Statement_GetPrev := S^.SM_PREV
End;

{ next statement }
Function Statement_GetNext( S : StmtPtr ) : StmtPtr;
Begin
  Statement_GetNext := S^.SM_NEXT
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ previous statement of a given type, or Nil }
Function Statement_FindPrevOfType( S : StmtPtr; types : SetOfTStmt ) : StmtPtr;
Begin
  S := Statement_GetPrev(S);
  While (S <> Nil) And Not (Statement_GetType(S) In types) Do
    S := Statement_GetPrev(S);
  Statement_FindPrevOfType := S
End;

{ next statement in a given set of types or Nil }
Function Statement_FindNextOfType( S : StmtPtr; types : SetOfTStmt ) : StmtPtr;
Begin
  S := Statement_GetNext(S);
  While (S <> Nil) And Not (Statement_GetType(S) In types) Do
    S := Statement_GetNext(S);
  Statement_FindNextOfType := S
End;

{ link two statements: S1 --> S2 }
Procedure Statement_ChainWith( S1,S2 : StmtPtr );
Begin
  S1^.SM_NEXT := S2;
  S2^.SM_PREV := S1
End;

End.