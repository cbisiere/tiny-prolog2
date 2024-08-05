{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjWrld.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  P R O L O G   O B J E C T :   W O R L D                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ invariant:  current statement cannot be the Start statement }

Unit PObjWrld;

Interface

Uses
  Errs,
  Memory,
  PObj,
  PObjStr,
  PObjDef,
  PObjStmt;

Function World_New( Name : StrPtr; UserLand : Boolean ) : WorldPtr;

Function World_GetName( W : WorldPtr ) : StrPtr;
Function World_GetParent( W : WorldPtr ) : WorldPtr;
Function World_GetFirstChild( W : WorldPtr ) : WorldPtr;
Function World_GetFirstStatement( W : WorldPtr ) : StmtPtr;
Procedure World_SetFirstStatement( W : WorldPtr; S : StmtPtr );
Function World_GetLastStatement( W : WorldPtr ) : StmtPtr;
Procedure World_SetLastStatement( W : WorldPtr; S : StmtPtr );
Function World_GetCurrentStatement( W : WorldPtr ) : StmtPtr;
Procedure World_SetCurrentStatement( W : WorldPtr; S : StmtPtr );

Procedure World_AppendChild( W,Wc : WorldPtr );
Function World_FindChildByName( W : WorldPtr; Name : StrPtr ) : WorldPtr;
Procedure World_SuppressChild( W,Wc : WorldPtr );
Procedure World_InsertStatement( W : WorldPtr; St : StmtPtr );
Procedure World_SuppressCurrentStatement( W : WorldPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new world with name Name; an empty world has two statements: 
 a start and an end; end is the current statement }
Function World_New( Name : StrPtr; UserLand : Boolean ) : WorldPtr;
Var 
  W : WorldPtr;
  ptr : TObjectPtr Absolute W;
Begin
  ptr := NewRegisteredPObject(WO,SizeOf(TObjWorld),8,True,8);
  With W^ Do
  Begin
    WO_NAME := Name;
    WO_FSTA := Statement_New(StatementStart,Nil);;
    WO_LSTA := Statement_New(StatementEnd,Nil);
    WO_CSTA := WO_LSTA;
    WO_WPAR := Nil;
    WO_WFCH := Nil;
    WO_WPRV := Nil;
    WO_WNXT := Nil;
    WO_USER := UserLand;
    Statement_ChainWith(WO_FSTA,WO_LSTA);
    Statement_SetWorld(WO_FSTA,W);
    Statement_SetWorld(WO_LSTA,W);
  End;
  World_New := W
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ name of the world }
Function World_GetName( W : WorldPtr ) : StrPtr;
Begin
  World_GetName := W^.WO_NAME
End;

{ next sibling }
Function World_GetNext( W : WorldPtr ) : WorldPtr;
Begin
  World_GetNext := W^.WO_WNXT
End;

{ previous sibling }
Function World_GetPrev( W : WorldPtr ) : WorldPtr;
Begin
  World_GetPrev := W^.WO_WPRV
End;

{ parent of the world }
Function World_GetParent( W : WorldPtr ) : WorldPtr;
Begin
  World_GetParent := W^.WO_WPAR
End;

{ first child of the world }
Function World_GetFirstChild( W : WorldPtr ) : WorldPtr;
Begin
  World_GetFirstChild := W^.WO_WFCH
End;

{ world's first statement }
Function World_GetFirstStatement( W : WorldPtr ) : StmtPtr;
Begin
  World_GetFirstStatement := W^.WO_FSTA
End;

{ set world's first statement }
Procedure World_SetFirstStatement( W : WorldPtr; S : StmtPtr );
Begin
  W^.WO_FSTA := S
End;

{ world's last statement }
Function World_GetLastStatement( W : WorldPtr ) : StmtPtr;
Begin
  World_GetLastStatement := W^.WO_LSTA
End;

{ set world's last statement }
Procedure World_SetLastStatement( W : WorldPtr; S : StmtPtr );
Begin
  W^.WO_LSTA := S
End;

{ world's current statement }
Function World_GetCurrentStatement( W : WorldPtr ) : StmtPtr;
Begin
  World_GetCurrentStatement := W^.WO_CSTA
End;

{ set world's last statement }
Procedure World_SetCurrentStatement( W : WorldPtr; S : StmtPtr );
Begin
  W^.WO_CSTA := S
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ return the first child of W with name Name of Nil if no children has this 
 name }
Function World_FindChildByName( W : WorldPtr; Name : StrPtr ) : WorldPtr;
Var
  Wi,Wn : WorldPtr;
Begin
  Wn := Nil;
  Wi := World_GetFirstChild(W);
  While (Wi <> Nil) And (Wn = Nil) Do
  Begin
    If Str_Equal(Wi^.WO_NAME,Name) Then { TODO: check case insensitive? }
      Wn := Wi;
    Wi := World_GetNext(Wi)
  End;
  World_FindChildByName := Wn
End;

{ last child or Nil if no child }
Function World_GetLastChild( W : WorldPtr ) : WorldPtr;
Var
  Wi,Wl : WorldPtr;
Begin
  Wl := Nil;
  Wi := World_GetFirstChild(W);
  While Wi <> Nil Do
  Begin
    Wl := Wi;
    Wi := World_GetNext(Wi)
  End;
  World_GetLastChild := Wl
End;

{ add Wc as the last child of world W }
Procedure World_AppendChild( W,Wc : WorldPtr );
Var
  Wl : WorldPtr;
Begin
  Wc^.WO_WPAR := W; { set child world's parent }
  Wl := World_GetLastChild(W);
  If Wl = Nil Then { Wc is the first and only child }
    W^.WO_WFCH := Wc
  Else
  Begin { append the child }
    Wl^.WO_WNXT := Wc;
    Wc^.WO_WPRV := Wl
  End
End;

{ suppress child Wc of world W, and all the subworlds of Wc }
Procedure World_SuppressChild( W,Wc : WorldPtr );
Begin
  If Wc = World_GetFirstChild(W) Then
  Begin
    Wc := World_GetNext(Wc);
    W^.WO_WFCH := Wc;
    If Wc <> Nil Then
      Wc^.WO_WPRV := Nil
  End
  Else If Wc = World_GetLastChild(W) Then
  Begin
    Wc := World_GetPrev(Wc);
    Wc^.WO_WNXT := Nil
  End
  Else
  Begin
    Wc^.WO_WPRV^.WO_WNXT := Wc^.WO_WNXT;
    Wc^.WO_WNXT^.WO_WPRV := Wc^.WO_WPRV
  End
End;

{ insert a statement S before the current statement of world W }
Procedure World_InsertStatement( W : WorldPtr; St : StmtPtr );
Var
  Sc : StmtPtr;
Begin
  Sc := World_GetCurrentStatement(W);
  Statement_ChainWith(Statement_GetPrev(Sc),St);
  Statement_ChainWith(St,Sc);
  Statement_SetWorld(St,W)
End;

{ suppress the current statement of world W; Start and End cannot be deleted }
Procedure World_SuppressCurrentStatement( W : WorldPtr );
Var
  Sc,Sn : StmtPtr;
Begin
  Sc := World_GetCurrentStatement(W);
  CheckCondition(Not (Statement_GetType(Sc) In [StatementStart,StatementEnd]),
      'World_SuppressCurrentStatement: cannot delete start or end statement');
  Sn := Statement_GetNext(Sc);
  Statement_Suppress(Sc);
  World_SetCurrentStatement(W,Sn)
End;

End.