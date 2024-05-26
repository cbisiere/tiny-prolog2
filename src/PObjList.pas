{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjList.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{          D O U B L E - L I N K E D   L I S T   O F   O B J E C T S         }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjList;

Interface

Uses
  Errs,
  Memory,
  PObj;

{ double-linked list of Prolog objects }
Type 
  ListPtr = ^TObjList;
  TObjList = Record
    PO_META : TObjMeta;
    { deep copied: }
    LL_PREV : ListPtr;
    LL_NEXT : ListPtr;
    LL_POBJ : TObjectPtr
  End;

Function List_New( Obj : TObjectPtr ) : ListPtr;
Function List_ShallowCopy( L : ListPtr ) : ListPtr;

Function List_GetNext( L : ListPtr ) : ListPtr;
Procedure List_SetNext( L : ListPtr; L1 : ListPtr );
Function List_Last( L : ListPtr ) : ListPtr;
Function List_GetPrev( L : ListPtr ) : ListPtr;
Procedure List_SetPrev( L : ListPtr; L1 : ListPtr );
Function List_GetObject( L : ListPtr ) : TObjectPtr;
Procedure List_SetObject( L : ListPtr; Obj : TObjectPtr );

Function List_Next( L : ListPtr; Backward : Boolean ) : ListPtr;
Function List_ShallowCopyAll( L : ListPtr ) : ListPtr;
Procedure List_Chain( L1,L2 : ListPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors / copy / compare                                         }
{-----------------------------------------------------------------------}

{ new list element }
Function List_New( Obj : TObjectPtr ) : ListPtr;
Var 
  L : ListPtr;
  ptr : TObjectPtr Absolute L;
Begin
  ptr := NewRegisteredPObject(LL,SizeOf(TObjList),3,True,3);
  With L^ Do
  Begin
    LL_PREV := Nil;
    LL_NEXT := Nil;
    LL_POBJ := Obj
  End;
  List_New := L
End;

{ shallow copy of a list element (without copying the Prolog object) }
Function List_ShallowCopy( L : ListPtr ) : ListPtr;
Begin
  List_ShallowCopy := List_New(List_GetObject(L))
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

Function List_GetNext( L : ListPtr ) : ListPtr;
Begin
  CheckCondition(L <> Nil,'List_GetNext: Nil');
  List_GetNext := L^.LL_NEXT
End;

Procedure List_SetNext( L : ListPtr; L1 : ListPtr );
Begin
  CheckCondition(L <> Nil,'List_SetNext: Nil');
  L^.LL_NEXT := L1
End;

Function List_GetPrev( L : ListPtr ) : ListPtr;
Begin
  CheckCondition(L <> Nil,'List_GetPrev: Nil');
  List_GetPrev := L^.LL_PREV
End;

Procedure List_SetPrev( L : ListPtr; L1 : ListPtr );
Begin
  CheckCondition(L <> Nil,'List_SetPrev: Nil');
  L^.LL_PREV := L1
End;

Function List_GetObject( L : ListPtr ) : TObjectPtr;
Begin
  CheckCondition(L <> Nil,'List_GetObject: Nil');
  List_GetObject := L^.LL_POBJ
End;

Procedure List_SetObject( L : ListPtr; Obj : TObjectPtr );
Begin
  CheckCondition(L <> Nil,'List_SetObject: Nil');
  L^.LL_POBJ := Obj
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

Function List_Last( L : ListPtr ) : ListPtr;
Begin
  CheckCondition(L <> Nil,'List_Last: Nil');
  While List_GetNext(L) <> Nil Do
    L := List_GetNext(L);
  List_Last := L
End;

Function List_Next( L : ListPtr; Backward : Boolean ) : ListPtr;
Begin
  CheckCondition(L <> Nil,'List_Next: Nil');
  If Backward Then
    List_Next := List_GetPrev(L)
  Else
    List_Next := List_GetNext(L)
End;

{ shallow copy of a list (without copying the Prolog Objects) }
Function List_ShallowCopyAll( L : ListPtr ) : ListPtr;
Var 
  Lc,Pc,Fc : ListPtr; { copy, previous copy, first copy }
Begin
  Pc := Nil;
  Fc := Nil;
  While L <> Nil Do
  Begin
    Lc := List_ShallowCopy(L);
    If Pc = Nil Then
      Fc := Lc;
    List_Chain(Pc,Lc);
    Pc := Lc;
    L := List_GetNext(L)
  End;
  List_ShallowCopyAll := Fc
End;

{ chain two list element; both can be Nil }
Procedure List_Chain( L1,L2 : ListPtr );
Begin
  If L1 <> Nil Then
    List_SetNext(L1,L2);
  If L2 <> Nil Then
    List_SetPrev(L2,L1)
End;

End.