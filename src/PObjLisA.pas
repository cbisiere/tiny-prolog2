{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjLisA.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       L I S T   O F   P O I N T E R S                      }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjLisA;

Interface

Uses
  Errs,
  CWrites,
  Memory,
  PObj,
  PObjRest;

{ list of addresses of pointers; the difference with ListPtr is that there is 
 an extra piece of data, a pointer to an object's address; also, we do not 
 need a double-linked list here }

Type
  TObjectPtrAddr = ^TObjectPtr;

Type 
  ListAPtr = ^TObjListA;
  TObjListA = Record
    PO_META : TObjMeta;
    { deep copied: }
    LL_NEXT : ListAPtr;
    LL_POBJ : TObjectPtr;
    { extra data: }
    LL_ADDR : TObjectPtrAddr
  End;

Function ListA_New( Obj : TObjectPtr; Addr : TObjectPtrAddr ) : ListAPtr;

Function ListA_GetNext( L : ListAPtr ) : ListAPtr;
Procedure ListA_SetNext( La : ListAPtr; L1 : ListAPtr );
Procedure ListA_SetNextWithUndo( La : ListAPtr; L1 : ListAPtr;
    L : RestPtr; Undo : Boolean );
Function ListA_GetObject( L : ListAPtr ) : TObjectPtr;
Procedure ListA_SetObject( L : ListAPtr; Obj : TObjectPtr );
Function ListA_GetAddr( L : ListAPtr ) : TObjectPtrAddr;
Procedure ListA_SetAddr( L : ListAPtr; Addr : TObjectPtrAddr );

Function ListA_Last( L : ListAPtr ) : ListAPtr;
Function ListA_Lookup( L : ListAPtr; 
    Obj : TObjectPtr; Addr : TObjectPtrAddr ) : ListAPtr;

Procedure ListA_DumpList( L : ListAPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new list element }
Function ListA_New( Obj : TObjectPtr; Addr : TObjectPtrAddr ) : ListAPtr;
Var 
  L : ListAPtr;
  ptr : TObjectPtr Absolute L;
Begin
  ptr := NewRegisteredPObject(LA,SizeOf(TObjListA),2,True,2);
  With L^ Do
  Begin
    LL_NEXT := Nil;
    LL_POBJ := Obj;
    LL_ADDR := Addr
  End;
  ListA_New := L
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

Function ListA_GetNext( L : ListAPtr ) : ListAPtr;
Begin
  CheckCondition(L <> Nil,'ListA_GetNext: Nil');
  ListA_GetNext := L^.LL_NEXT
End;

Procedure ListA_SetNext( La : ListAPtr; L1 : ListAPtr );
Var
  L : RestPtr;
Begin
  L := Nil;
  ListA_SetNextWithUndo(La,L1,L,False)
End;

Procedure ListA_SetNextWithUndo( La : ListAPtr; L1 : ListAPtr;
    L : RestPtr; Undo : Boolean );
Begin
  CheckCondition(La <> Nil,'ListA_SetNextWithUndo: Nil');
  Rest_SetMem(L,TObjectPtr(La),TObjectPtr(La^.LL_NEXT),TObjectPtr(L1),Undo)
End;

Function ListA_GetObject( L : ListAPtr ) : TObjectPtr;
Begin
  CheckCondition(L <> Nil,'ListA_GetObject: Nil');
  ListA_GetObject := L^.LL_POBJ
End;

Procedure ListA_SetObject( L : ListAPtr; Obj : TObjectPtr );
Begin
  CheckCondition(L <> Nil,'ListA_SetObject: Nil');
  L^.LL_POBJ := Obj
End;

Function ListA_GetAddr( L : ListAPtr ) : TObjectPtrAddr;
Begin
  CheckCondition(L <> Nil,'ListA_GetAddr: Nil');
  ListA_GetAddr := L^.LL_ADDR
End;

Procedure ListA_SetAddr( L : ListAPtr; Addr : TObjectPtrAddr );
Begin
  CheckCondition(L <> Nil,'ListA_SetAddr: Nil');
  L^.LL_ADDR := Addr
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ return the last item of a list }
Function ListA_Last( L : ListAPtr ) : ListAPtr;
Begin
  CheckCondition(L <> Nil,'ListA_Last: Nil');
  While ListA_GetNext(L) <> Nil Do
    L := ListA_GetNext(L);
  ListA_Last := L
End;

{ look for an item (Obj, Addr); return Nil if the item is not in the list; for
 convenience, L can be Nil }
Function ListA_Lookup( L : ListAPtr; 
    Obj : TObjectPtr; Addr : TObjectPtrAddr ) : ListAPtr;
Var
  Li : ListAPtr;
  Found : Boolean;
Begin
  Found := False;
  Li := L;
  While (Li <> Nil) And Not Found Do
  Begin
    If (ListA_GetObject(Li) = Obj) And (ListA_GetAddr(Li) = Addr) Then
      Found := True
    Else
      Li := ListA_GetNext(Li)
  End;
  ListA_Lookup := Li
End;

{-----------------------------------------------------------------------}
{ dump                                                                  }
{-----------------------------------------------------------------------}

Procedure ListA_DumpOne( L : ListAPtr );
Begin
  CWrite('(' + PtrToName(ListA_GetObject(L)) + ',' 
      + PtrToName(ListA_GetAddr(L)^) + ')')
End;

Procedure ListA_DumpList( L : ListAPtr );
Begin
  CWrite('[ ');
  While L <> Nil Do
  Begin
    ListA_DumpOne(L);
    CWrite(' ');
    L := ListA_GetNext(L)
  End;
  CWrite(']')
End;

End.