{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Stack.pas                                                  }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2023-01-02                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                      S T A C K   O F   P O I N T E R S                     }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

{ on the heap, to avoid recursions }

Unit Stack;

Interface

Uses
  Heap;

{$IFDEF CPU16}
{ TP4 
 p212: maximum size of a structured type is 65520 bytes 
 p414: maximum size of a single variable that on the heap is 65521 bytes }
Const
  StackSize = 16378; { memory constraint is binding: 65521/4 - 2 }
{$ELSE}
Const
  StackSize = 65535; { no memory constraint: max index array }
{$ENDIF}

Type 
  TStackCount = 0..StackSize;
  TStackIndex = 1..StackSize;
  TStackPtr = ^TStack;
  TStack = Record
    Top : TStackCount;
    Next : TStackPtr;
    Data : Array[TStackIndex] Of Pointer
  End;

Procedure Stack_New( Var s : TStackPtr );
Procedure Stack_Kill( Var s : TStackPtr );
Procedure Stack_Reset( s : TStackPtr );
Procedure Stack_Push( Var s : TStackPtr; p : Pointer );
Function Stack_Pop( Var s : TStackPtr ) : Pointer;

Implementation

{ new stack with a single stack chunk }
Procedure Stack_New( Var s : TStackPtr );
Begin
  GetMemOnHeap(s,SizeOf(TStack));
  s^.Top := 0;
  s^.Next := Nil
End;

{ free a single chunk }
Procedure Stack_Free( Var s : TStackPtr );
Begin
  FreeMemOnHeap(s,SizeOf(TStack))
End;

{ kill a stack }
Procedure Stack_Kill( Var s : TStackPtr );
Var
  t : TStackPtr;
Begin
  While s <> Nil Do
  Begin
    t := s^.Next;
    Stack_Free(s);
    s := t
  End
End;

{ keep and reset the first stack in the chain }
Procedure Stack_Reset( s : TStackPtr );
Begin
  Stack_Kill(s^.Next);
  s^.Top := 0
End;

{ push a new item }
Procedure Stack_Push( Var s : TStackPtr; p : Pointer );
Var
  t : TStackPtr;
Begin
  If s^.Top = StackSize Then
  Begin
    t := s;
    Stack_New(s);
    s^.Next := t
  End;
  With s^ Do
  Begin
    Top := Top + 1;
    Data[Top] := p
  End
End;

Function Stack_Pop( Var s : TStackPtr ) : Pointer;
Var
  t : TStackPtr;
Begin
  With s^ Do
  Begin
    Stack_Pop := Data[Top];
    Top := Top - 1
  End;
  If (s^.Next <> Nil) And (s^.Top = 0) Then
  Begin
    t := s;
    s := s^.Next;
    Stack_Free(t)
  End
End;

End.