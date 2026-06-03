{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Heap.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2023-01-02                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                         H E A P   A L L O C A T I O N                      }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Heap;

Interface

{ maximum size in bytes of a single heap allocation }
{$IFDEF CPU16}
Const
  MaxSizeOnHeap = 65521; { see TP4 pdf p. 443 }
Type
  TSizeOnHeap = Word;
{$ELSE}
Const
  MaxSizeOnHeap = 4294967295; { 2^32 - 1 }
Type
  TSizeOnHeap = PtrUInt;
{$ENDIF}

Procedure GetMemOnHeap( Var p : Pointer; Size: TSizeOnHeap);
Procedure FreeMemOnHeap( Var p : Pointer; Size: TSizeOnHeap);

Implementation

{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code to ensure failed heap allocations return Nil }

{$IFDEF TPC}
{$F+} Function HeapFunc( Size : Word) : Integer; {$F-} 
Begin
  HeapFunc := 1
End;
{$ENDIF}

Procedure InitMalloc;
Begin
{$IFDEF FPC}
  ReturnNilIfGrowHeapFails := True
{$ENDIF}
End;

{-----------------------------------------------------------------------------}

Procedure GetMemOnHeap( Var p : Pointer; Size: TSizeOnHeap);
Begin
  GetMem(p,Size)
End;

Procedure FreeMemOnHeap( Var p : Pointer; Size: TSizeOnHeap);
Begin
  FreeMem(p,Size);
  p := Nil
End;

Begin
{$IFDEF TPC}
  HeapError:=@HeapFunc;
{$ENDIF}
  InitMalloc
End.