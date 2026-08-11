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

Uses
  TPointer;

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

Procedure GetMemOnHeap( pp : PointerPtr; Size: TSizeOnHeap);
Procedure FreeMemOnHeap( pp : PointerPtr; Size: TSizeOnHeap);

Implementation

{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code to ensure failed heap allocations return Nil }

{$IFDEF TPC}
{$F+} Function HeapFunc( Size : Word ) : Integer; {$F-} 
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

Procedure GetMemOnHeap( pp : PointerPtr; Size: TSizeOnHeap );
Begin
  GetMem(pp^,Size)
End;

Procedure FreeMemOnHeap( pp : PointerPtr; Size: TSizeOnHeap );
Begin
  FreeMem(pp^,Size);
  pp^ := Nil
End;

Begin
{$IFDEF TPC}
  HeapError:=@HeapFunc;
{$ENDIF}
  InitMalloc
End.