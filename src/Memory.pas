{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : GC.pas                                                     }
{   Author      : Christophe Bisière                                         }
{   Date        : 2023-01-02                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                    G A R B A G E   C O L L E C T I O N                     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }


{----------------------------------------------------------------------------}
{ low level memory allocation procedure                                      }
{----------------------------------------------------------------------------}

{ type of allocated objects; note that SY, EQ, HE, RE are not managed by GC }
Type
  TypePrologObj = (PR, RU, QU, SY, EQ, BT, CO, FU, VA, HE, RE);

Var 
  mem : Real; { total number of bytes allocated }
  PObjCount : Array[TypePrologObj] of LongInt;

Procedure InitMemoryStats;
Var t : TypePrologObj;
Begin
  mem := 0;
  For t in TypePrologObj Do
    PObjCount[t] := 0
End;

Procedure GetMemory( t : TypePrologObj; Var p : Pointer; size : Integer );
Begin
  p := Malloc(size);
  If p<>Nil Then
  Begin
    FillChar(p^,size,0);
    mem := mem + size;
    PObjCount[t] := PObjCount[t] + 1
  End
  Else
  Begin
    Write('PANIC: MEMORY EXHAUSTED'); { TODO: try to recover }
    Halt(1)
  End
End;

Procedure FreeMemory( t : TypePrologObj; Var p : Pointer; size : Integer );
Begin
  FreeMem(p,size);
  p := Nil;
  mem := mem - size;
  PObjCount[t] := PObjCount[t] - 1
End;

Procedure PrintMemoryStats;
Var t : TypePrologObj;
Begin
  WriteLn('Bytes allocated: ',mem:10:0);
  For t in TypePrologObj Do
    Write(' ',t:2,PObjCount[t]:3);
  WriteLn
End;


{----------------------------------------------------------------------------}
{ marking primitives                                                         }
{----------------------------------------------------------------------------}

Type 
  TypeMark = Boolean;

Var 
  IsMarked : TypeMark; { current value for "is marked" }

{ initialize the mark value for "is marked" }
Procedure InitMark;
Begin
  IsMarked := True
End;

{ unmark all objects by switching the "marked" mark }
Procedure UnMark;
Begin
  IsMarked := Not IsMarked
End;

{ return true if the mark m means the mark is set }
Function IsMark( m : TypeMark ) : Boolean;
Begin
  IsMark := (m = IsMarked)
End;

{ set mark m }
Procedure SetMark( Var m : TypeMark; Marked : Boolean );
Begin
  If Marked Then
    m := IsMarked
  Else
    m := Not IsMarked
End;

Function MarkToStr( m : TypeMark ) : AnyStr;
Begin
  If IsMark(m) Then
    MarkToStr := '*'
  Else
    MarkToStr := '.'
End;



{----------------------------------------------------------------------------}
{ data structures for memory management                                      }
{----------------------------------------------------------------------------}

Const 
  MaxChildren = 255; { max numbers of child pointers per object }

{ Prolog object's metadata for object management (cloning, debugging, etc.) }
Type
  TPObjPtr = ^TPObj;
  { metadata }
  TObjMeta = Record
    PO_TYPE : TypePrologObj; { type of object }
    PO_NAME : LongInt;       { object unique identifier (for convenience) }
    PO_SIZE : Integer;       { size in bytes (including metadata) }
    PO_NPTR : Byte;          { number of pointer fields (which must immediately follow) }
    { deep copy }
    PO_DEEP : Boolean;       { has been visited during the current deep copy operation }
    PO_COPY : TPObjPtr;      { pointer to a copy made during a deep copy }
    PO_NCOP : Integer;       { copy number - FIXME: may overflow }
    { garbage collection }
    PO_MARK : TypeMark;      { GC mark }
    PO_NEXT : TPObjPtr       { GC list of allocated objects }
  End;
  TPObj = Record
    PO_META : TObjMeta;
    PO_PTRS : Array[1..MaxChildren] Of TPObjPtr { pointers to child objects }
  End;

Function PObjectType( p : TPObjPtr ) : TypePrologObj;
Begin
  PObjectType := p^.PO_META.PO_TYPE
End;

Function PObjectCopyNumber( p : TPObjPtr ) : Integer;
Begin
  PObjectCopyNumber := p^.PO_META.PO_NCOP
End;


{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

Function PtrToName( p : TPObjPtr ) : AnyStr;
Var
  s : AnyStr;
Begin
  If (p = Nil) Then
    s := '-'
  Else
    s := '#' + RealToStr(p^.PO_META.PO_NAME,0); //FIXME: LongInt under FP? No error?
  PtrToName := s
End;

Function ToString( p : TPObjPtr ) : AnyStr; forward;

Procedure DumpObject( p : TPObjPtr );
Var i : Byte;
Begin
  Write(PtrToName(p):5,' : ');
  With p^.PO_META Do
  Begin
    Write(PO_TYPE,' ',PtrToName(PO_NEXT):5,' ',MarkToStr(PO_MARK),' ',PO_NCOP,' ',PO_DEEP,' ',PtrToName(PO_COPY):5);
    For i := 1 To PO_NPTR Do
      Write('  ',PtrToName(p^.PO_PTRS[i]):5)
  End;
  Write(' ' + ToString(p));
  WriteLn
End;

Procedure DumpObjects( p : TPObjPtr );
Begin
  If p<>Nil Then
  Begin
    DumpObjects(p^.PO_META.PO_NEXT);
    DumpObject(p)
  End
End;


{----------------------------------------------------------------------------}
{ list of all allocated objects                                              }
{----------------------------------------------------------------------------}

Var 
  AllocHead : TPObjPtr; { list of all allocations }

Procedure InitAlloc;
Begin
  AllocHead := Nil
End;

{ add an object to the list of allocations and set its GC metadata }
Procedure RegisterObject( p : TPObjPtr );
Begin
  With p^.PO_META Do
  Begin
    PO_NEXT := AllocHead;
    SetMark(PO_MARK,False);
    If (PO_NEXT = Nil) Then
      PO_NAME := 0
    Else
      PO_NAME := PO_NEXT^.PO_META.PO_NAME + 1
  End;
  AllocHead := p
End;


{ free an object p, and return the next object }
Function FreeObject( prev, p : TPObjPtr ) : TPObjPtr;
Var nxt : TPObjPtr;
Begin
  With p^.PO_META Do
  Begin
    nxt := PO_NEXT;
    FreeMemory(PO_TYPE,p,PO_SIZE)
  End;
  If (prev = Nil) Then
    AllocHead := nxt
  Else
    prev^.PO_META.PO_NEXT := nxt;
  FreeObject := nxt
End;


Procedure DumpRegisteredObject;
Begin
  DumpObjects(AllocHead)
End;

{ free all unmarked objects and associated memory management record }
Procedure Sweep;
Var p, prev : TPObjPtr;
Begin
  p := AllocHead;
  prev := Nil;
  While (p <> Nil) Do
    With p^.PO_META Do
    Begin
      If Not IsMark(PO_MARK) Then
        p := FreeObject(prev, p)
      Else
      Begin
        prev := p;
        p := PO_NEXT
      End
    End
End;


{----------------------------------------------------------------------------}
{ marking                                                                    }
{----------------------------------------------------------------------------}

Function ObjectIsMarked( p : TPObjPtr ) : Boolean;
Begin
  ObjectIsMarked := IsMark(p^.PO_META.PO_MARK)
End;

Procedure MarkOneObject( p : TPObjPtr );
Begin
  SetMark(p^.PO_META.PO_MARK, True)
End;

{ is p a reference to an object that must be marked? }
Function Markable( p : TPObjPtr ) : Boolean;
Var must : Boolean;
Begin
  must := True;
  If (p = Nil) Then
    must := False
  Else
    If ObjectIsMarked(p) Then
      must := False;
  Markable := must
End;

{ mark p and all objects that are reachable from p }
Procedure Mark( p : TPObjPtr );
Var i : Byte;
Begin
  If Markable(p) Then
  Begin
    MarkOneObject(p);
    With p^.PO_META Do
      For i := 1 To PO_NPTR Do
        Mark(p^.PO_PTRS[i])
  End
End;



{----------------------------------------------------------------------------}
{ new / copy                                                                 }
{----------------------------------------------------------------------------}

{ allocate a Prolog object of size s }
Function NewPrologObject( t : TypePrologObj; s : Integer; n : Byte ) : TPObjPtr;
Var p : TPObjPtr;
Begin
  GetMemory(t,p,s);

  With p^.PO_META Do
  Begin
    PO_TYPE := t;
    PO_SIZE := s;
    PO_NPTR := n;
    { deep copy metadata: }
    PO_DEEP := False;
    PO_COPY := Nil;
    PO_NCOP := 0
  End;
  RegisterObject(p);
  NewPrologObject := p
End;


{ copy an object 
- set PO_DEEP for both the old and copied objects, to note they have
  been visited during the deep copy operation
- set PO_COPY of the old object to point to its copy
- set PO_COPY of the new object to Nil to characterize this object as new
}
Function CopyObject( p : TPObjPtr ) : TPObjPtr;
Var 
  pc : TPObjPtr;
Begin
  CheckCondition(Not p^.PO_META.PO_DEEP,'Copy of an already visited object');
  With p^.PO_META Do { memory copy }
  Begin
    GetMemory(PO_TYPE,pc,PO_SIZE); 
    Move(p^,pc^,PO_SIZE)
  End;
  RegisterObject(pc);
  With pc^.PO_META Do { new object is a copy }
  Begin
    PO_DEEP := True;
    PO_COPY := Nil;
    PO_NCOP := PO_NCOP + 1 { note it is an additional copy }
  End;
  With p^.PO_META Do { old object has been copied }
  Begin
    PO_DEEP := True;
    PO_COPY := pc  { link old -> copy }
  End;
  CopyObject := pc
End;


{ deep copy of an object, or Nil if p is Nil }
Function DeepCopyObject( p : TPObjPtr ) : TPObjPtr;
Var 
  pc : TPObjPtr;
  i : Byte;
Begin
  pc := Nil;
  If p <> Nil Then
  Begin
    With p^.PO_META Do
    Begin
      If PO_DEEP Then { object has already been visited during this deep copy }
      Begin 
        If PO_COPY=Nil Then { is a copy }
          pc := p
        Else
          pc := PO_COPY { has been copied }
      End
      Else
      Begin
        pc := CopyObject(p);
        With pc^ Do
        Begin
          For i := 1 To PO_META.PO_NPTR Do
            PO_PTRS[i] := DeepCopyObject(PO_PTRS[i])
        End
      End
    End
  End;
  DeepCopyObject := pc
End;

{ reset the deep copy state of p and of all objects that are reachable from p }
Procedure PrepareDeepCopy( p : TPObjPtr );
Var i : Byte;
Begin
  If p<>Nil Then
  Begin
    With p^.PO_META Do
    Begin
      If PO_DEEP Then
      Begin
        PO_DEEP := False;
        PO_COPY := Nil;
        For i := 1 To PO_NPTR Do
          PrepareDeepCopy(p^.PO_PTRS[i])
      End
    End
  End
End;


{----------------------------------------------------------------------------}
{ GC roots                                                                   }
{----------------------------------------------------------------------------}

Const MaxGCRoots = 255;

Var 
  NbRoots : Byte;
  Roots : Array[1..MaxGCRoots] Of TPObjPtr;

Procedure InitGCRoots;
Begin
  NbRoots := 0
End;

Procedure AddGCRoot(r : TPObjPtr);
Begin
  CheckCondition(NbRoots<MaxGCRoots,'GC root pool is full');
  NbRoots := NbRoots + 1;
  Roots[NbRoots] := r
End;


{----------------------------------------------------------------------------}
{ garbage collector                                                          }
{----------------------------------------------------------------------------}

Procedure GCInit;
Begin
  InitAlloc;
  InitGCRoots;
  InitMark
End;

Procedure GarbageCollector;
Var i : Byte;
Begin
  For i := 1 To NbRoots Do
    Mark(Roots[i]);
  Sweep;
  UnMark
End;


{----------------------------------------------------------------------------}
{ deep copy                                                                  }
{----------------------------------------------------------------------------}

Function DeepCopy( p : TPObjPtr ) : TPObjPtr;
Var pc : TPObjPtr;
Begin
  PrepareDeepCopy(p);
  pc := DeepCopyObject(p);
  DeepCopy := pc
End;


{----------------------------------------------------------------------------}
{ initialize memory management unit                                          }
{----------------------------------------------------------------------------}

Procedure MMInit;
Begin
  InitMalloc;
  InitMemoryStats;
  GCInit
End;
