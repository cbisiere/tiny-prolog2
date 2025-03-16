{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Memory.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 2023-01-02                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                    G A R B A G E   C O L L E C T I O N                     }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Memory;

Interface

Uses
  ShortStr,
  Num,
  Errs,
  CWrites;

{$IFDEF CPU16}
{ TP4 p212: maximum size of a structured type is 65520 bytes }
Const
  GetMemMaxSize = 65521; { see TP4 pdf p. 443 }
  MaxChildren = 16300; { 4-bytes pointers + small metadata overhead }
Type 
  TObjectSize = Word;
  TObjectChild = Word; { child index or counter }
{$ELSE}
Const
  GetMemMaxSize = 4294967295; { 2^32 - 1 }
  { maximum number of child objects (see TObject below); on some platforms (at 
   least Windows) Fpc enforces a 2Gb limit on data element, so assuming that 
   the number of children is limited by a maximum allocation size of 4Gb for a
   single item on the heap triggers a compilation error on TObject ("Error: 
   Data element too large"); the value below is overkill anyway }
  MaxChildren = 536870000; { 4-bytes pointers + small metadata overhead }
 Type
  TObjectSize = PtrUInt; { size of a single object, in bytes }
  TObjectChild = PosInt; { child index or counter }
{$ENDIF}

Const
  MaxNbObjectTypes = 255;

Type 
  TObjectTypeIndex = 1..MaxNbObjectTypes;
  TObjectName = String[2];
  TypeMark = Boolean;
  TSerial = PosInt; { serial number to support object marking } 

{ object's metadata for object management (cloning, debugging, etc.) }
Type
  TObjectPtr = ^TObject;
  { metadata }
  TObjMeta = Record
    PO_MAGI : Integer;          { magic number (debug) }
    PO_FREE : Boolean;          { object's memory has been freed (debug) }
    PO_TYPE : TObjectTypeIndex; { type of object }
    PO_GUID : LongInt;          { object globally unique identifier (for convenience and sorting) }
    PO_SIZE : TObjectSize;      { size in bytes (including metadata) }
    PO_NPTR : TObjectChild; { number of PObject pointers (which must immediately follow the metadata) }
    { deep copy }
    PO_DPOK : Boolean;          { deep copy of this object is allowed }
    PO_COPY : TObjectPtr;       { pointer to a copy made during a deep copy; warning: copy may have been GC'ed }
    PO_CUID : LongInt;          { GUID of the copy, or 0; for display purpose, as the copy may not exist }
    PO_NCOP : Integer;          { copy number - FIXME: may overflow }
    PO_NDEE : TObjectChild; { number of pointers of objects to deep copy (must be less of equal than PO_NPTR) }
    { garbage collection }
    PO_MARK : TypeMark;         { GC mark }
    PO_NEXT : TObjectPtr;       { GC list of allocated objects }
    { misc }
    PO_SEEN : TSerial           { generic 'seen' indicator; used to detect loops }
  End;
  TObject = Record
    PO_META : TObjMeta;
    PO_PTRS : Array[1..MaxChildren] Of TObjectPtr { pointers to child objects }
  End;

{ mem stats }
Procedure PrintMemoryStats;

{ object types }
Function DeclareObjectType( ObjName : TObjectName ) : TObjectTypeIndex;

{ object accessors }
Function ObjectType( p : TObjectPtr ) : TObjectTypeIndex;
Function ObjectGuid( p : TObjectPtr ) : LongInt;
Procedure CheckIsObject( p : TObjectPtr; prompt : TString );
Function ObjectCopyNumber( p : TObjectPtr ) : Integer;
Function PtrToName( p : TObjectPtr ) : TString;
Procedure DumpObject( p : TObjectPtr; extra : Boolean );

{ object creation and accounting }
Function GetRegistrationState : Boolean;
Procedure SetRegistration( state : Boolean );
Function NewRegisteredObject( t : TObjectTypeIndex; b: TObjectSize ; 
    n: TObjectChild; CanCopy : Boolean; d : TObjectChild ) : TObjectPtr;
Procedure DumpRegisteredObject;

{ helper to avoid loops }
Function NewMemorySerial : TSerial;

{ copy and deep copy }
Procedure SetObjectsAsGenuine( p : TObjectPtr );
Function DeepCopyObject( p : TObjectPtr; CopyChildren : Boolean; 
    g : TSerial ) : TObjectPtr;
Function DeepCopy( p : TObjectPtr ) : TObjectPtr;

{ helpers for marking }
Function ObjectSeen( p : TObjectPtr; g : TSerial ) : Boolean;
Procedure SetObjectSeen( p : TObjectPtr; g : TSerial );

{ GC }
Procedure AddGCRoot( r : TObjectPtr );
Procedure GarbageCollector;
Procedure DumpGCRoots;


Implementation
{-----------------------------------------------------------------------------}
{ TP4/FPC compatibility code to ensure that failed heap allocations 
 returns Nil }

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

{----------------------------------------------------------------------------}
{ declaration of object types managed par the memory manager                 }
{----------------------------------------------------------------------------}

Type 
  TMemSize = LongLongInt; { large amount of memory, in bytes }

  TObjectCount = LongInt; { number of objects, non negative }

  TMemObject = Record
    IsSet : Boolean; { this table entry is set }
    Name : TObjectName; { name of this type }
    Count : TObjectCount; { number of objects of this type allocated so far }
    Mem : TMemSize { total memory (in bytes) taken by all objects of this type }
  End;

Var
  NbObjectTypes : 0..MaxNbObjectTypes;
  MemObjects : Array[TObjectTypeIndex] Of TMemObject;

{ return the name of an object type }
Function GetObjectName( t : TObjectTypeIndex ) : TObjectName;
Begin
  With MemObjects[t] Do
  Begin
    CheckCondition(IsSet,'GetObjectName: entry not set');
    GetObjectName := Name
  End
End;

{ declare a new object type; return its index for future reference }
Function DeclareObjectType( ObjName : TObjectName ) : TObjectTypeIndex;
Begin
  CheckCondition(NbObjectTypes < MaxNbObjectTypes,
      'DeclareObjectType: table is full');
  NbObjectTypes := NbObjectTypes + 1;
  With MemObjects[NbObjectTypes] Do
  Begin
    CheckCondition(Not IsSet,'DeclareObjectType: entry already set');
    IsSet := True;
    Name := ObjName;
    Count := 0;
    Mem := 0
  End;
  DeclareObjectType := NbObjectTypes
End;

{ initialize the table of registered memory object types }
Procedure InitMemObjects;
Var
  t : TObjectTypeIndex;
Begin
  NbObjectTypes := 0;
  For t := 1 To MaxNbObjectTypes Do
    With MemObjects[t] Do
    Begin
      IsSet := False;
      Name := '';
      Count := 0;
      Mem := 0
    End
End;

{----------------------------------------------------------------------------}
{ memory allocation stats                                                    }
{----------------------------------------------------------------------------}

Var 
  TotalMemSize : TMemSize; { total number of bytes allocated }

Procedure InitMemoryStats;
Begin
  TotalMemSize := 0
End;

Procedure PrintMemoryStats;
Var 
  t : TObjectTypeIndex;
Begin
  CWrite('Bytes allocated: ' + LongLongIntToShortString(TotalMemSize));
  CWriteLn;
  For t := 1 To MaxNbObjectTypes Do
    With MemObjects[t] Do
      If IsSet Then
      Begin
        CWrite(' ' + Name + ': ' + RAlign(LongIntToShortString(Count),5));
        CWrite(' ' + RAlign(LongLongIntToShortString(Mem),8));
        CWriteLn
      End
End;

{ increase by one the number of objects of type t, with actual size b }
Procedure IncMemoryStats( t : TObjectTypeIndex; b : TObjectSize );
Begin
  With MemObjects[t] Do
  Begin
    CheckCondition(IsSet,'IncMemoryStats: entry not set');
    Count := Count + 1;
    Mem := Mem + b
  End;
  TotalMemSize := TotalMemSize + b
End;

{ decrease by one the number of objects of type t, with actual size size }
Procedure DecMemoryStats( t : TObjectTypeIndex; size : TObjectSize );
Begin
  With MemObjects[t] Do
  Begin
    CheckCondition(IsSet,'DecMemoryStats: entry not set');
    CheckCondition(Count > 0,'DecMemoryStats: count is already zero');
    Count := Count - 1;
    Mem := Mem - size
  End;
  TotalMemSize := TotalMemSize - size
End;



{----------------------------------------------------------------------------}
{ marking primitives                                                         }
{----------------------------------------------------------------------------}

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

Function MarkToShortString( m : TypeMark ) : TString;
Begin
  If IsMark(m) Then
    MarkToShortString := '*'
  Else
    MarkToShortString := '.'
End;


{----------------------------------------------------------------------------}
{ data structures for memory management                                      }
{----------------------------------------------------------------------------}

Const 
  OBJECT_MAGIC_NUMBER = 12345;

{----------------------------------------------------------------------------}
{ accessors                                                                  }
{----------------------------------------------------------------------------}

Function ObjectMagic( p : TObjectPtr ) : Integer;
Begin
  ObjectMagic := p^.PO_META.PO_MAGI
End;

Function IsObject( p : TObjectPtr ) : Boolean;
Begin
  IsObject := ObjectMagic(p) = OBJECT_MAGIC_NUMBER
End;

Procedure SetObjectMagic( p : TObjectPtr; magic : Integer );
Begin
  p^.PO_META.PO_MAGI := magic
End;

Function ObjectIsFree( p : TObjectPtr ) : Boolean;
Begin
  ObjectIsFree := p^.PO_META.PO_FREE
End;

Procedure SetObjectFree( p : TObjectPtr; IsFree : Boolean );
Begin
  p^.PO_META.PO_FREE := IsFree
End;

Function ObjectType( p : TObjectPtr ) : TObjectTypeIndex;
Begin
  ObjectType := p^.PO_META.PO_TYPE
End;

Procedure SetObjectType( p : TObjectPtr; t : TObjectTypeIndex );
Begin
  p^.PO_META.PO_TYPE := t
End;

Function ObjectSize( p : TObjectPtr ) : TObjectSize;
Begin
  ObjectSize := p^.PO_META.PO_SIZE
End;

Procedure SetObjectSize( p : TObjectPtr; b : TObjectSize );
Begin
  p^.PO_META.PO_SIZE := b
End;

Function ObjectNbChildren( p : TObjectPtr ) : TObjectChild;
Begin
  ObjectNbChildren := p^.PO_META.PO_NPTR
End;

Function ObjectNbChildrenToCopy( p : TObjectPtr ) : TObjectChild;
Begin
  ObjectNbChildrenToCopy := p^.PO_META.PO_NDEE
End;

Function ObjectChild( p : TObjectPtr; i : TObjectChild ) : TObjectPtr;
Begin
  ObjectChild := p^.PO_PTRS[i]
End;

Procedure SetObjectChild( p : TObjectPtr; i : TObjectChild; 
    child : TObjectPtr );
Begin
  p^.PO_PTRS[i] := child
End;

Function ObjectCopyNumber( p : TObjectPtr ) : Integer;
Begin
  ObjectCopyNumber := p^.PO_META.PO_NCOP
End;

Function ObjectGuid( p : TObjectPtr ) : LongInt;
Begin
  ObjectGuid := p^.PO_META.PO_GUID
End;

Procedure SetObjectGuid( p : TObjectPtr; guid : LongInt);
Begin
  p^.PO_META.PO_GUID := guid
End;

Function ObjectIsMarked( p : TObjectPtr ) : Boolean;
Begin
  ObjectIsMarked := IsMark(p^.PO_META.PO_MARK)
End;

Procedure SetObjectMark( p : TObjectPtr; Marked : Boolean);
Begin
  SetMark(p^.PO_META.PO_MARK,Marked)
End;

Function ObjectNext( p : TObjectPtr ) : TObjectPtr;
Begin
  ObjectNext := p^.PO_META.PO_NEXT
End;

Procedure SetObjectNext( p,nxt : TObjectPtr );
Begin
  p^.PO_META.PO_NEXT := nxt
End;

Function ObjectSeen( p : TObjectPtr; g : TSerial ) : Boolean;
Begin
  ObjectSeen := p^.PO_META.PO_SEEN = g
End;

Procedure SetObjectSeen( p : TObjectPtr; g : TSerial );
Begin
  p^.PO_META.PO_SEEN := g
End;

{----------------------------------------------------------------------------}
{ debug / dump                                                               }
{----------------------------------------------------------------------------}

Function FindObjectById( guid : LongInt ) : TObjectPtr; Forward;

{ global object ID to string }
Function GuidToShortString( guid : LongInt ) : TString;
Begin
  GuidToShortString := '#' + LongIntToShortString(guid)
End;

{ object pointer to object name }
Function PtrToName( p : TObjectPtr ) : TString;
Var
  s : TString;
  guid : LongInt;
Begin
  If (p = Nil) Then
    s := '-'
  Else If Not IsObject(p) Then
    s := '<Broken>' { Bug: Not An Object }
  Else If ObjectIsFree(p) Then
    s := '<Free>' { Bug: Object than has been freed }
  Else
  Begin
    guid := ObjectGuid(p);
    s := GuidToShortString(guid);
    { mark unregistered objects }
    If FindObjectById(guid) = Nil Then { Warning: time consuming }
      s := s + '(!)'
  End;
  PtrToName := s
End;

{ dump an object, possibly with extra data }
Procedure DumpObject( p : TObjectPtr; extra : Boolean );
Var 
  i : TObjectChild;
  child : TObjectPtr;
Begin
  CWrite(RAlign(PtrToName(p),5) + ' : ');
  CWrite(RAlign(IntToShortString(ObjectSize(p)),3) + ' ');
  With p^.PO_META Do
  Begin
    CWrite(GetObjectName(PO_TYPE) + ' ' + MarkToShortString(PO_MARK) + ' ');
    CWrite(IntToShortString(PO_NCOP) + ' ');
    CWrite(RAlign(GuidToShortString(PO_CUID),5))
  End;
  CWrite(' [');
  For i := 1 To ObjectNbChildren(p) Do
  Begin
    child := ObjectChild(p,i);
    CWrite('  ' + PtrToName(child))
  End;
  CWrite(' ]');
  If extra Then
  Begin
    CWrite(' ')
    {//;WriteExtraData(p)}
  End;
  CWriteLn
End;

{ check a memory location has a chance to be a legit Prolog object }
Procedure CheckIsObject( p : TObjectPtr; prompt : TString );
Begin
  CheckCondition(IsObject(p), prompt + ': not an object')
End;

{ dump all the registered Prolog objects }
Procedure DumpObjects( p : TObjectPtr; extra : Boolean );
Begin
  If p<>Nil Then
  Begin
    CheckIsObject(p,'DumpObjects');
    DumpObject(p,extra);
    DumpObjects(ObjectNext(p),extra)
  End
End;


{----------------------------------------------------------------------------}
{ list of all allocated objects                                              }
{----------------------------------------------------------------------------}

Var 
  AllocHead : TObjectPtr; { list of all allocations }
  OngoingGC : Boolean;  { is a GC ongoing? }
  DoRegister : Boolean; { should new objects be registered? False during debug }

Procedure SetRegistration( state : Boolean );
Begin
  DoRegister := state
End;

Function GetRegistrationState : Boolean;
Begin
  GetRegistrationState := DoRegister
End;

Procedure InitAlloc;
Begin
  AllocHead := Nil;
  OngoingGC := False;
  SetRegistration(True)
End;

{ dump all registered objects }
Procedure DumpRegisteredObject;
Begin
  DumpObjects(AllocHead,True)
End;

{ find the object with guid id in the object store, or Nil }
Function FindObjectById( guid : LongInt ) : TObjectPtr;
Var
  p : TObjectPtr;
  Found : Boolean;
Begin
  p := AllocHead;
  Found := False;
  While (p <> Nil) And (Not Found) Do
  Begin
    Found := ObjectGuid(p) = guid;
    If Not Found Then
      p := ObjectNext(p)
  End;
  FindObjectById := p
End;

{ add an object to the list of allocations and set its GC metadata }
Procedure RegisterObject( p : TObjectPtr );
Var
  nxt : TObjectPtr;
  guid : LongInt;
Begin
  CheckCondition(Not OngoingGC Or Not GetRegistrationState,
      'RegisterObject: object registration during GC');
  If GetRegistrationState Then 
  Begin
    SetObjectNext(p,AllocHead);
    SetObjectMark(p,False);
    nxt := ObjectNext(p);
    If nxt = Nil Then
      guid := 1
    Else
      guid := ObjectGuid(nxt) + 1;
    SetObjectGuid(p,guid);
    AllocHead := p
  End
End;

{ remove an object from the list of allocated objects; prev is the
  object just before in the list, or Nil if p is the first object;
  return the next object after p (could be Nil) }
Function UnregisterObject( prev,p : TObjectPtr ) : TObjectPtr;
Var 
  nxt : TObjectPtr;
Begin
  CheckCondition(p <> Nil,'Unregister: p is Nil');
  nxt := ObjectNext(p);
  SetObjectNext(p,Nil);
  If (prev = Nil) Then
    AllocHead := nxt
  Else
    SetObjectNext(prev,nxt);
  UnregisterObject := nxt
End;

{----------------------------------------------------------------------------}
{ object allocation with accounting                                          }
{----------------------------------------------------------------------------}

{ allocate heap memory for an object of b bytes; return the object }
Function GetMemForObject( b : TObjectSize ) : TObjectPtr;
Var
  p : TObjectPtr;
Begin
  CheckCondition(b <= GetMemMaxSize,'Cannot allocate an object of this size');
  GetMem(p,b);
  { cannot GC here, so in case of OOM we just abort }
  If p = Nil Then
  Begin
    CWrite('Runtime Error: Memory exhausted');
    CWriteln;
    PrintMemoryStats;
    CWrite('Halting');
    CWriteln;
    HaltProgram
  End;
  GetMemForObject := p
End;

Procedure FreeMemOfObject( Var p : TObjectPtr );
Begin
  FreeMem(p,ObjectSize(p));
  p := Nil { prevent dangling pointers }
End;

{ allocate memory on the heap for an object of type t and size b in bytes; 
 returns the object }
Function NewObject( t : TObjectTypeIndex; b : TObjectSize ) : TObjectPtr;
Var 
  p : TObjectPtr;
  ptr : Pointer Absolute p;
Begin
  p := GetMemForObject(b);
  FillChar(p^,b,0);
  { set the bare minimum object data }
  SetObjectMagic(p,OBJECT_MAGIC_NUMBER);
  SetObjectFree(p,False);
  SetObjectType(p,t);
  SetObjectSize(p,b);
  { accounting }
  IncMemoryStats(t,b);
  NewObject := p
End;

{ free a Prolog object }
Procedure FreeObject( Var p : TObjectPtr );
Begin
  DecMemoryStats(ObjectType(p),ObjectSize(p));
  SetObjectFree(p,True);
  FreeMemOfObject(p)
End;

{ clone object p in memory, and return the clone }
Function CloneObject( p : TObjectPtr ) : TObjectPtr;
Var 
  pc : TObjectPtr;
  t : TObjectTypeIndex;
Begin
  t := ObjectType(p);
  pc := GetMemForObject(ObjectSize(p));
  Move(p^,pc^,ObjectSize(p));
  IncMemoryStats(t,ObjectSize(pc));
  CloneObject := pc
End;

{----------------------------------------------------------------------------}
{ helpers for loop prevention                                                }
{----------------------------------------------------------------------------}

{ globally unique serial number used for loop detection }
Var
  MM_CURRENT_SERIAL : TSerial;

Function NewMemorySerial : TSerial;
Begin
  MM_CURRENT_SERIAL := MM_CURRENT_SERIAL + 1;
  NewMemorySerial := MM_CURRENT_SERIAL
End;

{----------------------------------------------------------------------------}
{ operations on registered objects                                           }
{----------------------------------------------------------------------------}

{ free a registered object p, and return the next object }
Function FreeRegisteredObject( prev : TObjectPtr; Var p : TObjectPtr ) : TObjectPtr;
Var 
  nxt : TObjectPtr;
Begin
  CheckIsObject(p,'FreeRegisteredObject');
  CheckCondition(Not ObjectIsFree(p),'FreeRegisteredObject: double free');
  nxt := UnregisterObject(prev,p);
  FreeObject(p);
  FreeRegisteredObject := nxt
End;


{ allocate an object of type t and size b; metadata are followed by n child 
  object pointers; the first d child objects of these n are copied when the 
  object is deep copied (but only if deep copy is allowed for that object: 
  CanCopy) }
Function NewRegisteredObject( t : TObjectTypeIndex; b: TObjectSize; 
  n: TObjectChild; CanCopy : Boolean; d : TObjectChild ) : TObjectPtr;
Var 
  p : TObjectPtr;
Begin
  CheckCondition(d <= n,'NewRegisteredObject: d > n');
  CheckCondition(n <= MaxChildren,'NewRegisteredObject: too many children');
  p := NewObject(t,b);
  With p^.PO_META Do
  Begin
    PO_NPTR := n;
    { deep copy metadata: }
    PO_DPOK := CanCopy;
    PO_COPY := Nil;
    PO_CUID := 0;
    PO_NCOP := 0;
    PO_NDEE := d;
    { misc }
    PO_SEEN := 0
  End;
  RegisterObject(p);
  NewRegisteredObject := p
End;

{ copy an object 
- set PO_COPY of the old object to point to its copy
- set PO_COPY of the new object to Nil to characterize this object as new
}
Function CopyObject( p : TObjectPtr ) : TObjectPtr;
Var 
  pc : TObjectPtr;
Begin
  pc := CloneObject(p);
  RegisterObject(pc);
  With pc^.PO_META Do { new object is a copy }
  Begin
    PO_COPY := Nil;
    PO_CUID := 0;
    PO_NCOP := PO_NCOP + 1 { note it is an additional copy }
  End;
  With p^.PO_META Do { old object has been copied }
  Begin
    PO_COPY := pc;  { link old -> copy }
    PO_CUID := ObjectGuid(pc)
  End;
  CopyObject := pc
End;


{ deep copy of an object, or Nil if p is Nil; deep copy all the children if
 CopyChildren is True; this boolean allows writing tailor-made deep copy 
 procedures (see, e.g. CopyTerm) }
Function DeepCopyObject( p : TObjectPtr; CopyChildren : Boolean; 
    g : TSerial ) : TObjectPtr;
Var 
  pc : TObjectPtr;
  i : TObjectChild;
Begin
  pc := Nil;
  If p <> Nil Then
  Begin
    With p^.PO_META Do
    Begin
      If Not PO_DPOK Then { deep copy not authorized: return the object itself }
        pc := p
      Else If ObjectSeen(p,g) Then { object has already been visited during this deep copy }
      Begin 
        If PO_COPY = Nil Then { is a copy }
          pc := p
        Else
          pc := PO_COPY { has been copied }
      End
      Else
      Begin
        SetObjectSeen(p,g);
        pc := CopyObject(p); { copy, including serial }
        If CopyChildren Then
          For i := 1 To ObjectNbChildrenToCopy(p) Do
            SetObjectChild(pc,i,DeepCopyObject(ObjectChild(p,i),CopyChildren,g))
      End
    End
  End;
  DeepCopyObject := pc
End;

{ set as genuine (that is, not a copy) p and of all objects that are reachable 
 from p through children }
Procedure SetObjectsAsGenuine( p : TObjectPtr );

  Procedure DoSetObjectsAsGenuine( p : TObjectPtr; g : TSerial );
  Var 
    i : TObjectChild;
  Begin
    If (p <> Nil) And Not ObjectSeen(p,g) Then
    Begin
      SetObjectSeen(p,g);
      With p^.PO_META Do
      Begin
        PO_NCOP := 0;
        For i := 1 To ObjectNbChildrenToCopy(p) Do
          DoSetObjectsAsGenuine(ObjectChild(p,i),g)
      End
    End
  End;

Begin
  DoSetObjectsAsGenuine(p,NewMemorySerial)
End;


{----------------------------------------------------------------------------}
{ mark & sweep                                                               }
{----------------------------------------------------------------------------}

{ free all unmarked objects and associated memory management record }
Procedure Sweep;
Var 
  p, prev : TObjectPtr;
Begin
  p := AllocHead;
  prev := Nil;
  While (p <> Nil) Do
  Begin
    If Not ObjectIsMarked(p) Then
      p := FreeRegisteredObject(prev,p)
    Else
    Begin
      prev := p;
      p := ObjectNext(p)
    End
  End
End;

{ set p as marked to escape the next sweeping operation }
Procedure MarkOneObject( p : TObjectPtr );
Begin
  SetObjectMark(p,True)
End;

{ is p a reference to an object that must be marked? }
Function Markable( p : TObjectPtr ) : Boolean;
Var 
  must : Boolean;
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
Procedure Mark( p : TObjectPtr );
Var 
  i : TObjectChild;
Begin
  If Markable(p) Then
  Begin
    MarkOneObject(p);
    For i := 1 To ObjectNbChildren(p) Do
      Mark(ObjectChild(p,i))
  End
End;


{----------------------------------------------------------------------------}
{ GC roots                                                                   }
{----------------------------------------------------------------------------}

Const 
  MaxGCRoots = 255;

Type
  TNbRoots = 0..MaxGCRoots;

Var 
  NbRoots : TNbRoots;
  Roots : Array[1..MaxGCRoots] Of TObjectPtr;

Procedure InitGCRoots;
Begin
  NbRoots := 0
End;

Procedure DumpGCRoots;
Var 
  i : TNbRoots;
Begin
  CWrite('GC roots:');
  CWriteLn;
  For i := 1 To NbRoots Do
  Begin
    CWrite(RAlign(IntToShortString(i),3) + ' ' + RAlign(PtrToName(Roots[i]),5));
    CWriteLn
  End
End;

Procedure AddGCRoot( r : TObjectPtr );
Begin
  CheckCondition(NbRoots < MaxGCRoots,'GC root pool is full');
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
Var 
  i : TNbRoots;
Begin
  CheckCondition(Not OngoingGC, 'GC: not reentrant');
  OngoingGC := True;
  For i := 1 To NbRoots Do
    Mark(Roots[i]);
  Sweep;
  UnMark;
  OngoingGC := False
End;


{----------------------------------------------------------------------------}
{ deep copy                                                                  }
{----------------------------------------------------------------------------}

Function DeepCopy( p : TObjectPtr ) : TObjectPtr;
Begin
  DeepCopy := DeepCopyObject(p,True,NewMemorySerial)
End;


{----------------------------------------------------------------------------}
{ initialize the memory management unit                                      }
{----------------------------------------------------------------------------}
Begin
{$IFDEF TPC}
  HeapError:=@HeapFunc;
{$ENDIF}
  MM_CURRENT_SERIAL := 0;
  InitMalloc;
  InitMemObjects;
  InitMemoryStats;
  GCInit
End.