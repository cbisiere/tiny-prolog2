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


{----------------------------------------------------------------------------}
{ low level memory allocation procedures                                     }
{----------------------------------------------------------------------------}

{ type of allocated objects; note that SY, EQ, HE, RE are not managed by GC }
Type
  TypePrologObj = (PR, RU, QU, SY, EQ, BT, CO, FU, VA, ID, CS, CI, CR, DE, HE, 
      ST, SD, RE, OP, TK);

{ string representation of these types (TP3 cannot write enumerated types);
  must match TypePrologObj }
Type
  TypePrologObjStr = Array[TypePrologObj] Of String[2];
Const
  ObjStr : TypePrologObjStr = ('PR', 'RU', 'QU', 'SY', 'EQ', 'BT', 'CO', 'FU', 
      'VA', 'ID', 'CS', 'CI', 'CR', 'DE', 'HE', 'ST', 'SD', 'RE', 'OP', 'TK');

{----------------------------------------------------------------------------}
{ memory allocation stats                                                    }
{----------------------------------------------------------------------------}

Var 
  mem : LongLongInt; { total number of bytes allocated }
  PObjCount : Array[TypePrologObj] of LongInt;

Procedure InitMemoryStats;
Var 
  t : TypePrologObj;
Begin
  mem := 0;
  For t := PR To TK Do
    PObjCount[t] := 0
End;

Procedure PrintMemoryStats;
Var t : TypePrologObj;
Begin
  CWrite('Bytes allocated: ' + LongLongIntToStr(mem));
  CWriteLn;
  For t := PR To TK Do
  Begin
    CWrite(' ' + ObjStr[t] + ': ' + RAlign(LongIntToStr(PObjCount[t]),5));
    CWriteLn
  End
End;

{ update memory stat with delta objects of a given size }
Procedure UpdateMemoryStats( t : TypePrologObj; delta : LongInt; size : Integer );
Begin
  PObjCount[t] := PObjCount[t] + delta;
  CheckCondition(PObjCount[t] >= 0,'negative number of objects');
  mem := mem + delta*size;
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

Function MarkToStr( m : TypeMark ) : TString;
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
  POBJECT_MAGIC_NUMBER = 12345;

{ Prolog object's metadata for object management (cloning, debugging, etc.) }
Type
  TPObjPtr = ^TPObj;
  { metadata }
  TObjMeta = Record
    PO_MAGI : Integer;       { magic number (debug) }
    PO_FREE : Boolean;       { object's memory has been freed (debug) }
    PO_TYPE : TypePrologObj; { type of object }
    PO_GUID : LongInt;       { Prolog object globally unique identifier (for convenience and sorting) }
    PO_SIZE : Integer;       { size in bytes (including metadata) }
    PO_NPTR : Byte;          { number of PObject pointers (which must immediately follow the metadata) }
    { deep copy }
    PO_DPOK : Boolean;       { deep copy of this object is allowed }
    PO_DEEP : Boolean;       { has been visited during the current deep copy operation }
    PO_COPY : TPObjPtr;      { pointer to a copy made during a deep copy; warning: copy may have been GC'ed }
    PO_CUID : LongInt;       { GUID of the copy, or 0; for display purpose, as the copy may not exist }
    PO_NCOP : Integer;       { copy number - FIXME: may overflow }
    PO_NDEE : Byte;          { number of PObject pointers to deep copy (must be less of equal to PO_NPTR) }
    { garbage collection }
    PO_MARK : TypeMark;      { GC mark }
    PO_NEXT : TPObjPtr       { GC list of allocated objects }
  End;
  TPObj = Record
    PO_META : TObjMeta;
    PO_PTRS : Array[1..MaxChildren] Of TPObjPtr { pointers to child objects }
  End;

{----------------------------------------------------------------------------}
{ accessors                                                                  }
{----------------------------------------------------------------------------}

Function ObjectMagic( p : TPObjPtr ) : Integer;
Begin
  ObjectMagic := p^.PO_META.PO_MAGI
End;

Function IsObject( p : TPObjPtr ) : Boolean;
Begin
  IsObject := ObjectMagic(p) = POBJECT_MAGIC_NUMBER
End;

Procedure SetObjectMagic( p : TPObjPtr; magic : Integer );
Begin
  p^.PO_META.PO_MAGI := magic
End;

Function ObjectIsFree( p : TPObjPtr ) : Boolean;
Begin
  ObjectIsFree := p^.PO_META.PO_FREE
End;

Procedure SetObjectFree( p : TPObjPtr; IsFree : Boolean );
Begin
  p^.PO_META.PO_FREE := IsFree
End;

Function ObjectType( p : TPObjPtr ) : TypePrologObj;
Begin
  ObjectType := p^.PO_META.PO_TYPE
End;

Procedure SetObjectType( p : TPObjPtr; t : TypePrologObj );
Begin
  p^.PO_META.PO_TYPE := t
End;

Function ObjectSize( p : TPObjPtr ) : Integer;
Begin
  ObjectSize := p^.PO_META.PO_SIZE
End;

Procedure SetObjectSize( p : TPObjPtr; size : Integer );
Begin
  p^.PO_META.PO_SIZE := size
End;

Function ObjectNbChildren( p : TPObjPtr ) : Byte;
Begin
  ObjectNbChildren := p^.PO_META.PO_NPTR
End;

Function ObjectNbChildrenToCopy( p : TPObjPtr ) : Byte;
Begin
  ObjectNbChildrenToCopy := p^.PO_META.PO_NDEE
End;

Function ObjectChild( p : TPObjPtr; i : Byte ) : TPObjPtr;
Begin
  ObjectChild := p^.PO_PTRS[i]
End;

Procedure SetObjectChild( p : TPObjPtr; i : Byte; child : TPObjPtr );
Begin
  p^.PO_PTRS[i] := child
End;

Function ObjectCopyNumber( p : TPObjPtr ) : Integer;
Begin
  ObjectCopyNumber := p^.PO_META.PO_NCOP
End;

Function ObjectGuid( p : TPObjPtr ) : LongInt;
Begin
  ObjectGuid := p^.PO_META.PO_GUID
End;

Procedure SetObjectGuid( p : TPObjPtr; guid : LongInt);
Begin
  p^.PO_META.PO_GUID := guid
End;

Function ObjectIsMarked( p : TPObjPtr ) : Boolean;
Begin
  ObjectIsMarked := IsMark(p^.PO_META.PO_MARK)
End;

Procedure SetObjectMark( p : TPObjPtr; Marked : Boolean);
Begin
  SetMark(p^.PO_META.PO_MARK,Marked)
End;

Function ObjectNext( p : TPObjPtr ) : TPObjPtr;
Begin
  ObjectNext := p^.PO_META.PO_NEXT
End;

Procedure SetObjectNext( p,nxt : TPObjPtr );
Begin
  p^.PO_META.PO_NEXT := nxt
End;

{----------------------------------------------------------------------------}
{ debug / dump                                                               }
{----------------------------------------------------------------------------}

Function FindObjectById( guid : LongInt ) : TPObjPtr; Forward;

{ global object ID to string }
Function GuidToStr( guid : LongInt ) : TString;
Begin
  GuidToStr := '#' + LongIntToStr(guid)
End;

{ object pointer to object name }
Function PtrToName( p : TPObjPtr ) : TString;
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
    s := GuidToStr(guid);
    { mark unregistered objects }
    If FindObjectById(guid) = Nil Then { Warning: time consuming }
      s := s + '(!)'
  End;
  PtrToName := s
End;

Procedure WriteExtraData( p : TPObjPtr ); forward;

{ dump an object, possibly with extra data }
Procedure DumpObject( p : TPObjPtr; extra : Boolean );
Var 
  i : Byte;
  child : TPObjPtr;
Begin
  CWrite(RAlign(PtrToName(p),5) + ' : ');
  CWrite(RAlign(IntToStr(ObjectSize(p)),3) + ' ');
  With p^.PO_META Do
  Begin
    CWrite(ObjStr[PO_TYPE] + ' ' + MarkToStr(PO_MARK) + ' ');
    CWrite(IntToStr(PO_NCOP) + ' ' + IntToStr(Ord(PO_DEEP)) + ' ');
    CWrite(RAlign(GuidToStr(PO_CUID),5))
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
    CWrite(' ');
    WriteExtraData(p)
  End;
  CWriteLn
End;

{ check a memory location has a chance to be a legit Prolog object }
Procedure CheckIsPObj( p : TPObjPtr; prompt : TString );
Begin
  CheckCondition(IsObject(p),
    prompt + ': not a Prolog object')
End;

{ dump all the registered Prolog objects }
Procedure DumpObjects( p : TPObjPtr; extra : Boolean );
Begin
  If p<>Nil Then
  Begin
    CheckIsPObj(p,'DumpObjects');
    DumpObject(p,extra);
    DumpObjects(ObjectNext(p),extra)
  End
End;


{----------------------------------------------------------------------------}
{ list of all allocated objects                                              }
{----------------------------------------------------------------------------}

Var 
  AllocHead : TPObjPtr; { list of all allocations }
  OngoingGC : Boolean;  { is a GC ongoing? }
  DoRegister : Boolean; { should new objects be registered? False during debug }

Procedure InitAlloc;
Begin
  AllocHead := Nil;
  OngoingGC := False;
  DoRegister := True
End;

{ dump all registered objects }
Procedure DumpRegisteredObject;
Begin
  DumpObjects(AllocHead,True)
End;

{ find the object with guid id in the object store, or Nil }
Function FindObjectById; (*( guid : LongInt ) : TPObjPtr;*)
Var
  p : TPObjPtr;
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
Procedure RegisterObject( p : TPObjPtr );
Var
  nxt : TPObjPtr;
  guid : LongInt;
Begin
  CheckCondition(Not OngoingGC Or Not DoRegister,
      'RegisterObject: object registration during GC');
  If DoRegister Then 
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
Function UnregisterObject( prev,p : TPObjPtr ) : TPObjPtr;
Var 
  nxt : TPObjPtr;
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

{ low-level memory function for typed Prolog objects; see PObjNew.pas }
Function PObjNew( t : TypePrologObj ) : TPObjPtr; Forward;
Procedure PObjDispose( t : TypePrologObj; p : TPObjPtr ); Forward;
Function PObjSizeOf( t : TypePrologObj; p : TPObjPtr ) : Integer; Forward;

{ allocate memory on the heap for an object of type t; size returns 
  the size of the allocated object, in bytes }
Function NewObject( t : TypePrologObj ) : TPObjPtr;
Var 
  p : TPObjPtr;
  ptr : Pointer Absolute p;
  size : Integer;
Begin
  p := PObjNew(t);
  { GC cannot only be run at some specific execution points, so in case of 
    OOM we just abort }
  CheckCondition(p<>Nil,'Memory exhausted');
  size := MemSizeOf(ptr,PObjSizeOf(t,p)); { true allocated size }
  FillChar(p^,size,0);
  { set the bare minimum object data }
  SetObjectMagic(p,POBJECT_MAGIC_NUMBER);
  SetObjectFree(p,False);
  SetObjectType(p,t);
  SetObjectSize(p,size);
  { accounting }
  UpdateMemoryStats(t,1,size);
  NewObject := p
End;

{ free a Prolog object }
Procedure FreeObject( Var p : TPObjPtr );
Begin
  UpdateMemoryStats(ObjectType(p),-1,ObjectSize(p));
  SetObjectFree(p,True);
  PObjDispose(ObjectType(p),p);
  p := Nil { prevent dangling pointers }
End;

{ clone object p in memory, and return the clone }
Function CloneObject( p : TPObjPtr ) : TPObjPtr;
Var 
  pc : TPObjPtr;
Begin
  pc := PObjNew(ObjectType(p));
  Move(p^,pc^,ObjectSize(p));
  UpdateMemoryStats(ObjectType(pc),1,ObjectSize(pc));
  CloneObject := pc
End;

{----------------------------------------------------------------------------}
{ operations on registered objects                                           }
{----------------------------------------------------------------------------}

{ free a registered object p, and return the next object }
Function FreeRegisteredObject( prev : TPObjPtr; Var p : TPObjPtr ) : TPObjPtr;
Var 
  nxt : TPObjPtr;
Begin
  CheckIsPObj(p,'FreeRegisteredObject');
  CheckCondition(Not ObjectIsFree(p),'FreeRegisteredObject: double free');
  nxt := UnregisterObject(prev,p);
  FreeObject(p);
  FreeRegisteredObject := nxt
End;


{ allocate a Prolog object of size s; metadata are followed by n Prolog child 
  object pointers; the first d child objects of these n are copied when the 
  object is deep copied (but only if deep copy is allowed for that object: 
  CanCopy) }
Function NewRegisteredObject( t : TypePrologObj; n: Byte; CanCopy : Boolean; 
    d : Byte ) : TPObjPtr;
Var 
  p : TPObjPtr;
Begin
  p := NewObject(t);
  With p^.PO_META Do
  Begin
    PO_NPTR := n;
    { deep copy metadata: }
    PO_DPOK := CanCopy;
    PO_DEEP := False;
    PO_COPY := Nil;
    PO_CUID := 0;
    PO_NCOP := 0;
    PO_NDEE := d
  End;
  RegisterObject(p);
  NewRegisteredObject := p
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
  pc := CloneObject(p);
  RegisterObject(pc);
  With pc^.PO_META Do { new object is a copy }
  Begin
    PO_DEEP := True;
    PO_COPY := Nil;
    PO_CUID := 0;
    PO_NCOP := PO_NCOP + 1 { note it is an additional copy }
  End;
  With p^.PO_META Do { old object has been copied }
  Begin
    PO_DEEP := True;
    PO_COPY := pc;  { link old -> copy }
    PO_CUID := ObjectGuid(pc)
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
      If Not PO_DPOK Then { deep copy not authorized: return the object itself }
        pc := p
      Else If PO_DEEP Then { object has already been visited during this deep copy }
      Begin 
        If PO_COPY=Nil Then { is a copy }
          pc := p
        Else
          pc := PO_COPY { has been copied }
      End
      Else
      Begin
        pc := CopyObject(p);
        For i := 1 To ObjectNbChildrenToCopy(p) Do
          SetObjectChild(pc,i,DeepCopyObject(ObjectChild(p,i)))
      End
    End
  End;
  DeepCopyObject := pc
End;

{ reset the deep copy state of p and of all objects that are reachable 
  from p through children subject to deep copy }
Procedure PrepareDeepCopy( p : TPObjPtr );
Var 
  i : Byte;
Begin
  If p<>Nil Then
  Begin
    With p^.PO_META Do
    Begin
      If PO_DPOK And PO_DEEP Then
      Begin
        PO_DEEP := False;
        PO_COPY := Nil;
        PO_CUID := 0;
        For i := 1 To ObjectNbChildrenToCopy(p) Do
          PrepareDeepCopy(ObjectChild(p,i))
      End
    End
  End
End;


{----------------------------------------------------------------------------}
{ mark & sweep                                                               }
{----------------------------------------------------------------------------}

{ free all unmarked objects and associated memory management record }
Procedure Sweep;
Var 
  p, prev : TPObjPtr;
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
Procedure MarkOneObject( p : TPObjPtr );
Begin
  SetObjectMark(p,True)
End;

{ is p a reference to an object that must be marked? }
Function Markable( p : TPObjPtr ) : Boolean;
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
Procedure Mark( p : TPObjPtr );
Var 
  i : Byte;
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
  Roots : Array[1..MaxGCRoots] Of TPObjPtr;

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
    CWrite(RAlign(IntToStr(i),3) + ' ' + RAlign(PtrToName(Roots[i]),5));
    CWriteLn
  End
End;

Procedure AddGCRoot( r : TPObjPtr );
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

Function DeepCopy( p : TPObjPtr ) : TPObjPtr;
Var 
  pc : TPObjPtr;
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
