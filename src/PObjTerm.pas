{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjTerm.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                P R O L O G   O B J E C T S :   T E R M S                   }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjTerm;

Interface

Uses
  Common,
  ShortStr,
  Num,
  Errs,
  CWrites,
  Memory,
  PObj,
  PObjRest,
  PObjStr,
  PObjLisA;

{ metadata for terms; this needs to be a pointer to be properly handled by 
 our memory manager; note that basically we have NB_SEEN 'seen' indicators, 
 so they can be used independently }

Const
  NB_SEEN = 2;

  
Type
  TSeenIndex = 1..NB_SEEN;
  TSeenArray = Array[TSeenIndex] Of TSerial;
Type
  TermMetaPtr = ^TermMeta;
  TermMeta = Record { only pointers here! }
    PO_META : TObjMeta;
    { deep copied: (why??) }
    TT_CALL : ListAPtr; { list of call sites (TermPtrAddr) }
    { not deep copied: }
    TT_DISP : StrPtr; { string representation of this term }
    { extra data: }
    TT_DEPT : PosInt; { depth of the term (*n notation) }
    TT_SEEN : TSeenArray { 'seen' indicators }
  End;

{ a term; all derived classes must start with the same structure, to enable
 safe casting }
Type
  TermPtr = ^Term;
  Term = Record
    PO_META : TObjMeta; { Term is derived from TObject }
    TT_META : TermMetaPtr { members common to all 'derived classes' }
  End;

{ address of a term pointer, used for call site inventory }
Type
  TermPtrAddr = ^TermPtr;

{ term metadata object }
Function TermMeta_New : TermMetaPtr;
Function NewSerial : TSerial;

{ helpers to avoid loops when printing }
Function Term_GetSeen( T : TermPtr; k : TSeenIndex; g : TSerial ) : Boolean;
Procedure Term_SetSeen( T : TermPtr; k : TSeenIndex; g : TSerial );
Function Term_GetDisplay( T : TermPtr ) : StrPtr;
Procedure Term_SetDisplay( T : TermPtr; s : STrPtr );
Function Term_GetDepth( T : TermPtr ) : PosInt;
Procedure Term_SetDepth( T : TermPtr; depth : PosInt );
Function Term_GetListA( T : TermPtr ) : ListAPtr;
Procedure Term_SetListA( T : TermPtr; Lc : ListAPtr );
Procedure Term_SetListAWithUndo( T : TermPtr; Lc : ListAPtr; 
    L : RestPtr; Undo : Boolean );
Procedure Term_InsertOneCallSite( T : TermPtr; Lc : ListAPtr );
Procedure Term_AppendCallSitesWithUndo( T : TermPtr; Lc : ListAPtr;
    L : RestPtr; Undo : Boolean );

{ comparisons and tests }
Function Term_SameAs( T : TermPtr; T1 : TermPtr ) : Boolean;
Function Term_UnifiableWith( T : TermPtr; T1 : TermPtr ) : Boolean;
Function Term_OrderedWith( T : TermPtr; T1 : TermPtr ) : Boolean;
Function Term_OneIsNil( T : TermPtr; T1 : TermPtr ) : Boolean;

{ misc. }
Procedure SwapTerms( Var T1, T2 : TermPtr );

{ debug }
Procedure Term_Dump( T : TermPtr );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ metadata                                                              }
{-----------------------------------------------------------------------}

{ globally unique serial number used for loop detection }
Var
  CURRENT_SERIAL : TSerial;

Function NewSerial : TSerial;
Begin
  CURRENT_SERIAL := CURRENT_SERIAL + 1;
  NewSerial := CURRENT_SERIAL
End;

{ create a new term metadata object; must be copyable along with the term it
 it attached to }
Function TermMeta_New : TermMetaPtr;
Var 
  A : TermMetaPtr;
  k : TSeenIndex;
Begin
  A := TermMetaPtr(NewRegisteredPObject(TM,SizeOf(TermMeta),2,True,1));
  With A^ Do
  Begin
    TT_CALL := Nil;
    TT_DISP := Nil;
    TT_DEPT := 0;
    For k := 1 To NB_SEEN Do
      TT_SEEN[k] := 0
  End;
  TermMeta_New := A
End;

{-----------------------------------------------------------------------}
{ access to metadata through terms                                      }
{-----------------------------------------------------------------------}

{ has term been seen? }
Function Term_GetSeen( T : TermPtr; k : TSeenIndex; g : TSerial ) : Boolean;
Begin
  Term_GetSeen := T^.TT_META^.TT_SEEN[k] = g
End;

{ note that a term has been seen }
Procedure Term_SetSeen( T : TermPtr; k : TSeenIndex; g : TSerial );
Begin
  T^.TT_META^.TT_SEEN[k] := g
End;

{ get string representation of term T, or Nil if not available yet }
Function Term_GetDisplay( T : TermPtr ) : StrPtr;
Begin
  Term_GetDisplay := T^.TT_META^.TT_DISP
End;

{ set string representation of term T, or Nil if not available }
Procedure Term_SetDisplay( T : TermPtr; s : StrPtr );
Begin
  T^.TT_META^.TT_DISP := s
End;

{ depth of a seen terms }
Function Term_GetDepth( T : TermPtr ) : PosInt;
Begin
  Term_GetDepth := T^.TT_META^.TT_DEPT
End;

{ set the depth of a seen term }
Procedure Term_SetDepth( T : TermPtr; depth : PosInt );
Begin
  T^.TT_META^.TT_DEPT := depth
End;

{ get the list of addresses of call sites }
Function Term_GetListA( T : TermPtr ) : ListAPtr;
Begin
  Term_GetListA := T^.TT_META^.TT_CALL
End;

{ set the the list of addresses of call sites }
Procedure Term_SetListAWithUndo( T : TermPtr; Lc : ListAPtr; 
    L : RestPtr; Undo : Boolean );
Begin
  Rest_SetMem(L,TObjectPtr(T^.TT_META),TObjectPtr(T^.TT_META^.TT_CALL),
      TObjectPtr(Lc),Undo)
End;

{ set the the list of addresses of call sites, no undo }
Procedure Term_SetListA( T : TermPtr; Lc : ListAPtr );
Var
  L : RestPtr;
Begin
  L := Nil;
  Term_SetListAWithUndo(T,Lc,L,False)
End;

{ register a new call site for term T }
Procedure Term_InsertOneCallSite( T : TermPtr; Lc : ListAPtr );
Begin
  ListA_SetNext(Lc,Term_GetListA(T));
  Term_SetListA(T,Lc)
End;

{ append a list of call sites at the end of term T's call sites }
Procedure Term_AppendCallSitesWithUndo( T : TermPtr; Lc : ListAPtr;
    L : RestPtr; Undo : Boolean );
Var
  La : ListAPtr;
Begin
  La := Term_GetListA(T);
  If La = Nil Then
    Term_SetListAWithUndo(T,Lc,L,Undo)
  Else
  Begin
    La := ListA_Last(La);
    ListA_SetNextWithUndo(La,Lc,L,Undo)
  End
End;

{----------------------------------------------------------------------------}
{ comparison                                                                 }
{----------------------------------------------------------------------------}

{ return true if T and T1 are equal, that is: same variable, same
  identifier or same constant value; an invariant (unique constant 
  values and terms) simplify the test greatly, as testing checking 
  pointers are equal is enough }
Function Term_SameAs( T : TermPtr; T1 : TermPtr ) : Boolean;
Begin
  Term_SameAs := T = T1
End;

{ are two terms possibly unifiable? if not, there is not point in copying
 a rule, etc.; note that since we make sure that a given constant value 
 (identifiers, numbers, strings) is represented by exactly one term,
 comparing pointers is fine even for constants }
Function Term_UnifiableWith( T : TermPtr; T1 : TermPtr ) : Boolean;
Var 
  Ok : Boolean;
Begin
  CheckCondition((T<>Nil) Or (T1<>Nil),
    'Call to Term_UnifiableWith with two Nil terms'); { FIXME: is it really a problem?}
  Ok := Term_SameAs(T,T1) Or (T=Nil) Or (T1=Nil); { FIXME: why Nil? }
  Term_UnifiableWith := Ok
End;

{ arbitrary order on terms: are two terms ordered? }
Function Term_OrderedWith( T : TermPtr; T1 : TermPtr ) : Boolean;
Begin
  CheckCondition((T <> Nil) And (T1 <> Nil),'Undefined order');
  Term_OrderedWith := ObjectGuid(TObjectPtr(T)) <= ObjectGuid(TObjectPtr(T1))
End;

{ return true if T xor T1 is Nil }
Function Term_OneIsNil( T : TermPtr; T1 : TermPtr ) : Boolean;
Begin
  Term_OneIsNil := (T = Nil) Xor (T1 = Nil)
End;

{ swap two terms }
Procedure SwapTerms( Var T1, T2 : TermPtr );
Var 
  Tmp : TermPtr;
Begin
  Tmp := T1;
  T1 := T2;
  T2 := Tmp
End;

{----------------------------------------------------------------------------}
{ dump                                                                       }
{----------------------------------------------------------------------------}

Procedure Term_Dump( T : TermPtr );
Begin
  DumpObject(TObjectPtr(T),False);
  CWrite(' TT_CALL: ');
  ListA_DumpList(Term_GetListA(T));
  CWriteLn
End;

Begin
  CURRENT_SERIAL := 0
End.