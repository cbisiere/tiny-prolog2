{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjDict.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{      P R O L O G   O B J E C T S :   D I C T I O N A R Y   E N T R Y       }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjDict;

Interface

Uses
  ShortStr,
  Memory,
  PObj,
  PObjStr;

{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ dictionary entry }
Type
  DictPtr = ^TObjDict;
  TObjDict = Record
    PO_META : TObjMeta;
    { not deep copied: }
    DE_NEXT : DictPtr;
    DE_TERM : TermPtr; { term for which this entry was initially created }
    DE_STRI : StrPtr;
    { extra data: }
    DE_TYPE : TypePrologObj;
    DE_GLOB : Boolean { is this entry global, and cannot be discarded? }
  End;


Function DictIsGlobal( D : DictPtr) : Boolean;
Procedure DictSetGlobal( D : DictPtr; glob : Boolean );

Function DictLookup( start,stop : DictPtr; str : StrPtr; 
    glob : Boolean ) : DictPtr;
Function DictAppend( Var list : DictPtr; str : StrPtr; T : TermPtr; 
  ty : TypePrologObj; glob : Boolean ) : DictPtr;

Function DictStrStartWith( D : DictPtr; E : CharSet ) : Boolean;
Function DictStrEqualTo( D : DictPtr; ps : TString ) : Boolean;


Implementation
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new dictionary entry }
Function NewDictEntry( ty : TypePrologObj; glob : Boolean ) : DictPtr;
Var 
  D : DictPtr;
  ptr : TObjectPtr Absolute D;
Begin
  ptr := NewRegisteredPObject(DE,SizeOf(TObjDict),3,False,0);
  With D^ Do
  Begin
    DE_NEXT := Nil;
    DE_STRI := NewString;
    DE_TERM := Nil;
    DE_TYPE := ty;
    DE_GLOB := glob
  End;
  NewDictEntry := D
End;

{-----------------------------------------------------------------------}
{ accessors                                                             }
{-----------------------------------------------------------------------}

{ is a dictionary entry global? }
Function DictIsGlobal( D : DictPtr) : Boolean;
Begin
  DictIsGlobal := D^.DE_GLOB
End;

{ set the 'global' status of a dictionary entry }
Procedure DictSetGlobal( D : DictPtr; glob : Boolean );
Begin
  D^.DE_GLOB := glob
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ look in a dictionary for a string, from start to stop (excluding stop); 
  return a pointer to the dictionary entry, or Nil; when found, makes the 
  entry global if global is True (as a consequence this entry will not be
  discarded during the post-clearing cleanup) }
Function DictLookup( start,stop : DictPtr; str : StrPtr; 
    glob : Boolean ) : DictPtr;
Var
  e : DictPtr;
  Found : Boolean;
Begin
  e := start;
  Found := False;
  While (e<>Nil) And (e<>stop) And Not Found Do
  Begin
    If StrEqual(e^.DE_STRI,str) Then
      Found := True
    Else
      e := e^.DE_NEXT
  End;
  If Found Then
    DictSetGlobal(e,DictIsGlobal(e) Or glob)
  Else
    e := Nil;
  DictLookup := e
End;

{ append an entry to a dictionary }
Function DictAppend( Var list : DictPtr; str : StrPtr; T : TermPtr; 
    ty : TypePrologObj; glob : Boolean ) : DictPtr;
Var e : DictPtr;
Begin
  e := NewDictEntry(ty,glob);
  With e^ Do
  Begin
    DE_NEXT := list;
    DE_STRI := str;
    DE_TERM := T
  End;
  list := e;
  DictAppend := e
End;

{ does the string in a dictionary entry start with a char in a given set? 
  not UTF-8 aware }
Function DictStrStartWith( D : DictPtr; E : CharSet ) : Boolean;
Begin
  DictStrStartWith := StrStartsWith(D^.DE_STRI,E)
End;

{ is the string in a dictionary entry equal to a Pascal string? }
Function DictStrEqualTo( D : DictPtr; ps : TString ) : Boolean;
Begin
  DictStrEqualTo := StrEqualTo(D^.DE_STRI,ps)
End;

End.