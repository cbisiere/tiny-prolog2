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

Function Dict_GetNext( D : DictPtr) : DictPtr;
Function Dict_GetType( D : DictPtr) : TypePrologObj;
Function Dict_GetStr( D : DictPtr) : StrPtr;
Function Dict_IsGlobal( D : DictPtr) : Boolean;
Procedure Dict_SetGlobal( D : DictPtr; glob : Boolean );
Function Dict_GetTerm( D : DictPtr) : TermPtr;

Function Dict_Lookup( D : DictPtr; str : StrPtr; glob : Boolean ) : DictPtr;
Function Dict_Append( Var D : DictPtr; str : StrPtr; T : TermPtr; 
  ty : TypePrologObj; glob : Boolean ) : DictPtr;

Function Dict_StrStartsWith( D : DictPtr; E : CharSet ) : Boolean;
Function Dict_StrIsEqualTo( D : DictPtr; ps : TString ) : Boolean;


Implementation
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new dictionary entry }
Function Dict_New( ty : TypePrologObj; glob : Boolean ) : DictPtr;
Var 
  D : DictPtr;
  ptr : TObjectPtr Absolute D;
Begin
  ptr := NewRegisteredPObject(DE,SizeOf(TObjDict),3,False,0);
  With D^ Do
  Begin
    DE_NEXT := Nil;
    DE_STRI := Str_New;
    DE_TERM := Nil;
    DE_TYPE := ty;
    DE_GLOB := glob
  End;
  Dict_New := D
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ next dict entry }
Function Dict_GetNext( D : DictPtr) : DictPtr;
Begin
  Dict_GetNext := D^.DE_NEXT
End;

{ type of element }
Function Dict_GetType( D : DictPtr) : TypePrologObj;
Begin
  Dict_GetType := D^.DE_TYPE
End;

{ string }
Function Dict_GetStr( D : DictPtr) : StrPtr;
Begin
  Dict_GetStr := D^.DE_STRI
End;

{ term }
Function Dict_GetTerm( D : DictPtr) : TermPtr;
Begin
  Dict_GetTerm := D^.DE_TERM
End;

{ is a dictionary entry global? }
Function Dict_IsGlobal( D : DictPtr) : Boolean;
Begin
  Dict_IsGlobal := D^.DE_GLOB
End;

{ set the 'global' status of a dictionary entry }
Procedure Dict_SetGlobal( D : DictPtr; glob : Boolean );
Begin
  D^.DE_GLOB := glob
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ look in a dictionary D for a string; 
  return a pointer to the dictionary entry, or Nil; when found, makes the 
  entry global if global is True (as a consequence this entry will not be
  discarded during the post-clearing cleanup) }
Function Dict_Lookup( D : DictPtr; str : StrPtr; glob : Boolean ) : DictPtr;
Var
  e : DictPtr;
  Found : Boolean;
Begin
  e := D;
  Found := False;
  While (e<>Nil) And Not Found Do
  Begin
    If Str_Equal(Dict_GetStr(e),str) Then
      Found := True
    Else
      e := Dict_GetNext(e)
  End;
  If Found Then
    Dict_SetGlobal(e,Dict_IsGlobal(e) Or glob)
  Else
    e := Nil;
  Dict_Lookup := e
End;

{ append an entry to a dictionary }
Function Dict_Append( Var D : DictPtr; str : StrPtr; T : TermPtr; 
    ty : TypePrologObj; glob : Boolean ) : DictPtr;
Var e : DictPtr;
Begin
  e := Dict_New(ty,glob);
  With e^ Do
  Begin
    DE_NEXT := D;
    DE_STRI := str;
    DE_TERM := T
  End;
  D := e;
  Dict_Append := e
End;

{ does the string in a dictionary entry start with a char in a given set? 
  not UTF-8 aware }
Function Dict_StrStartsWith( D : DictPtr; E : CharSet ) : Boolean;
Begin
  Dict_StrStartsWith := Str_StartsWith(Dict_GetStr(D),E)
End;

{ is the string in a dictionary entry equal to a Pascal string? }
Function Dict_StrIsEqualTo( D : DictPtr; ps : TString ) : Boolean;
Begin
  Dict_StrIsEqualTo := Str_EqualToString(Dict_GetStr(D),ps)
End;

End.