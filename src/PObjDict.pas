{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjDict.pas                                               }
{   Author      : Christophe Bisière                                         }
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
    DE_TYPE : TypePrologObj
  End;

  
{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ create a new dictionary entry }
Function NewDictEntry( ty : TypePrologObj ) : DictPtr;
Var 
  D : DictPtr;
  ptr : TPObjPtr Absolute D;
Begin
  ptr := NewPrologObject(DE,SizeOf(TObjDict),3,False,0);
  With D^ Do
  Begin
    DE_NEXT := Nil;
    DE_STRI := NewString;
    DE_TERM := Nil;
    DE_TYPE := ty
  End;
  NewDictEntry := D
End;


{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ look in a dictionary for a string, from start to stop (excluding stop); 
  return a pointer to the dictionary entry, or Nil }
Function DictLookup( start,stop : DictPtr; str : StrPtr ) : DictPtr;
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
  If Not Found Then
    e := Nil;
  DictLookup := e
End;

{ append an entry to a dictionary }
Function DictAppend( Var list : DictPtr; str : StrPtr; T : TermPtr; ty : TypePrologObj) : DictPtr;
Var e : DictPtr;
Begin
  e := NewDictEntry(ty);
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
Function DictStrStartWith( DC : DictPtr; E : CharSet ) : Boolean;
Begin
  DictStrStartWith := StrStartsWith(DC^.DE_STRI,E)
End;

{ is the string in a dictionary entry equal to a Pascal string? }
Function DictStrEqualTo( DC : DictPtr; ps : AnyStr ) : Boolean;
Begin
  DictStrEqualTo := StrEqualTo(DC^.DE_STRI,ps)
End;
