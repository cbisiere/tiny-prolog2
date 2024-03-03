{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObj.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                P R O L O G   O B J E C T S :   C O M M O N                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObj;

Interface

Uses
  Errs,
  Memory;

{ type of allocated objects; note that SY, EQ, HE, RE are not managed by GC }
Type
  TypePrologObj = (PR, RU, QU, SY, EQ, BT, CO, FU, VA, ID, CS, CI, CR, DE, HE, 
      ST, SD, RE, OP, TK);

{ term: constant, variable, functional symbol }
Type
  TermPtr = TObjectPtr;


Function PObjectType( p : TObjectPtr ) : TypePrologObj;

Function NewRegisteredPObject( t : TypePrologObj; b: TObjectSize; n: Byte; 
    CanCopy : Boolean; d : Byte ) : TObjectPtr;

Function SameTerms( T1,T2 : TermPtr ) : Boolean;
Function OrderedTerms( T1,T2 : TermPtr ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}


{----------------------------------------------------------------------------}
{ interface with the memory manger                                           }
{----------------------------------------------------------------------------}

{ string representation of these types (TP3 cannot write enumerated types);
  must match TypePrologObj }
Type
  TypePrologObjStr = Array[TypePrologObj] Of TObjectName;
Const
  ObjStr : TypePrologObjStr = ('PR', 'RU', 'QU', 'SY', 'EQ', 'BT', 'CO', 'FU', 
      'VA', 'ID', 'CS', 'CI', 'CR', 'DE', 'HE', 'ST', 'SD', 'RE', 'OP', 'TK');

{ mapping between types: Internal Index <-> Prolog }
Var
  TypePrologObjMap : Array[TypePrologObj] Of TObjectTypeIndex;
  ObjectTypeIndexMap : Array[TObjectTypeIndex] Of TypePrologObj; { small waste }


{ declare all Prolog object types }
Procedure DeclarePObjectTypes;
Var
  t : TypePrologObj;
Begin
  For t := PR To TK Do
    If Not (t In [CS,CI,CR]) Then
    Begin
      TypePrologObjMap[t] := DeclareObjectType(ObjStr[t]);
      ObjectTypeIndexMap[TypePrologObjMap[t]] := t
    End
End;

{ mapping between TypePrologObj and TObjectTypeIndex }
Function ObjectTypeIndexOf( t : TypePrologObj ) : TObjectTypeIndex;
Begin
  ObjectTypeIndexOf := TypePrologObjMap[t]
End;

{ mapping between TypePrologObj and TObjectTypeIndex }
Function TypePrologObjOf( t : TObjectTypeIndex ) : TypePrologObj;
Begin
  TypePrologObjOf := ObjectTypeIndexMap[t]
End;

Function PObjectType( p : TObjectPtr ) : TypePrologObj;
Begin
  PObjectType := TypePrologObjOf(ObjectType(p))
End;

{ allocate a Prolog object of type t and size b; metadata are followed by n 
 Prolog child object pointers; the first d child objects of these n are copied 
 when the object is deep copied (but only if deep copy is allowed for that 
 object: CanCopy) }
Function NewRegisteredPObject( t : TypePrologObj; b: TObjectSize; n: Byte;  
    CanCopy : Boolean; d : Byte ) : TObjectPtr;
Begin
  NewRegisteredPObject := NewRegisteredObject(ObjectTypeIndexOf(t),b,n,CanCopy,d)
End;


{----------------------------------------------------------------------------}
{ term comparison                                                            }
{----------------------------------------------------------------------------}

{ return true if T1 and T2 are equal, that is: same variable, same
  identifier or same constant value; an invariant (unique constant 
  values and terms) simplify the test greatly, as testing checking 
  pointers are equal is enough }
Function SameTerms( T1,T2 : TermPtr ) : Boolean;
Begin
  SameTerms := T1 = T2
End;

{ arbitrary order on terms: are two terms ordered? }
Function OrderedTerms( T1,T2 : TermPtr ) : Boolean;
Begin
  CheckCondition((T1 <> Nil) And (T2 <> Nil),'Undefined order');
  OrderedTerms := ObjectGuid(T1) <= ObjectGuid(T2)
End;

Begin
  DeclarePObjectTypes
End.