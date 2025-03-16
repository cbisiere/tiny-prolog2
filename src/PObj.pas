{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObj.pas                                                   }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                       P R O L O G   O B J E C T S :                        }
{                                                                            }
{          I N T E R F A C E   W I T H   M E M O R Y   M A N A G E R         }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit PObj;

Interface

Uses
  Errs,
  Memory;

{ type of allocated objects; note that SY, EQ, HE, RE are not managed by GC }
Type
  TypePrologObj = (PR, SM, WO, CM, RU, QU, SY, EQ, BT, TM, CO, FU, VA, ID, 
      CS, CI, CR, AR, DE, LL, LT, LA, HE, ST, SD, RE, OP, FI, TK);
  SetOfTypePrologObj = Set Of TypePrologObj;


Function PObjectType( p : TObjectPtr ) : TypePrologObj;

Function NewRegisteredPObject( t : TypePrologObj; b: TObjectSize; 
    n: TObjectChild; CanCopy : Boolean; d : TObjectChild ) : TObjectPtr;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ interface with the memory manager                                          }
{----------------------------------------------------------------------------}

{ string representation of these types (TP3 cannot write enumerated types);
  must match TypePrologObj }
Type
  TypePrologObjStr = Array[TypePrologObj] Of TObjectName;
Const
  ObjStr : TypePrologObjStr = ('PR', 'SM', 'WO', 'CM', 'RU', 'QU', 'SY', 'EQ', 
      'BT', 'TM', 'CO', 'FU', 'VA', 'ID', 'CS', 'CI', 'CR', 'AR', 'DE', 'LL', 
      'LT', 'LA', 'HE', 'ST', 'SD', 'RE', 'OP', 'FI', 'TK');

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
Function NewRegisteredPObject( t : TypePrologObj; b: TObjectSize; 
    n: TObjectChild; CanCopy : Boolean; d : TObjectChild ) : TObjectPtr;
Begin
  NewRegisteredPObject := NewRegisteredObject(ObjectTypeIndexOf(t),b,n,CanCopy,d)
End;

Begin
  DeclarePObjectTypes
End.