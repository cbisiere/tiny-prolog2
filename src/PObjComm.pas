{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjComm.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  P R O L O G   O B J E C T :   C O M M E N T               }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjComm;

Interface

Uses
  Memory,
  PObj,
  PObjStr,
  PObjTerm,
  PObjFCVI,
  PObjDef;

Function Comment_New( C : ConstPtr ) : CommPtr;
Function Comment_GetConst( Comm : CommPtr ) : ConstPtr;

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new comment }
Function Comment_New( C : ConstPtr ) : CommPtr;
Var 
  Comm : CommPtr;
  ptr : TObjectPtr Absolute Comm;
Begin
  ptr := NewRegisteredPObject(CM,SizeOf(TObjComm),1,True,1);
  With Comm^ Do
  Begin
    CM_COMM := C
  End;
  Comment_New := Comm
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ string }
Function Comment_GetConst( Comm : CommPtr ) : ConstPtr;
Begin
  Comment_GetConst := Comm^.CM_COMM
End;

End.