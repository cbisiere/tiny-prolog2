{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjBter.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{           P R O L O G   O B J E C T :   L I S T   O F   T E R M S          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ term in a list, with access data }

Unit PObjBter;

Interface

Uses
  Errs,
  Memory,
  PObj,
  PObjStr,
  PObjTerm,
  PObjDef;

{ BTerm }
Function BTerm_New( T : TermPtr ) : BTermPtr;

Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );

{ list }
Function BTerms_GetNext( B : BTermPtr ) : BTermPtr;
Procedure BTerms_SetHeader( B : BTermPtr; H : HeadPtr );

Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new block for term T }
Function BTerm_New( T : TermPtr ) : BTermPtr;
Var 
  B : BTermPtr;
  ptr : TObjectPtr Absolute B;
Begin
  ptr := NewRegisteredPObject(BT,SizeOf(TObjBTerm),4,True,3);
  With B^ Do
  Begin
    BT_TERM := T;
    BT_NEXT := Nil;
    BT_ACCE := AccessIdentifier(T);
    BT_HEAD := Nil
  End;
  BTerm_New := B
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ term }
Function BTerm_GetTerm( B : BTermPtr ) : TermPtr;
Begin
  CheckCondition(B <> Nil,'BTerm_GetTerm: Nil');
  BTerm_GetTerm := B^.BT_TERM
End;

{ access identifier of a block }
Function BTerm_GetAccessTerm( B : BTermPtr ) : IdPtr;
Begin
  BTerm_GetAccessTerm := B^.BT_ACCE
End;

{ set clock header }
Procedure BTerm_SetHeader( B : BTermPtr; H : HeadPtr );
Begin
  B^.BT_HEAD := H
End;

{-----------------------------------------------------------------------}
{ list of BTerms                                                        }
{-----------------------------------------------------------------------}

{ next BTerm }
Function BTerms_GetNext( B : BTermPtr ) : BTermPtr;
Begin
  CheckCondition(B <> Nil,'BTerms_GetNext: Nil');
  BTerms_GetNext := B^.BT_NEXT
End;

{ make all terms in B point back to header H }
Procedure BTerms_SetHeader( B : BTermPtr; H : HeadPtr );
Begin
  While B <> Nil Do
  Begin
    BTerm_SetHeader(B,H);
    B := BTerms_GetNext(B)
  End
End;

End.