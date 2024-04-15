{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjHead.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{             P R O L O G   O B J E C T :   C L O C K   H E A D E R          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit PObjHead;

Interface

Uses
  Memory,
  PObj,
  PObjStr,
  PObjDef;

Function Header_New : HeadPtr;

Procedure Header_GetRule( H : HeadPtr; Var R : RulePtr; Var isSys : Boolean; 
    Var isCut : Boolean );
Procedure Header_SetRule( H : HeadPtr; R : RulePtr; isSys, isCut : Boolean);

Procedure Headers_PushNew( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );


Implementation

{-----------------------------------------------------------------------}
{ constructor                                                           }
{-----------------------------------------------------------------------}

{ new clock header }
Function Header_New : HeadPtr;
Var 
  H : HeadPtr;
  ptr : TObjectPtr Absolute H;
Begin
  ptr := NewRegisteredPObject(HE,SizeOf(TObjHead),5,True,0);
  With H^ Do
  Begin
    HH_NEXT := Nil;
    HH_RULE := Nil;
    HH_FBCL := Nil;
    HH_REST := Nil;
    HH_BACK := Nil;
    HH_CLOC := 0;
    HH_ISYS := False;
    HH_ICUT := False
  End;
  Header_New := H
End;

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ get the rule data of a clock header }
Procedure Header_GetRule( H : HeadPtr; Var R : RulePtr; Var isSys : Boolean; 
    Var isCut : Boolean );
Begin
  R := H^.HH_RULE;
  isSys := H^.HH_ISYS;
  isCut := H^.HH_ICUT
End;

{ set the rule data of a clock header }
Procedure Header_SetRule( H : HeadPtr; R : RulePtr; isSys, isCut : Boolean);
Begin
  With H^ Do
  Begin
    HH_RULE := R;
    HH_ISYS := isSys;
    HH_ICUT := isCut
  End
End;

{-----------------------------------------------------------------------}
{ list                                                                  }
{-----------------------------------------------------------------------}

{ append a clock header to a list (which may be Nil) }
Procedure Headers_Push( Var list : HeadPtr; H : HeadPtr );
Begin
  H^.HH_NEXT := list;
  list := H;
  If H^.HH_NEXT <> Nil Then
    H^.HH_CLOC := H^.HH_NEXT^.HH_CLOC + 1
End;

{ create and set a clock header on top of a list of headers; list now points to
 the new clock header }
Procedure Headers_PushNew( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );
Var 
  H : HeadPtr;
Begin
  H := Header_New;
  With H^ Do
  Begin
    HH_FBCL := Fbcl
  End;
  Header_SetRule(H,R,isSys,isCut);
  Headers_Push(list,H)
End;

End.