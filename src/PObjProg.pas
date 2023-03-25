{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Objects.pas                                                }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{               P R O L O G   O B J E C T S :   P R O G R A M                }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }


{-----------------------------------------------------------------------}
{ types                                                                 }
{-----------------------------------------------------------------------}

{ list of terms }
Type 
  BTermPtr = ^TObjBTerm;
  TObjBTerm = Record
    PO_META : TObjMeta;
    BT_NEXT : BTermPtr; { next element }
    BT_TERM : TermPtr; { term }
    BT_CONS : ConstPtr {access constant or 0 }
  End;

{ rule }
Type 
  RulePtr = ^TObjRule;
  RuType = (RTYPE_AUTO, RTYPE_USER);
  TObjRule = Record
    PO_META : TObjMeta;
    RU_NEXT : RulePtr; { next rule }
    RU_FBTR : BTermPtr; { list of terms (the first is the rule head) }
    RU_SYST : EqPtr; { list of equation or inequation in the rule; Warning: not GC}
    RU_FVAR : Integer; { variables local to the rule: first index }
    RU_LVAR : Integer; { variables local to the rule: last index }
    RU_TYPE : RuType { type of rule: read from init file, or user }
  End;

{ query }
Type 
  QueryPtr = ^TObjQuery;
  TObjQuery = Record
    PO_META : TObjMeta;
    QU_NEXT : QueryPtr; { next query }
    QU_FRUL : RulePtr; { first rule to try }
    QU_LRUL : RulePtr; { last rule to try }
    QU_FBTR : BTermPtr; { terms in the query }
    QU_SYST : EqPtr; { list of equation or inequation in the query }
    QU_FVAR : Integer; { variables local to the query: first index }
    QU_LVAR : Integer; { variables local to the query: last index }
    QU_FCON : Integer { index of the first constant in the query }
  End;

{ program }
Type 
  ProgPtr = ^TObjProg;
  TObjProg = Record
    PO_META : TObjMeta;
    PP_FQRY : QueryPtr; { first query }
    PP_LQRY : QueryPtr; { last query }
    PP_FRUL : RulePtr; { first rule }
    PP_LRUL : RulePtr; { last rule }
    PP_LVAR : Integer; { variables local to the program: last index }
    PP_LCON : Integer { constants local to the program: last index }
  End;


{-----------------------------------------------------------------------}
{ create / destroy                                                      }
{-----------------------------------------------------------------------}

{ new block }
Function NewBTerm : BTermPtr;
Var 
  B : BTermPtr;
  ptr : TPObjPtr Absolute B;
Begin
  ptr := NewPrologObject(BT, SizeOf(TObjBTerm), 3);
  With B^ Do
  Begin
    BT_TERM := Nil;
    BT_NEXT := Nil;
    BT_CONS := Nil
  End;
  NewBTerm := B
End;

{ new rule }
Function NewRule( RuleType : RuType ) : RulePtr;
Var 
  R : RulePtr;
  ptr : TPObjPtr Absolute R;
Begin
  ptr := NewPrologObject(RU, SizeOf(TObjRule), 3);
  With R^ Do
  Begin
    RU_NEXT := Nil;
    RU_FBTR := Nil;
    RU_SYST := Nil;
    RU_FVAR := 0;
    RU_LVAR := 0;
    RU_TYPE := RuleType
  End;
  NewRule := R
End;

{ new program }
Function NewProgram : ProgPtr;
Var 
  P : ProgPtr;
  ptr : TPObjPtr Absolute P;
Begin
  ptr := NewPrologObject(PR, SizeOf(TObjProg), 4);
  With P^ Do
  Begin
    PP_FRUL := Nil;
    PP_LRUL := Nil;
    PP_FQRY := Nil;
    PP_LQRY := Nil;
    PP_LVAR := NbVar; { TODO }
    PP_LCON := NbConst
  End;
  NewProgram := P
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ next block }
Function NextTerm( B : BTermPtr ) : BTermPtr;
Begin
  CheckCondition(B <> Nil,'Cannot compute the next term of Nil');
  NextTerm := B^.BT_NEXT;
End;

{ access constant of a block }
Function AccessTerm( B : BTermPtr ) : ConstPtr;
Begin
  AccessTerm := B^.BT_CONS
End;

{ next rule }
Function NextRule( R : RulePtr ) : RulePtr;
Begin
  NextRule := R^.RU_NEXT
End;

{ last rule in a list }
Function LastRule( R : RulePtr ) : RulePtr;
Begin
  While (NextRule(R) <> Nil) Do
    R := NextRule(R);
  LastRule := R
End;

{ last query in a list }
Function LastQuery( Q : QueryPtr ) : QueryPtr;
Begin
  While (Q^.QU_NEXT <> Nil) Do
    Q := Q^.QU_NEXT;
  LastQuery := Q
End;
