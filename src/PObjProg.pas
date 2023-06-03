{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjProg.pas                                               }
{   Author      : Christophe Bisiere                                         }
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

Type 
  BTermPtr = ^TObjBTerm;
  RulePtr = ^TObjRule;
  QueryPtr = ^TObjQuery;
  ProgPtr = ^TObjProg;
  HeadPtr = ^TObjHead;

  { list of terms }
  TObjBTerm = Record
    PO_META : TObjMeta;
    { deep copied: }
    BT_NEXT : BTermPtr; { next element }
    BT_TERM : TermPtr; { term }
    BT_ACCE : IdPtr; { access identifier or Nil }
    { not deep copied: }
    BT_HEAD : HeadPtr { clock header point to the rule containing this term }
  End;

  { rule }
  RuType = (RTYPE_AUTO, RTYPE_USER);
  TObjRule = Record
    PO_META : TObjMeta;
    { deep copied: }
    RU_NEXT : RulePtr; { next rule }
    RU_FBTR : BTermPtr; { list of terms (the first is the rule head) }
    RU_SYST : EqPtr; { list of equation or inequation in the rule; Warning: not GC }
    { not deep copied: }
    RU_FVAR : DictPtr; { where to start looking up }
    RU_LVAR : DictPtr; { where to stop looking up }
    { extra data: }
    RU_ACUT : Boolean; { rule queue contains a cut }
    RU_TYPE : RuType { type of rule: read from init file, or user }
  End;

  { query }
  TObjQuery = Record
    PO_META : TObjMeta;
    { deep copied: }
    QU_NEXT : QueryPtr; { next query }
    QU_FRUL : RulePtr; { first rule to try }
    QU_LRUL : RulePtr; { last rule to try }
    QU_FBTR : BTermPtr; { terms in the query }
    QU_SYST : EqPtr; { list of equation or inequation in the query }
    { not deep copied: }
    QU_FVAR : DictPtr; { where to start looking up }
    QU_LVAR : DictPtr; { where to stop looking up }
    { extra data: }
    QU_ACUT : Boolean; { query contains a cut }
  End;

  { program }
  TObjProg = Record
    PO_META : TObjMeta;
    { deep copied: }
    PP_FQRY : QueryPtr; { first query }
    PP_LQRY : QueryPtr; { last query }
    PP_FRUL : RulePtr; { first rule }
    PP_LRUL : RulePtr; { last rule }
    { not deep copied: }
    PP_HEAD : HeadPtr; { current clock head (during execution or a query) }
    PP_DCON : DictPtr; { list of all constants }
    PP_UCON : DictPtr; { constant list head before processing user's command line; unused }
    PP_DIDE : DictPtr; { list of all identifiers (globals, can be assigned, must not backtrack) }
    PP_DVAR : DictPtr; { list of all variable identifiers }
    PP_UVAR : DictPtr; { variable identifier list head before processing user's command line }
    PP_LVAR : DictPtr { last identifier to lookup when parsing (local variables)}
  End;

  { clock header }
  TObjHead = Record
      PO_META : TObjMeta;
      { not deep copied: }
      HH_NEXT : HeadPtr; { previous clock header or Nil }
      HH_RULE : RulePtr; { rule to apply }
      HH_FBCL : BTermPtr; { terms to clear }
      HH_REST : RestorePtr; { restoration stack }
      HH_BACK : HeadPtr; { where to backtrack (cut) }
      { extra data: }
      HH_CLOC : LongInt; { clock time (unlikely to overflow)}
      HH_ISYS : Boolean; { term to clear is a system call? }
      HH_ICUT : Boolean { term to clear is a cut? }
  End;


{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new block }
Function NewBTerm : BTermPtr;
Var 
  B : BTermPtr;
  ptr : TPObjPtr Absolute B;
Begin
  ptr := NewRegisteredObject(BT,4,True,3);
  With B^ Do
  Begin
    BT_TERM := Nil;
    BT_NEXT := Nil;
    BT_ACCE := Nil;
    BT_HEAD := Nil
  End;
  NewBTerm := B
End;

{ new rule }
Function NewRule( RuleType : RuType ) : RulePtr;
Var 
  R : RulePtr;
  ptr : TPObjPtr Absolute R;
Begin
  ptr := NewRegisteredObject(RU,5,True,3);
  With R^ Do
  Begin
    RU_NEXT := Nil;
    RU_FBTR := Nil;
    RU_SYST := Nil;
    RU_FVAR := Nil;
    RU_LVAR := Nil;
    RU_ACUT := False;
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
  ptr := NewRegisteredObject(PR,11,True,4);
  With P^ Do
  Begin
    PP_FRUL := Nil;
    PP_LRUL := Nil;
    PP_FQRY := Nil;
    PP_LQRY := Nil;
    PP_HEAD := Nil;
    PP_DCON := Nil;
    PP_UCON := Nil;
    PP_DIDE := Nil;
    PP_DVAR := Nil;
    PP_UVAR := Nil;
    PP_LVAR := Nil
  End;
  NewProgram := P
End;

{ new clock header }
Function NewClockHeader : HeadPtr;
Var 
  H : HeadPtr;
  ptr : TPObjPtr Absolute H;
Begin
  ptr := NewRegisteredObject(HE,5,True,0);
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
  NewClockHeader := H
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

{ access identifier of a block }
Function AccessTerm( B : BTermPtr ) : IdPtr;
Begin
  AccessTerm := B^.BT_ACCE
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

{ next query }
Function NextQuery( Q : QueryPtr ) : QueryPtr;
Begin
  NextQuery := Q^.QU_NEXT
End;

{ last query in a list }
Function LastQuery( Q : QueryPtr ) : QueryPtr;
Begin
  While (Q^.QU_NEXT <> Nil) Do
    Q := Q^.QU_NEXT;
  LastQuery := Q
End;

{ last query in a program }
Function LastProgramQuery( P : ProgPtr ) : QueryPtr;
Begin
  LastProgramQuery := P^.PP_LQRY
End;

{ append queries Q to program P }
Procedure AppendQueries( P : ProgPtr; Q : QueryPtr );
Var
  LastQ : QueryPtr;
Begin
  If Q <> Nil Then
  Begin
    CheckCondition((P^.PP_FQRY = Nil) And (P^.PP_LQRY = Nil) 
        Or (P^.PP_FQRY <> Nil) And (P^.PP_LQRY <> Nil),'broken list of queries');
    LastQ := P^.PP_LQRY;
    P^.PP_LQRY := LastQuery(Q);
    If LastQ = Nil Then
      P^.PP_FQRY := Q
    Else
      LastQ^.QU_NEXT := Q
  End
End;

{ update the rule scope of query Q with all the rules in P }
Procedure UpdateQueryScope( P : ProgPtr; Q : QueryPtr );
Begin
  Q^.QU_FRUL := P^.PP_FRUL;
  Q^.QU_LRUL := P^.PP_LRUL
End;

{ append a clock header to a list }
Procedure AppendClockHeader( Var list : HeadPtr; H : HeadPtr );
Begin
  H^.HH_NEXT := list;
  list := H;
  If H^.HH_NEXT <> Nil Then
    H^.HH_CLOC := H^.HH_NEXT^.HH_CLOC + 1
End;

{ get the rule data of a clock header }
Procedure GetHeaderRule( H : HeadPtr; Var R : RulePtr; Var isSys : Boolean; Var isCut : Boolean );
Begin
  R := H^.HH_RULE;
  isSys := H^.HH_ISYS;
  isCut := H^.HH_ICUT
End;

{ set the rule data of a clock header }
Procedure SetHeaderRule( H : HeadPtr; R : RulePtr; isSys, isCut : Boolean);
Begin
  With H^ Do
  Begin
    HH_RULE := R;
    HH_ISYS := isSys;
    HH_ICUT := isCut
  End
End;

{ create and set a clock header on top of a list of headers }
Procedure PushNewClockHeader( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );
Var 
  H : HeadPtr;
Begin
  H := NewClockHeader;
  With H^ Do
  Begin
    HH_FBCL := Fbcl
  End;
  SetHeaderRule(H,R,isSys,isCut);
  AppendClockHeader(list,H)
End;

{ make all terms in B point back to header H }
Procedure SetTermsHeader( H : HeadPtr; B : BTermPtr );
Begin
  While B <> Nil Do
  Begin
    B^.BT_HEAD := H;
    B := NextTerm(B)
  End
End;