{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjProg.pas                                               }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
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
  TILevel = Integer; { file insertion level (0 is command line) }
  TSyntax = (
    PrologII,   { Prolog II: second Prolog developed by the GIA }
    PrologIIc,  { Tiny Prolog II: Prolog II with constraints (my version) }
    PrologIIp,  { Prolog II+ }
    Edinburgh   { Edinburgh syntax as defined in Prolog II+ }
  );

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
    RU_FVAR : DictPtr; { where to start looking up for local variables }
    RU_LVAR : DictPtr; { where to stop looking up for local variables }
    { extra data: }
    RU_ACUT : Boolean; { rule queue contains a cut }
    RU_TYPE : RuType; { type of rule: read from init file, or user }
    RU_SYNT : TSyntax { syntax the rule is written in }
  End;

  { query }
  TObjQuery = Record
    PO_META : TObjMeta;
    { deep copied: }
    QU_PREV : QueryPtr; { previous query }
    QU_NEXT : QueryPtr; { next query }
    QU_FRUL : RulePtr; { first rule to try }
    QU_LRUL : RulePtr; { last rule to try }
    QU_FBTR : BTermPtr; { terms in the query }
    QU_SYST : EqPtr; { list of equation or inequation in the query }
    { not deep copied: }
    QU_FVAR : DictPtr; { where to start looking up for local variables }
    QU_LVAR : DictPtr; { where to stop looking up for local variables }
    { extra data: }
    QU_LEVL : TILevel; { 0: command-line; 1: first inserted file, etc. }
    QU_ACUT : Boolean; { query contains a cut }
    QU_SYNT : TSyntax { syntax the query is written in }
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
    PP_HEAD : HeadPtr; { current clock head (during execution of a query) }
    PP_DCON : DictPtr; { list of all constants }
    PP_UCON : DictPtr; { constant list head before processing user's command line; unused }
    PP_DIDE : DictPtr; { list of all identifiers (globals, can be assigned, must not backtrack) }
    PP_DVAR : DictPtr; { list of all variable identifiers }
    PP_UVAR : DictPtr; { variable identifier list head before processing user's command line }
    PP_LVAR : DictPtr; { last identifier to lookup when parsing (local variables)}
    PP_OPER : OpPtr;   { list of operators }
    { extra data: }
    PP_LEVL : TILevel; { current file insertion level (0 is command-line) }
    PP_PATH : TString; { path (dir) of the file passed as parameter in the CL }
    PP_TYPE : RuType;  { type of rule the program is about to read }
    PP_SYNT : TSyntax  { current active syntax }
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

{ new query w/ given file insertion level }
Function NewQuery( level : TILevel; y : TSyntax ) : QueryPtr;
Var 
  Q : QueryPtr;
  ptr : TPObjPtr Absolute Q;
Begin
  ptr := NewRegisteredObject(QU,8,True,6);
  With Q^ Do
  Begin
    QU_PREV := Nil;
    QU_NEXT := Nil;
    QU_FRUL := Nil;
    QU_LRUL := Nil;
    QU_FBTR := Nil;
    QU_SYST := Nil;
    QU_FVAR := Nil;
    QU_LVAR := Nil;
    QU_LEVL := level;
    QU_ACUT := False;
    QU_SYNT := y
  End;
  NewQuery := Q
End;

{ new rule }
Function NewRule( RuleType : RuType; y : TSyntax ) : RulePtr;
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
    RU_TYPE := RuleType;
    RU_SYNT := y
  End;
  NewRule := R
End;

{ new program }
Function NewProgram : ProgPtr;
Var 
  P : ProgPtr;
  ptr : TPObjPtr Absolute P;
Begin
  ptr := NewRegisteredObject(PR,12,True,4);
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
    PP_LVAR := Nil;
    PP_OPER := Nil;
    PP_LEVL := 0;
    PP_PATH := '';
    PP_TYPE := RTYPE_USER;
    PP_SYNT := PrologIIc
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
{ methods: misc. convenient emit functions                              }
{-----------------------------------------------------------------------}

{ return a new constant as a term }
Function EmitConst( P : ProgPtr; s : StrPtr; ty : TypePrologObj; 
    glob : Boolean ) : TermPtr;
Var
  C : ConstPtr;
  TC : TermPtr Absolute C;
Begin
  EmitConst := Nil;
  C := InstallConst(P^.PP_DCON,s,ty,glob);
  EmitConst := TC
End;

{ return a new identifier as a term, from a Pascal string }
Function EmitIdent( P : ProgPtr; ident : TString; glob : Boolean ) : TermPtr;
Var
  I : IdPtr;
  TI : TermPtr Absolute I;
Begin
  EmitIdent := Nil;
  I := InstallIdentifier(P^.PP_DIDE,NewStringFrom(ident),glob);
  EmitIdent := TI
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ next block }
Function NextTerm( B : BTermPtr ) : BTermPtr;
Begin
  CheckCondition(B <> Nil,'Cannot compute the next term of Nil');
  NextTerm := B^.BT_NEXT
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

{ append a non-empty list of rules R to program P }
Procedure AppendRules( P : ProgPtr; R : RulePtr );
Begin
  if P^.PP_FRUL = Nil Then
    P^.PP_FRUL := R
  Else
    P^.PP_LRUL^.RU_NEXT := R;
  P^.PP_LRUL := LastRule(R)
End;

{ insert a list of rules R after rule Ra of program P, or, if Ra is Nil, just
 append the list R to program P; return the last element of R (for chaining) }
Function InsertRulesAfter( P : ProgPtr; Ra, R : RulePtr ) : RulePtr;
Var
  Rn,tmp : RulePtr;
Begin
  CheckCondition(R <> Nil,'InsertRulesAfter: R is Nil');
  CheckCondition((P^.PP_FRUL = Nil) And (P^.PP_LRUL = Nil) 
      Or (P^.PP_FRUL <> Nil) And (P^.PP_LRUL <> Nil), 
      'broken list of rules');
  Rn := LastRule(R);
  If Ra = Nil Then
    AppendRules(P,R)
  Else If NextRule(Ra) = Nil Then
    AppendRules(P,R)
  Else
  Begin { insertion "inside" P's rules, so no change to PP_FRUL and PP_LRUL }
    tmp := NextRule(Ra);
    Ra^.RU_NEXT := R;
    Rn^.RU_NEXT := tmp
  End;
  InsertRulesAfter := Rn
End;

{ next query }
Function NextQuery( Q : QueryPtr ) : QueryPtr;
Begin
  NextQuery := Q^.QU_NEXT
End;

{ previous query }
Function PrevQuery( Q : QueryPtr ) : QueryPtr;
Begin
  PrevQuery := Q^.QU_PREV
End;

{ last query in a list }
Function LastQuery( Q : QueryPtr ) : QueryPtr;
Begin
  While (NextQuery(Q) <> Nil) Do
    Q := NextQuery(Q);
  LastQuery := Q
End;

{ first rule in the scope of a query }
Function FirstRuleInQueryScope( Q : QueryPtr ) : RulePtr;
Begin
  FirstRuleInQueryScope := Q^.QU_FRUL
End;

{ set the first rule in the scope of a query }
Procedure SetFirstRuleInQueryScope( Q : QueryPtr; R : RulePtr );
Begin
  Q^.QU_FRUL := R
End;

{ last rule in the scope of a query }
Function LastRuleInQueryScope( Q : QueryPtr ) : RulePtr;
Begin
  LastRuleInQueryScope := Q^.QU_LRUL
End;

{ set the last rule in the scope of a query }
Procedure SetLastRuleInQueryScope( Q : QueryPtr; R : RulePtr );
Begin
  Q^.QU_LRUL := R
End;

{ first query in a program }
Function FirstProgramQuery( P : ProgPtr ) : QueryPtr;
Begin
  FirstProgramQuery := P^.PP_FQRY
End;

{ last query in a program }
Function LastProgramQuery( P : ProgPtr ) : QueryPtr;
Begin
  LastProgramQuery := P^.PP_LQRY
End;

{ first rule in a program }
Function FirstProgramRule( P : ProgPtr ) : RulePtr;
Begin
  FirstProgramRule := P^.PP_FRUL
End;

{ last rule in a program }
Function LastProgramRule( P : ProgPtr ) : RulePtr;
Begin
  LastProgramRule := P^.PP_LRUL
End;

{ append a non-empty list of query Q to program P }
Procedure AppendQueries( P : ProgPtr; Q : QueryPtr );
Begin
  CheckCondition(Q <> Nil,'AppendQueries: Q is Nil');
  CheckCondition((P^.PP_FQRY = Nil) And (P^.PP_LQRY = Nil) 
      Or (P^.PP_FQRY <> Nil) And (P^.PP_LQRY <> Nil),
      'broken list of queries');
  If P^.PP_FQRY = Nil Then
    P^.PP_FQRY := Q
  Else
  Begin
    P^.PP_LQRY^.QU_NEXT := Q;
    Q^.QU_PREV := P^.PP_LQRY
  End;
  P^.PP_LQRY := LastQuery(Q)
End;

{ first query to execute, that is, first query having the same 
  insertion level as the current one }
Function FirstQueryToExecute( P : ProgPtr ) : QueryPtr;
Var 
  Q : QueryPtr;
  Found : Boolean;
Begin
  Q := P^.PP_FQRY;
  Found := False;
  While (Q <> Nil) And (Not Found) Do
  Begin
    Found := Q^.QU_LEVL = P^.PP_LEVL;
    If Not Found Then
      Q := NextQuery(Q)
  End;
  FirstQueryToExecute := Q
End;

{ remove all queries from program P at current insertion level; 
  FIXME: we cannot discard constants 
  and identifiers, since they have a global scope (plus, identifiers can be
  assigned); FIXME: as more queries are submitted, more memory is consumed }
Procedure RemoveQueries( P : ProgPtr );
Var 
  Q : QueryPtr;
Begin
  { detach the queries }
  Q := FirstQueryToExecute(P);
  If Q <> Nil Then
  Begin
    If Q^.QU_PREV = Nil Then { was first in list }
    Begin
      P^.PP_FQRY := Nil;
      P^.PP_LQRY := Nil
    End
    Else
    Begin
      Q^.QU_PREV^.QU_NEXT := Nil; { shorten the list }
      P^.PP_LQRY := Q^.QU_PREV;
      Q^.QU_PREV := Nil { detach }
    End;
  End;
  { TODO: discard non persistent identifiers and constants }
  { forget variables }
  P^.PP_DVAR := P^.PP_UVAR 
End;

{ a file is about to be inserted }
Procedure BeginInsertion( P : ProgPtr );
Begin
  P^.PP_LEVL := P^.PP_LEVL + 1
End;

{ we are done with the current insertion }
Procedure EndInsertion( P : ProgPtr );
Begin
  CheckCondition(P^.PP_LEVL > 0,'negative insertion level');
  P^.PP_LEVL := P^.PP_LEVL - 1
End;

{ get the current type of rules to read }
Function GetProgramPath( P : ProgPtr ) : TString;
Begin
  GetProgramPath := P^.PP_PATH
End;

{ set the current type of rules to read }
Procedure SetProgramPath( P : ProgPtr; path : TString );
Begin
  P^.PP_PATH := path
End;

{ get the current type of rules to read }
Function GetRuleType( P : ProgPtr ) : RuType;
Begin
  GetRuleType := P^.PP_TYPE
End;

{ set the current type of rules to read }
Procedure SetRuleType( P : ProgPtr; t : RuType );
Begin
  P^.PP_TYPE := t
End;

{ get the current syntax }
Function GetSyntax( P : ProgPtr ) : TSyntax;
Begin
  GetSyntax := P^.PP_SYNT
End;

{ set the current syntax }
Procedure SetSyntax( P : ProgPtr; y : TSyntax );
Begin
  P^.PP_SYNT := y
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

{ get a rule's syntax }
Function GetRuleSyntax( R : RulePtr ) : TSyntax;
Begin
  GetRuleSyntax := R^.RU_SYNT
End;

{ get a query's syntax }
Function GetQuerySyntax( Q : QueryPtr ) : TSyntax;
Begin
  GetQuerySyntax := Q^.QU_SYNT
End;
