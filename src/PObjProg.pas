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

Unit PObjProg;

Interface

Uses
  ShortStr,
  Errs,
  Memory,
  PObj,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjTerm;

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
    RU_PREV : RulePtr; { previous rule }
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


Function NewBTerm( T : TermPtr ) : BTermPtr;
Function NewQuery( level : TILevel; y : TSyntax ) : QueryPtr;
Function NewRule( RuleType : RuType; y : TSyntax ) : RulePtr;
Function NewProgram : ProgPtr;

Function EmitConst( P : ProgPtr; s : StrPtr; ty : TypePrologObj; 
    glob : Boolean ) : TermPtr;
Function EmitIdent( P : ProgPtr; s : StrPtr; glob : Boolean ) : TermPtr;
Function EmitShortIdent( P : ProgPtr; ident : TString; 
    glob : Boolean ) : TermPtr;

Function NextTerm( B : BTermPtr ) : BTermPtr;
Function AccessTerm( B : BTermPtr ) : IdPtr;
Function PrevRule( R : RulePtr ) : RulePtr;
Function NextRule( R : RulePtr ) : RulePtr;
Procedure ChainRules( R1,R2 : RulePtr );
Function FirstRuleWithHead( P : ProgPtr; I : IdPtr ) : RulePtr;
Function LastRuleWithHead( P : ProgPtr; I : IdPtr ) : RulePtr;
Procedure PrependRules( P : ProgPtr; R : RulePtr );
Procedure AppendRules( P : ProgPtr; R : RulePtr );
Function InsertRulesB( P : ProgPtr; Rb, R : RulePtr ) : RulePtr;
Function InsertRulesA( P : ProgPtr; Ra, R : RulePtr ) : RulePtr;
Function NextQuery( Q : QueryPtr ) : QueryPtr;
Function FirstRuleInQueryScope( Q : QueryPtr ) : RulePtr;
Procedure SetFirstRuleInQueryScope( Q : QueryPtr; R : RulePtr );
Function LastRuleInQueryScope( Q : QueryPtr ) : RulePtr;
Procedure SetLastRuleInQueryScope( Q : QueryPtr; R : RulePtr );
Function FirstProgramQuery( P : ProgPtr ) : QueryPtr;
Function LastProgramQuery( P : ProgPtr ) : QueryPtr;
Function FirstProgramRule( P : ProgPtr ) : RulePtr;
Function LastProgramRule( P : ProgPtr ) : RulePtr;
Procedure AppendQueries( P : ProgPtr; Q : QueryPtr );
Function FirstQueryToExecute( P : ProgPtr ) : QueryPtr;
Procedure RemoveQueries( P : ProgPtr );
Procedure BeginInsertion( P : ProgPtr );
Procedure EndInsertion( P : ProgPtr );
Function GetProgramPath( P : ProgPtr ) : TString;
Procedure SetProgramPath( P : ProgPtr; path : TString );
Function GetRuleType( P : ProgPtr ) : RuType;
Procedure SetRuleType( P : ProgPtr; t : RuType );
Function GetSyntax( P : ProgPtr ) : TSyntax;
Procedure SetSyntax( P : ProgPtr; y : TSyntax );
Procedure GetHeaderRule( H : HeadPtr; Var R : RulePtr; Var isSys : Boolean; 
    Var isCut : Boolean );

Procedure SetHeaderRule( H : HeadPtr; R : RulePtr; isSys, isCut : Boolean);
Procedure PushNewClockHeader( Var list : HeadPtr; Fbcl : BTermPtr; R : RulePtr; 
    isSys, isCut : Boolean );
Procedure SetTermsHeader( H : HeadPtr; B : BTermPtr );

Function GetRuleSyntax( R : RulePtr ) : TSyntax;
Function GetQuerySyntax( Q : QueryPtr ) : TSyntax;


Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new block for term T }
Function NewBTerm( T : TermPtr ) : BTermPtr;
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
  NewBTerm := B
End;

{ new query w/ given file insertion level }
Function NewQuery( level : TILevel; y : TSyntax ) : QueryPtr;
Var 
  Q : QueryPtr;
  ptr : TObjectPtr Absolute Q;
Begin
  ptr := NewRegisteredPObject(QU,SizeOf(TObjQuery),8,True,6);
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
  ptr : TObjectPtr Absolute R;
Begin
  ptr := NewRegisteredPObject(RU,SizeOf(TObjRule),6,True,4);
  With R^ Do
  Begin
    RU_PREV := Nil;
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
  ptr : TObjectPtr Absolute P;
Begin
  ptr := NewRegisteredPObject(PR,SizeOf(TObjProg),12,True,4);
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

{ return a new identifier as a term, from a string }
Function EmitIdent( P : ProgPtr; s : StrPtr; glob : Boolean ) : TermPtr;
Var
  I : IdPtr;
Begin
  EmitIdent := Nil;
  I := InstallIdentifier(P^.PP_DIDE,s,glob);
  EmitIdent := TermPtr(I)
End;

{ return a new identifier as a term, from a Pascal string }
Function EmitShortIdent( P : ProgPtr; ident : TString; 
    glob : Boolean ) : TermPtr;
Begin
  EmitShortIdent := EmitIdent(P,NewStringFrom(ident),glob)
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

{ previous rule }
Function PrevRule( R : RulePtr ) : RulePtr;
Begin
  PrevRule := R^.RU_PREV
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

{ first rule whose head is a given identifier, or Nil }
Function FirstRuleWithHead( P : ProgPtr; I : IdPtr ) : RulePtr;
Var
  R : RulePtr;
  Ir : IdPtr;
  Found : Boolean;
Begin
  Found := False;
  R := FirstProgramRule(P);
  While (R <> Nil) And Not Found Do
  Begin
    Ir := AccessTerm(R^.RU_FBTR);
    Found := SameTerms(TermPtr(Ir),TermPtr(I));
    If Not Found Then
      R := NextRule(R)
  End;
  If Not Found Then
    R := Nil;
  FirstRuleWithHead := R
End;

{ last rule whose head is a given identifier, or Nil }
Function LastRuleWithHead( P : ProgPtr; I : IdPtr ) : RulePtr;
Var
  R : RulePtr;
  Ir : IdPtr;
  Found : Boolean;
Begin
  Found := False;
  R := LastProgramRule(P);
  While (R <> Nil) And Not Found Do
  Begin
    Ir := AccessTerm(R^.RU_FBTR);
    Found := SameTerms(TermPtr(Ir),TermPtr(I));
    If Not Found Then
      R := PrevRule(R)
  End;
  If Not Found Then
    R := Nil;
  LastRuleWithHead := R
End;

{ link two rules: R1 --> R2 }
Procedure ChainRules( R1,R2 : RulePtr );
Begin
  R1^.RU_NEXT := R2;
  R2^.RU_PREV := R1
End;

{ insert a list of rules R before rule Rb of program P; does not update P's
 list pointers }
Procedure InsertRulesBefore( Rb, R : RulePtr );
Var
  Rp,Rl : RulePtr;
Begin
  CheckCondition(Rb <> Nil,'InsertRulesBefore: Rb is Nil');
  CheckCondition(R <> Nil,'InsertRulesBefore: R is Nil');

  { goal: prev(Rb) --> first(R) --> ... --> last(R) --> Rb }
  Rp := PrevRule(Rb);
  Rl := LastRule(R);

  { last(R) <--> Rb }
  ChainRules(Rl,Rb);

  { prev(Rb) <--> first(R) }
  If Rp <> Nil Then
    ChainRules(Rp,R)
End;

{ insert a list of rules R after rule Ra of program P; does not update P's
 list pointers }
Procedure InsertRulesAfter( Ra, R : RulePtr );
Var
  Rn,Rl : RulePtr;
Begin
  CheckCondition(Ra <> Nil,'InsertRulesAfter: Ra is Nil');
  CheckCondition(R <> Nil,'InsertRulesAfter: R is Nil');

  { goal: Ra --> first(R) --> ... --> last(R) --> next(Ra) }
  Rn := NextRule(Ra);
  Rl := LastRule(R);

  { Ra <--> first(R) }
  ChainRules(Ra,R);

  { last(R) <--> next(Ra) }
  If Rn <> Nil Then
    ChainRules(Rl,Rn)
End;

{ prepend a non-empty list of rules R to program P }
Procedure PrependRules( P : ProgPtr; R : RulePtr );
Var
  Rl : RulePtr;
Begin
  Rl := LastRule(R);
  R^.RU_PREV := Nil;

  if P^.PP_FRUL = Nil Then
    P^.PP_LRUL := Rl { last(P) := last(R) }
  Else
    ChainRules(Rl,P^.PP_FRUL); { last(R) <--> first(P) }
  P^.PP_FRUL := R { first(P) := first(R) }
End;

{ append a non-empty list of rules R to program P }
Procedure AppendRules( P : ProgPtr; R : RulePtr );
Var
  Rl : RulePtr;
Begin
  Rl := LastRule(R);
  R^.RU_PREV := Nil;

  if P^.PP_FRUL = Nil Then
    P^.PP_FRUL := R
  Else
    ChainRules(P^.PP_LRUL,R);
  P^.PP_LRUL := Rl
End;

{ insert a list of rules R before rule Rb of program P, or, if Rb is Nil, just
 prepend the list R to program P; return the first element of R (for chaining) }
Function InsertRulesB( P : ProgPtr; Rb, R : RulePtr ) : RulePtr;
Begin
  CheckCondition(R <> Nil,'InsertRulesB: R is Nil');
  CheckCondition((P^.PP_FRUL = Nil) And (P^.PP_LRUL = Nil) 
      Or (P^.PP_FRUL <> Nil) And (P^.PP_LRUL <> Nil), 
      'broken list of rules');
  If Rb = Nil Then
    PrependRules(P,R)
  Else If PrevRule(Rb) = Nil Then { Ra is the first rule of P }
    PrependRules(P,R)
  Else
    InsertRulesBefore(Rb,R); { insertion "inside" P's rules, so no change to P }
  InsertRulesB := R
End;

{ insert a list of rules R after rule Ra of program P, or, if Ra is Nil, just
 append the list R to program P; return the last element of R (for chaining) }
Function InsertRulesA( P : ProgPtr; Ra, R : RulePtr ) : RulePtr;
Var
  Rn : RulePtr;
Begin
  CheckCondition(R <> Nil,'InsertRulesA: R is Nil');
  CheckCondition((P^.PP_FRUL = Nil) And (P^.PP_LRUL = Nil) 
      Or (P^.PP_FRUL <> Nil) And (P^.PP_LRUL <> Nil), 
      'broken list of rules');
  Rn := LastRule(R);
  If Ra = Nil Then
    AppendRules(P,R)
  Else If NextRule(Ra) = Nil Then { Ra is the last rule of P }
    AppendRules(P,R)
  Else
    InsertRulesAfter(Ra,R); { insertion "inside" P's rules, so no change to P }
  InsertRulesA := Rn
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
Procedure GetHeaderRule( H : HeadPtr; Var R : RulePtr; Var isSys : Boolean; 
    Var isCut : Boolean );
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

End.