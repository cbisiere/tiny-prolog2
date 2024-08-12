{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : PObjDef.pas                                                }
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

{ declarations for engine's main objects; they cannot be split across source 
 files as forward references to types must be under the same Type declaration }

Unit PObjDef;

Interface

Uses
  Num,
  ShortStr,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjIO,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjSys;

Type 
  TILevel = Integer; { file insertion level (0 is command line) }
  TSyntax = (
    PrologIIc,  { Prolog II version 1 (with constraints) }
    PrologII,   { Prolog II version 2 }
    PrologIIp,  { Prolog II+ Marseille syntax }
    Edinburgh   { Prolog II+ Edinburgh syntax }
  );

Type 
  BTermPtr = ^TObjBTerm;
  RulePtr = ^TObjRule;
  QueryPtr = ^TObjQuery;
  ProgPtr = ^TObjProg;
  HeadPtr = ^TObjHead;
  StmtPtr = ^TObjStmt;
  CommPtr = ^TObjComm;
  WorldPtr = ^TObjWorld;

  TGoalType = (GOAL_UNDEFINED,GOAL_STD,GOAL_CUT,GOAL_SYS,GOAL_FIND,GOAL_BLOCK);

  { list of pterms w/ extra data (access) }
  TObjBTerm = Record
    PO_META : TObjMeta;
    { deep copied: }
    BT_NEXT : BTermPtr; { next element }
    BT_TERM : TermPtr; { term }
    BT_ACCE : IdPtr; { access identifier or Nil }
    { not deep copied: }
    BT_HEAD : HeadPtr; { clock header point to the rule containing this term }
    { extra data }
    BT_ARIT : TArity; { arity or the access identifier if any, otherwise zero }
    BT_TYPE : TGoalType { type of goal }
  End;

  { rule }
  TObjRule = Record
    PO_META : TObjMeta;
    { deep copied: }
    RU_FBTR : BTermPtr; { list of terms (the first is the rule head) }
    RU_SYST : EqPtr; { list of equation or inequation in the rule; Warning: not GC }
    { not deep copied: }
    RU_STMT : StmtPtr; { statement of this rule }
    { extra data: }
    RU_ACUT : Boolean; { rule queue contains a cut }
    RU_SYNT : TSyntax { syntax the rule is written in }
  End;

  { clock header }
  TBranch = PosInt; { branch under exploration }
  TClock = PosInt; { clock time }
  TObjHead = Record
      PO_META : TObjMeta;
      { not deep copied: }
      HH_NEXT : HeadPtr; { previous clock header or Nil }
      HH_RULE : RulePtr; { rule to apply, if any }
      HH_FBCL : BTermPtr; { all terms to clear }
      HH_REST : RestPtr; { restoration stack }
      HH_BACK : HeadPtr; { where to backtrack (cut) }
      HH_BLOC : HeadPtr; { header of the last opened bloc (scope) }
      HH_CHOI : Pointer; { data transferred between successive calls of a system call }
      HH_CHOV : TermPtr; { term to extract data from the goal (findall/3), or block/2 label }
      HH_ACCE : IdPtr; { goal access }
      { extra data: }
      HH_TYPE : TGoalType; { goal type } 
      HH_ARIT : TArity;  { goal arity }
      HH_CLOC : TClock;  { depth: clock time (unlikely to overflow)}
      HH_BRAN : TBranch; { width: branch number under exploration }
      HH_SUCC : TBranch; { width: success number }
      HH_CLEA : Boolean; { current goal has been cleared }
      HH_MORE : Boolean; { syscall asks to be called again }
      HH_FGOA : Boolean; { term to clear is on behalf of a syscall }
      HH_DONE : Boolean  { no more branches to explore for the current goal }
  End;

  { query }
  TObjQuery = Record
    PO_META : TObjMeta;
    { deep copied: }
    QU_NEXT : QueryPtr; { next query }
    QU_FBTR : BTermPtr; { terms in the query }
    QU_SYST : EqPtr; { list of equation or inequation in the query }
    { not deep copied: }
    QU_DVAR : DictPtr; { local variables }
    QU_HEAD : HeadPtr; { head of local list of clock headers }
    { extra data: }
    QU_LEVL : TILevel; { 0: command-line; 1: first inserted file, etc. }
    QU_SYNT : TSyntax { syntax the query is written in }
  End;

  { comment }
  TObjComm = Record
    PO_META : TObjMeta;
    { deep copied: }
    CM_COMM : ConstPtr { the comment as a constant string term }
  End;

  { world }
  TObjWorld = Record
    WO_META : TObjMeta;
    { deep copied: }
    WO_NAME : StrPtr; { name of the world }
    WO_FSTA : StmtPtr; { first statement in this world }
    WO_LSTA : StmtPtr; { last statement in this world }
    WO_CSTA : StmtPtr; { current statement in this world }
    WO_WPAR : WorldPtr; { parent or Nil }
    WO_WFCH : WorldPtr; { first child or Nil }
    WO_WPRV : WorldPtr; { previous sibling or Nil }
    WO_WNXT : WorldPtr; { next sibling or Nil }
    { extra data: }
    WO_USER : Boolean { is it a user land world? }
  End;

  { statement }
  TStmt = (StatementStart,Comment,Rule,StatementEnd);
  TObjStmt = Record
    PO_META : TObjMeta;
    { deep copied: }
    SM_PREV : StmtPtr; { previous statement or Nil }
    SM_NEXT : StmtPtr; { next statement or Nil }
    SM_OBJC : TObjectPtr; { associated object in any }
    SM_WRLD : WorldPtr; { world this statement belongs to }
    { extra data: }
    SM_TYPE : TStmt { type of statement }
  End;

  { program }
  TObjProg = Record
    PO_META : TObjMeta;
    { deep copied: }
    PP_WTOP : WorldPtr; { top world }
    PP_WCUR : WorldPtr; { current world }
    PP_TOKE : TokenPtr; { tokens (GC shield) }
    PP_FQRY : QueryPtr; { queries (GC shield and debug) }
    { not deep copied: }
    PP_FILE : StreamPtr; { top of the stack of streams or Nil }
    PP_DCON : DictPtr; { list of all constants }
    PP_DIDE : DictPtr; { list of all identifiers (globals, can be assigned, must not backtrack) }
    PP_DVAR : DictPtr; { local variables when parsing a rule or a query }
    PP_OPER : OpPtr;   { list of operators }
    PP_PATH : StrPtr; { path (dir) of the file passed as parameter in the CL }
    { extra data: }
    PP_LEVL : TILevel; { current file insertion level (0 is command-line) }
    PP_SYNT : TSyntax; { current active syntax }
    PP_ECHO : Boolean; { echo is on/off }
    PP_TRAC : Boolean; { trace is on/off }
    PP_DEBG : Boolean { debug is on/off }
  End;

Function Statement_GetWorld( S : StmtPtr ) : WorldPtr;
Function Rule_GetStatement( R : RulePtr ) : StmtPtr;

Function World_IsUserLand( W : WorldPtr ) : Boolean;
Function Statement_IsUserLand( S : StmtPtr ) : Boolean;
Function Rule_IsUserLand( R : RulePtr ) : Boolean;

Implementation

{-----------------------------------------------------------------------}
{ get / set                                                             }
{-----------------------------------------------------------------------}

{ some getters need to be located here to break circular references of units;
 the typical case is when a member implements a has(O1,O2) relation while a
 "back pointer", member of O2, points back to O1 )

{ statement's world }
Function Statement_GetWorld( S : StmtPtr ) : WorldPtr;
Begin
  Statement_GetWorld := S^.SM_WRLD
End;

{ get a rule's statement }
Function Rule_GetStatement( R : RulePtr ) : StmtPtr;
Begin
  Rule_GetStatement := R^.RU_STMT
End;

{ is this world a userland world? }
Function World_IsUserLand( W : WorldPtr ) : Boolean;
Begin
  World_IsUserland := W^.WO_USER
End;

{ statement lives in userland? }
Function Statement_IsUserLand( S : StmtPtr ) : Boolean;
Begin
  Statement_IsUserLand := World_IsUserland(Statement_GetWorld(S))
End;

{ rule lives in userland? }
Function Rule_IsUserLand( R : RulePtr ) : Boolean;
Begin
  Rule_IsUserLand := Statement_IsUserLand(Rule_GetStatement(R))
End;

End.