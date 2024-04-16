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
  Files,
  Trace,
  Memory,
  PObj,
  PObjTok,
  Tokenize,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjIO,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjHead,
  PObjComm,
  PObjWrld,
  PObjStmt;

Function Prog_New : ProgPtr;

Function CurrentInput( P : ProgPtr ) : StreamPtr;
Function CurrentOutput( P : ProgPtr ) : StreamPtr;
Function OutputIsConsole( P : ProgPtr ) : Boolean;
Function GetInputConsole( P : ProgPtr )  : StreamPtr;

Function GetStreamByPath( P : ProgPtr; Path : TPath ) : StreamPtr;
Function GetStreamByMode( P : ProgPtr; Mode : TStreamMode ) : StreamPtr;
Function GetStreamByDescriptor( P : ProgPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Function GetStreamByAlias( P : ProgPtr; Alias : TAlias ) : StreamPtr;
Procedure CloseAndDeleteStream( P : ProgPtr; f : StreamPtr );
Procedure PushStream( P : ProgPtr; f : StreamPtr );
Procedure SetStreamAsCurrent( P : ProgPtr; f : StreamPtr );
Procedure DeleteTopBuffer( P : ProgPtr );
Procedure ResetIO( P : ProgPtr );
Procedure ReadFromConsole( P : ProgPtr );

{ worlds }
Function GetCurrentWorld( P : ProgPtr ) : WorldPtr;
Procedure SetCurrentWorld( P : ProgPtr; W : WorldPtr );
Function CreateNewSubWorld( P : ProgPtr; Name : StrPtr; 
    UserLand : Boolean ) : Boolean;

{ statements: iterate }
Procedure ProgInsertComment( P : ProgPtr; s : StrPtr );
Procedure ProgInsertRule( P : ProgPtr; R : RulePtr );

{ rules: iterate }
Function FirstRule( P : ProgPtr; Local : Boolean ) : RulePtr;
Function NextRule( R : RulePtr; Local : Boolean ) : RulePtr;
Function FirstRuleWithHead( P : ProgPtr; I : IdPtr; Local : Boolean ) : RulePtr;
Function LastRuleWithHead( P : ProgPtr; I : IdPtr; Local : Boolean ) : RulePtr;
Function FindRuleWithHead( R : RulePtr; I : IdPtr; Local : Boolean ) : RulePtr;

Function EmitConst( P : ProgPtr; s : StrPtr; ty : TypePrologObj; 
    glob : Boolean ) : TermPtr;
Function EmitIdent( P : ProgPtr; s : StrPtr; glob : Boolean ) : TermPtr;
Function EmitShortIdent( P : ProgPtr; ident : TString; 
    glob : Boolean ) : TermPtr;


Function GetCurrentQuery( P : ProgPtr )  : QueryPtr;
Function NewProgramQuery( P : ProgPtr ) : QueryPtr;

Function ReadProgramToken( P : ProgPtr; f : StreamPtr ) : TokenPtr; 

Procedure ReleaseMemory( P : ProgPtr );
Procedure BeginInsertion( P : ProgPtr );
Procedure EndInsertion( P : ProgPtr );
Function GetProgramPath( P : ProgPtr ) : TString;
Procedure SetProgramPath( P : ProgPtr; path : TString );
Function GetSyntax( P : ProgPtr ) : TSyntax;
Procedure SetSyntax( P : ProgPtr; y : TSyntax );

Implementation
{-----------------------------------------------------------------------------}

Const
  DEFAULT_PROLOG_SYNTAX : TSyntax = PrologIIc;

{-----------------------------------------------------------------------}
{ helpers                                                               }
{-----------------------------------------------------------------------}

{ create the default streams for a Prolog engine using syntax y }
Function CreateDefaultStreams( y : TSyntax ) : StreamPtr;
Var 
  f,f2,f3 : StreamPtr;
Begin
  f := Nil;
  If y in [PrologII,PrologIIc,PrologIIp] Then
  Begin
    f := NewConsole(MODE_READ); { top read: default input }
    f2 := NewConsole(MODE_WRITE); { top write: default output }
    StreamChain(f,f2);
    If y In [PrologII,PrologIIc] Then
    Begin
      f3 := NewBuffer; { new-buffer is not mandatory to use a buffer (TBC)}
      StreamChain(f2,f3)
    End
  End;
  CreateDefaultStreams := f
End;

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new program }
Function Prog_New : ProgPtr;
Var 
  P : ProgPtr;
  ptr : TObjectPtr Absolute P;
Begin
  ptr := NewRegisteredPObject(PR,SizeOf(TObjProg),9,True,4);
  With P^ Do
  Begin
    PP_WTOP := World_New(Str_NewFromString('Supervisor'),False);
    PP_WCUR := PP_WTOP;
    PP_FILE := CreateDefaultStreams(DEFAULT_PROLOG_SYNTAX);
    PP_TOKE := Nil;
    PP_FQRY := Nil;
    PP_DCON := Nil;
    PP_DIDE := Nil;
    PP_DVAR := Nil;
    PP_OPER := Nil;
    PP_LEVL := 0;
    PP_PATH := '';
    PP_SYNT := DEFAULT_PROLOG_SYNTAX
  End;
  Prog_New := P
End;

{-----------------------------------------------------------------------}
{ streams                                                               }
{-----------------------------------------------------------------------}

{ return the current input stream }
Function CurrentInput( P : ProgPtr ) : StreamPtr;
Begin
  CurrentInput := StreamStackCurrentInput(P^.PP_FILE)
End;

{ return the current output stream }
Function CurrentOutput( P : ProgPtr ) : StreamPtr;
Begin
  CurrentOutput := StreamStackCurrentOutput(P^.PP_FILE)
End;

{ return true if the current output is the terminal }
Function OutputIsConsole( P : ProgPtr ) : Boolean;
Begin
  OutputIsConsole := StreamIsConsole(CurrentOutput(P))
End;

{ return the input console as a stream }
Function GetInputConsole( P : ProgPtr )  : StreamPtr;
Begin
  GetInputConsole := StreamStackInputConsole(P^.PP_FILE)
End;

{ return a stream having path Path, or Nil }
Function GetStreamByPath( P : ProgPtr; Path : TPath ) : StreamPtr;
Begin
  GetStreamByPath := StreamStackLookupByPath(P^.PP_FILE,Path)
End;

{ return a stream having mode Mode, or Nil }
Function GetStreamByMode( P : ProgPtr; Mode : TStreamMode ) : StreamPtr;
Begin
  GetStreamByMode := StreamStackLookupByMode(P^.PP_FILE,Mode)
End;

{ return a stream having file descriptor Desc, or Nil }
Function GetStreamByDescriptor( P : ProgPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Begin
  GetStreamByDescriptor := StreamStackLookupByDescriptor(P^.PP_FILE,Desc)
End;

{ return a stream having alias Alias, or Nil }
Function GetStreamByAlias( P : ProgPtr; Alias : TAlias ) : StreamPtr;
Begin
  GetStreamByAlias := StreamStackLookupByAlias(P^.PP_FILE,Alias)
End;

{ push a stream at the top of the stack }
Procedure PushStream( P : ProgPtr; f : StreamPtr );
Begin
  StreamStackPush(P^.PP_FILE,f)
End;

{ close and remove a stream from the stack }
Procedure CloseAndDeleteStream( P : ProgPtr; f : StreamPtr );
Begin
  CheckCondition(Not StreamIsConsole(f),
      'CloseAndDeleteStream: attempt to delete a console');
  StreamClose(f);
  StreamStackUnchain(P^.PP_FILE,f)
End;

{ set stream f as current (read or write) }
Procedure SetStreamAsCurrent( P : ProgPtr; f : StreamPtr );
Begin
  StreamStackMoveToTop(P^.PP_FILE,f)
End;

{ delete the highest buffer in the stack }
Procedure DeleteTopBuffer( P : ProgPtr );
Var
  f : StreamPtr;
Begin
  f := StreamStackLookupByDevice(P^.PP_FILE,DEV_BUFFER);
  CheckCondition(f <> Nil,'DelBuffer: no buffer');
  CloseAndDeleteStream(P,f)
End;

{ reset the program stream set }
Procedure ResetIO( P : ProgPtr );
Begin
  StreamStackCloseAll(P^.PP_FILE);
  P^.PP_FILE := CreateDefaultStreams(GetSyntax(P))
End;

{ read a line from the keyboard }
Procedure ReadFromConsole( P : ProgPtr );
Begin
  ReadLineFromKeyboard(GetInputConsole(P))
End;


{-----------------------------------------------------------------------}
{ worlds                                                                }
{-----------------------------------------------------------------------}

{ return the current world of program P }
Function GetCurrentWorld( P : ProgPtr ) : WorldPtr;
Begin
  GetCurrentWorld := P^.PP_WCUR
End;

{ set the current world of program P to be W }
Procedure SetCurrentWorld( P : ProgPtr; W : WorldPtr );
Begin
  P^.PP_WCUR := W
End;

{ create a new world with name Name, below the current world of P, and set it
 as the new current world }
Function CreateNewSubWorld( P : ProgPtr; Name : StrPtr; 
    UserLand : Boolean ) : Boolean;
Var
  Wc,W : WorldPtr;
  Ok : Boolean;
Begin
  Wc := GetCurrentWorld(P);
  CheckCondition(Wc <> Nil,'CreateNewWorldBelow: current world is not set');
  Ok := World_FindChildByName(Wc,Name) = Nil;
  If Not Ok Then
  Begin
    CWriteWarning('current world already has a child with name ''');
    Str_CWrite(Name);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  W := World_New(Name,UserLand);
  World_AppendChild(Wc,W);
  SetCurrentWorld(P,W);
  CreateNewSubWorld := Ok
End;


{----------------------------------------------s-------------------------}
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
  EmitShortIdent := EmitIdent(P,Str_NewFromString(ident),glob)
End;

{-----------------------------------------------------------------------}
{ rules: iterate, through worlds and statements                         }
{-----------------------------------------------------------------------}

{ first statement to look at when looking for a rule to apply }
Function FirstStatement( P : ProgPtr ) : StmtPtr;
Begin
  FirstStatement := World_GetFirstStatement(GetCurrentWorld(P))
End;

{ next statement to look at, or Nil when there is no more statements; do not
 climb worlds if Local is True }
Function NextStatement( St : StmtPtr; Local : Boolean ) : StmtPtr;
Var
  W : WorldPtr;
Begin
  W := Statement_GetWorld(St);
  St := Statement_GetNext(St);
  If (St = Nil) And (Not Local) Then
  Begin
    W := World_GetParent(W); { go one world up }
    If W <> Nil Then
      St := World_GetFirstStatement(W)
  End;
  NextStatement := St 
End;

{ first rule in the program starting on the current world, or Nil }
Function FirstRule( P : ProgPtr; Local : Boolean ) : RulePtr;
Var
  St : StmtPtr;
Begin
  FirstRule := Nil;
  St := FirstStatement(P);
  While (St <> Nil) And (Statement_GetType(St) <> Rule) Do
    St := NextStatement(St,Local);
  If St <> Nil Then
    FirstRule := RulePtr(Statement_GetObject(St))
End;

{ next rule after a given rule, or Nil }
Function NextRule( R : RulePtr; Local : Boolean ) : RulePtr;
Var
  St : StmtPtr;
Begin
  NextRule := Nil;
  St := Rule_GetStatement(R);
  St := NextStatement(St,Local);
  While (St <> Nil) And (Statement_GetType(St) <> Rule) Do
    St := NextStatement(St,Local);
  If St <> Nil Then
    NextRule := RulePtr(Statement_GetObject(St))
End;

{ find the first rule whose head is a given identifier, starting on
 a given rule, or Nil; if the starting rule is Nil, return Nil }
Function FindRuleWithHead( R : RulePtr; I : IdPtr; Local : Boolean ) : RulePtr;
Begin
  While (R <> Nil) And (Not Unifiable(TermPtr(Rule_Access(R)),TermPtr(I))) Do
    R := NextRule(R,Local);
  FindRuleWithHead := R
End;

{ first rule whose head is a given identifier, or Nil }
Function FirstRuleWithHead( P : ProgPtr; I : IdPtr; Local : Boolean ) : RulePtr;
Begin
  FirstRuleWithHead := FindRuleWithHead(FirstRule(P,Local),I,Local)
End;

{ last rule whose head is a given identifier, or Nil }
Function LastRuleWithHead( P : ProgPtr; I : IdPtr; Local : Boolean ) : RulePtr;
Var
  R,Rn : RulePtr;
Begin
  LastRuleWithHead := Nil;
  R := FirstRuleWithHead(P,I,Local);
  If R = Nil Then
    Exit;
  Rn := FindRuleWithHead(NextRule(R,Local),I,Local);
  If Rn <> Nil Then
    LastRuleWithHead := Rn
  Else
    LastRuleWithHead := R
End;

{-----------------------------------------------------------------------}
{ statements                                                            }
{-----------------------------------------------------------------------}

{ insert a comment }
Procedure ProgInsertComment( P : ProgPtr; s : StrPtr );
Var
  St : StmtPtr;
  C : ConstPtr;
Begin
  C := InstallConst(P^.PP_DCON,s,CS,True);
  St := Statement_New(Comment,TObjectPtr(Comment_New(C)));
  World_InsertStatement(GetCurrentWorld(P),St)
End;

{ insert one rule; setting the link between rule and statement }
Procedure ProgInsertRule( P : ProgPtr; R : RulePtr );
Var
  St : StmtPtr;
Begin
  St := Statement_New(Rule,TObjectPtr(R));
  Rule_SetStatement(R,St);
  World_InsertStatement(GetCurrentWorld(P),St)
End;

{-----------------------------------------------------------------------}
{ queries                                                               }
{-----------------------------------------------------------------------}

{ get the most recent query }
Function GetCurrentQuery( P : ProgPtr )  : QueryPtr;
Begin
  GetCurrentQuery := P^.PP_FQRY
End;

{ set the most recent added query }
Procedure SetCurrentQuery( P : ProgPtr; Q : QueryPtr );
Begin
  P^.PP_FQRY := Q
End;

{ add a query to the program's list; meant to protect them against GC }
Procedure AppendProgramQuery( P : ProgPtr; Q : QueryPtr );
Begin
  If GetCurrentQuery(P) <> Nil Then
    Query_SetNext(Q,GetCurrentQuery(P));
  SetCurrentQuery(P,Q)
End;

{ create a new query, protected from GC }
Function NewProgramQuery( P : ProgPtr ) : QueryPtr;
Var
  Q : QueryPtr;
Begin
  Q := Query_New(P^.PP_LEVL,GetSyntax(P));
  AppendProgramQuery(P,Q);
  NewProgramQuery := Q
End;

{-----------------------------------------------------------------------}
{ methods                                                               }
{-----------------------------------------------------------------------}

{ add a token to the program's list; meant to protect them against GC }
Procedure AppendProgramToken( P : ProgPtr; K: TokenPtr );
Begin
  If P^.PP_TOKE <> Nil Then
    Token_SetNext(K,P^.PP_TOKE);
  P^.PP_TOKE := K
End;

{ read a token from a stream; protect it from GC }
Function ReadProgramToken( P : ProgPtr; f : StreamPtr ) : TokenPtr; 
Var
  K : TokenPtr;
Begin
  K := ReadToken(f,GetSyntax(P));
  AppendProgramToken(P,K);
  ReadProgramToken := K
End;

{ expose to GC objects that are not useful anymore }
Procedure ReleaseMemory( P : ProgPtr );
Begin
  P^.PP_TOKE := Nil;
  SetCurrentQuery(P,Nil)
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

{ get the Prolog program's path passed as parameter }
Function GetProgramPath( P : ProgPtr ) : TString;
Begin
  GetProgramPath := P^.PP_PATH
End;

{ set the current type of rules to read }
Procedure SetProgramPath( P : ProgPtr; path : TString );
Begin
  P^.PP_PATH := path
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

End.