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
  Num,
  Errs,
  Files,
  Trace,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjTok,
  Tokenize,
  PObjRest,
  PObjOp,
  PObjStr,
  PObjIO,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjHead,
  PObjComm,
  PObjWrld,
  PObjStmt;

{ default worlds }
Type
  TWorldSetup = Array[TSyntax] Of Record
    Base: String[10];
    Supervisor: String[10];
    User: String[10]
  End;
Const
  WorldSetup : TWorldSetup = (
    (Base:'origine';Supervisor:'?????';User:'ordinaire'),
    (Base:'Base';Supervisor:'Supervisor';User:'Normal'),
    (Base:'Base';Supervisor:'Supervisor';User:'Normal'),
    (Base:'Base';Supervisor:'Supervisor';User:'Normal')
  );

Function Prog_New( y : TSyntax ) : ProgPtr;

Procedure SetEcho( P : ProgPtr; state : Boolean );
Function GetEcho( P : ProgPtr ) : Boolean;
Procedure SetTrace( P : ProgPtr; state : Boolean );
Function GetTrace( P : ProgPtr ) : Boolean;
Procedure SetDebug( P : ProgPtr; state : Boolean );
Function GetDebug( P : ProgPtr ) : Boolean;
Function GetTraceStream( P : ProgPtr ) : StreamPtr;
Function GetDebugStream( P : ProgPtr ) : StreamPtr;

Function BufferAlias( y : TSyntax ) : TAlias;
Function ConsoleAlias( y : TSyntax ) : TAlias;

Function CurrentInput( P : ProgPtr ) : StreamPtr;
Function CurrentOutput( P : ProgPtr ) : StreamPtr;
Function OutputIsConsole( P : ProgPtr ) : Boolean;
Function GetInputConsole( P : ProgPtr ) : StreamPtr;
Function GetOutputConsole( P : ProgPtr ) : StreamPtr;

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
Function FindRuleWithHeadAndArity( R : RulePtr; I : IdPtr; a : PosInt; 
    Local : Boolean ) : RulePtr;

Function EmitConst( P : ProgPtr; s : StrPtr; ty : TypePrologObj; 
    glob : Boolean ) : TermPtr;
Function EmitVariable( P : ProgPtr; s : StrPtr; anonymous : Boolean; 
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
Function GetProgramPath( P : ProgPtr ) : StrPtr;
Procedure SetProgramPath( P : ProgPtr; path : StrPtr );
Function GetSyntax( P : ProgPtr ) : TSyntax;
Procedure SetSyntax( P : ProgPtr; y : TSyntax );

Implementation
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{ helpers                                                               }
{-----------------------------------------------------------------------}

{ return the alias for a buffer }
Function BufferAlias( y : TSyntax ) : TAlias;
Var
  ShortAlias : TString;
Begin
  If y = PrologIIc Then
    ShortAlias := 'tampon'
  Else
    ShortAlias := 'buffer';
  BufferAlias := Str_NewFromShortString(ShortAlias)
End;

{ return the alias for a console }
Function ConsoleAlias( y : TSyntax ) : TAlias;
Begin
  ConsoleAlias := Str_NewFromShortString('console')
End;


{ create the default streams for a Prolog engine using syntax y }
Function CreateDefaultStreams( y : TSyntax ) : StreamPtr;
Var 
  f,f2,f3 : StreamPtr;
Begin
  f := Nil;
  { top read: default input }
  f := Stream_NewConsole(ConsoleAlias(y),MODE_READ);
  { top write: default output }
  f2 := Stream_NewConsole(ConsoleAlias(y),MODE_WRITE); 
  Streams_Chain(f,f2);
  If y In [PrologII,PrologIIc] Then
  Begin
    { new-buffer not mandatory (TBC)}
    f3 := Stream_NewBuffer(BufferAlias(y)); 
    Streams_Chain(f2,f3)
  End;
  CreateDefaultStreams := f
End;

{-----------------------------------------------------------------------}
{ constructors                                                          }
{-----------------------------------------------------------------------}

{ new program }
Function Prog_New( y : TSyntax ) : ProgPtr;
Var 
  P : ProgPtr;
  ptr : TObjectPtr Absolute P;
Begin
  ptr := NewRegisteredPObject(PR,SizeOf(TObjProg),10,True,4);
  With P^ Do
  Begin
    PP_WTOP := World_New(Str_NewFromShortString(WorldSetup[y].Base),False);
    PP_WCUR := PP_WTOP;
    PP_FILE := CreateDefaultStreams(y);
    PP_TOKE := Nil;
    PP_FQRY := Nil;
    PP_DCON := Nil;
    PP_DIDE := Nil;
    PP_DVAR := Nil; { TODO: get rid of it }
    PP_OPER := Nil;
    PP_LEVL := 0;
    PP_PATH := Nil;
    PP_SYNT := y;
    PP_ECHO := Stream_GetEcho;
    PP_TRAC := False;
    PP_DEBG := False
  End;
  Prog_New := P
End;

{-----------------------------------------------------------------------}
{ debug                                                                 }
{-----------------------------------------------------------------------}

Procedure SetEcho( P : ProgPtr; state : Boolean );
Begin
  Stream_SetEcho(state);
  P^.PP_ECHO := Stream_GetEcho
End;

Function GetEcho( P : ProgPtr ) : Boolean;
Begin
  GetEcho := Stream_GetEcho
End;

Procedure SetTrace( P : ProgPtr; state : Boolean );
Begin
  P^.PP_TRAC := state
End;

Function GetTrace( P : ProgPtr ) : Boolean;
Begin
  GetTrace := P^.PP_TRAC
End;

Procedure SetDebug( P : ProgPtr; state : Boolean );
Begin
  P^.PP_DEBG := state
End;

Function GetDebug( P : ProgPtr ) : Boolean;
Begin
  GetDebug := P^.PP_DEBG
End;

{ return the trace stream }
Function GetTraceStream( P : ProgPtr ) : StreamPtr;
Begin
  GetTraceStream := GetOutputConsole(P)
End;

{ return the debug stream or Nil if debug is off }
Function GetDebugStream( P : ProgPtr ) : StreamPtr;
Begin
  If GetDebug(P) Then
    GetDebugStream := GetTraceStream(P)
  Else
    GetDebugStream := Nil
End;

{-----------------------------------------------------------------------}
{ streams                                                               }
{-----------------------------------------------------------------------}

{ return the current input stream }
Function CurrentInput( P : ProgPtr ) : StreamPtr;
Begin
  CurrentInput := Streams_CurrentInput(P^.PP_FILE)
End;

{ return the current output stream }
Function CurrentOutput( P : ProgPtr ) : StreamPtr;
Begin
  CurrentOutput := Streams_CurrentOutput(P^.PP_FILE)
End;

{ return true if the current output is the terminal }
Function OutputIsConsole( P : ProgPtr ) : Boolean;
Begin
  OutputIsConsole := Stream_IsConsole(CurrentOutput(P))
End;

{ return the input console as a stream }
Function GetInputConsole( P : ProgPtr ) : StreamPtr;
Begin
  GetInputConsole := Streams_InputConsole(P^.PP_FILE)
End;

{ return the input console as a stream }
Function GetOutputConsole( P : ProgPtr ) : StreamPtr;
Begin
  GetOutputConsole := Streams_OutputConsole(P^.PP_FILE)
End;

{ return a stream having path Path, or Nil }
Function GetStreamByPath( P : ProgPtr; Path : TPath ) : StreamPtr;
Begin
  GetStreamByPath := Streams_LookupByPath(P^.PP_FILE,Path)
End;

{ return a stream having mode Mode, or Nil }
Function GetStreamByMode( P : ProgPtr; Mode : TStreamMode ) : StreamPtr;
Begin
  GetStreamByMode := Streams_LookupByMode(P^.PP_FILE,Mode)
End;

{ return a stream having file descriptor Desc, or Nil }
Function GetStreamByDescriptor( P : ProgPtr; 
    Desc : TFileDescriptor ) : StreamPtr;
Begin
  GetStreamByDescriptor := Streams_LookupByDescriptor(P^.PP_FILE,Desc)
End;

{ return a stream having alias Alias, or Nil }
Function GetStreamByAlias( P : ProgPtr; Alias : TAlias ) : StreamPtr;
Begin
  GetStreamByAlias := Streams_LookupByAlias(P^.PP_FILE,Alias)
End;

{ push stream f (not yet in the stack) at the top of the stack }
Procedure PushStream( P : ProgPtr; f : StreamPtr );
Begin
  Streams_Push(P^.PP_FILE,f)
End;

{ close and remove a stream from the stack }
Procedure CloseAndDeleteStream( P : ProgPtr; f : StreamPtr );
Begin
  CheckCondition(Not Stream_IsConsole(f),
      'CloseAndDeleteStream: attempt to delete a console');
  Stream_Close(f);
  Streams_Unchain(P^.PP_FILE,f)
End;

{ set stream f (already in the stack) as current (read or write) }
Procedure SetStreamAsCurrent( P : ProgPtr; f : StreamPtr );
Begin
  Streams_MoveToTop(P^.PP_FILE,f)
End;

{ delete the highest buffer in the stack }
Procedure DeleteTopBuffer( P : ProgPtr );
Var
  f : StreamPtr;
Begin
  f := Streams_LookupByDevice(P^.PP_FILE,DEV_BUFFER);
  CheckCondition(f <> Nil,'DelBuffer: no buffer');
  CloseAndDeleteStream(P,f)
End;

{ reset the program stream set }
Procedure ResetIO( P : ProgPtr );
Begin
  Streams_CloseAll(P^.PP_FILE);
  P^.PP_FILE := CreateDefaultStreams(GetSyntax(P))
End;

{ read a line from the keyboard }
Procedure ReadFromConsole( P : ProgPtr );
Begin
  Stream_ReadLineFromKeyboard(GetInputConsole(P))
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

{ return a new variable as a term, from a string }
Function EmitVariable( P : ProgPtr; s : StrPtr; anonymous : Boolean; 
    glob : Boolean ) : TermPtr;
Var
  V : VarPtr;
Begin
  EmitVariable := Nil;
  V := InstallVariable(P^.PP_DVAR,s,anonymous,glob);
  EmitVariable := TermPtr(V)
End;

{ return a new identifier as a term, from a string }
Function EmitIdent( P : ProgPtr; s : StrPtr; glob : Boolean ) : TermPtr;
Var
  Quoted : Boolean;
  I : IdPtr;
Begin
  EmitIdent := Nil;
  Quoted := Not IsUnquotedIdentifier(s,GetSyntax(P));
  I := InstallIdentifier(P^.PP_DIDE,s,Quoted,glob);
  EmitIdent := TermPtr(I)
End;

{ return a new identifier as a term, from a Pascal string }
Function EmitShortIdent( P : ProgPtr; ident : TString; 
    glob : Boolean ) : TermPtr;
Begin
  EmitShortIdent := EmitIdent(P,Str_NewFromShortString(ident),glob)
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

{ does the head of R matches identifier I? }
Function RuleHeadMatches( R : RulePtr; I : IdPtr ) : Boolean;
Begin
  RuleHeadMatches := Term_UnifiableWith(TermPtr(Rule_Access(R)),TermPtr(I))
End;

{ ditto, with arity }
Function RuleHeadMatchesWithArity( R : RulePtr; I : IdPtr; 
    a : PosInt ) : Boolean;
Begin
  RuleHeadMatchesWithArity := RuleHeadMatches(R,I) And (Rule_Arity(R) = a)
End;

{ find the first rule whose head is a given identifier, starting on
 a given rule R, or Nil; if the starting rule R is Nil, return Nil }
Function FindRuleWithHead( R : RulePtr; I : IdPtr; Local : Boolean ) : RulePtr;
Begin
  While (R <> Nil) And (Not RuleHeadMatches(R,I)) Do
    R := NextRule(R,Local);
  FindRuleWithHead := R
End;

{ ditto but with a given arity }
Function FindRuleWithHeadAndArity( R : RulePtr; I : IdPtr; a : PosInt; 
    Local : Boolean ) : RulePtr;
Begin
  While (R <> Nil) And (Not RuleHeadMatchesWithArity(R,I,a)) Do
    R := NextRule(R,Local);
  FindRuleWithHeadAndArity := R
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
  R := FirstRuleWithHead(P,I,Local);
  Rn := R;
  While Rn <> Nil Do
  Begin
    Rn := FindRuleWithHead(NextRule(R,Local),I,Local);
    If Rn <> Nil Then
      R := Rn
  End;
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
    Queries_SetNext(Q,GetCurrentQuery(P));
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

{ attach a token to the program; meant to protect it against GC }
Procedure SetProgramCurrentToken( P : ProgPtr; K: TokenPtr );
Begin
  P^.PP_TOKE := K
End;

{ read a token from a stream; protect it from GC }
Function ReadProgramToken( P : ProgPtr; f : StreamPtr ) : TokenPtr; 
Var
  K : TokenPtr;
Begin
  ReadProgramToken := Nil;
  K := ReadToken(f,GetSyntax(P));
  If Error Then Exit;
  SetProgramCurrentToken(P,K);
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
Function GetProgramPath( P : ProgPtr ) : StrPtr;
Begin
  GetProgramPath := P^.PP_PATH
End;

{ set the current type of rules to read }
Procedure SetProgramPath( P : ProgPtr; path : StrPtr );
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