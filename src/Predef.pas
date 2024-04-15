{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Predef.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024   s                                          }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  P R E D E F I N E D   P R E D I C A T E S                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Predef;

Interface

Uses
  Dos,
  ShortStr,
  Num,
  Errs,
  Chars,
  Crt2,
  Files,
  Trace,
  Memory,
  PObj,
  PObjIO,
  PObjOp,
  PObjStr,
  PObjDict,
  PObjTerm,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjStmt,
  PObjWrld,
  PObjProg,
  Encoding,
  Unparse,
  Reduc,
  Expr,
  Parse,
  Debug;

Type
  TPP = (
    PP_IS_IDENT,
    PP_IS_INTEGER,
    PP_IS_STRING,
    PP_CHAR_CODE,
    PP_WORLD,
    PP_PARENT_WORLD,
    PP_NEW_SUBWORLD,
    PP_KILL_SUBWORLD,
    PP_CLIMB_WORLD,
    PP_DOWN_WORLD,
    PP_TOP_STATEMENT,
    PP_BOTTOM_STATEMENT,
    PP_UP_STATEMENT,
    PP_DOWN_STATEMENT,
    PP_FIND_RULE,
    PP_ASSERTA,
    PP_ASSERTZ,
    PP_EXPAND_FILENAME,
    PP_NEW_BUFFER,
    PP_DEL_BUFFER,
    PP_SELECT_INPUT,
    PP_SELECT_OUTPUT,
    PP_INPUT_IS,
    PP_OPEN,
    PP_CLOSE_INPUT,
    PP_CLEAR_INPUT,
    PP_IN_TERM,
    PP_IN_CHAR,
    PP_IN_CHAR_SKIP_SPACES,
    PP_OUTPUT_IS,
    PP_CLOSE_OUTPUT,
    PP_FLUSH,
    PP_QUIT,
    PP_INSERT,
    PP_LIST,
    PP_OUT,
    PP_OUTM,
    PP_LINE,
    PP_BACKTRACE,
    PP_CLRSRC,
    PP_EVAL,
    PP_OP,
    PP_ASSIGN,
    PP_DUMP,
    PP_DIF,
    PP_UNIV
  );

Procedure RegisterPredefined( P : ProgPtr );
Function IdentifierIsSyscall( I : IdPtr ) : Boolean;
Function PredefCallIsOk( P : ProgPtr; T : TermPtr; Var Predef : TPP ) : Boolean;

Function GetAtomArgAsStr( n : Byte; T : TermPtr; 
    Quotes : Boolean ) : StrPtr;

Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ predefined predicates                                                      }
{----------------------------------------------------------------------------}

Const
  NB_PP = 46;
  MAX_PP_LENGHT = 21; { max string length }
  SYSCALL_IDENT_AS_STRING = 'syscall'; 
Type
  TPPRec = Record
    I : TPP; { identifier }
    S : String[MAX_PP_LENGHT]; { identifier as string }
    N : Byte; { number of arguments }
  End;
  TPPArray = Array[1..NB_PP] Of TPPRec;

{ predefined predicates; use identifiers that are portable 
 across all the supported syntaxes: lowercase letters with at least two
 letters }

Const 
  PPArray : TPPArray = (
    (I:PP_IS_IDENT;S:'sysisdent';N:1),
    (I:PP_IS_INTEGER;S:'sysisinteger';N:1),
    (I:PP_IS_STRING;S:'sysisstring';N:1),
    (I:PP_CHAR_CODE;S:'syscharcode';N:2),
    (I:PP_WORLD;S:'sysworld';N:1),
    (I:PP_PARENT_WORLD;S:'sysparentworld';N:1),
    (I:PP_NEW_SUBWORLD;S:'sysnewworld';N:1),
    (I:PP_KILL_SUBWORLD;S:'syskillworld';N:1),
    (I:PP_CLIMB_WORLD;S:'sysclimbworld';N:1),
    (I:PP_DOWN_WORLD;S:'sysdownworld';N:1),
    (I:PP_TOP_STATEMENT;S:'systopstatement';N:0),
    (I:PP_BOTTOM_STATEMENT;S:'sysbottomstatement';N:0),
    (I:PP_UP_STATEMENT;S:'sysupstatement';N:1),
    (I:PP_DOWN_STATEMENT;S:'sysdownstatement';N:1),
    (I:PP_FIND_RULE;S:'sysfindrule';N:1),
    (I:PP_ASSERTA;S:'sysasserta';N:2),
    (I:PP_ASSERTZ;S:'sysassertz';N:2),
    (I:PP_EXPAND_FILENAME;S:'sysexpandfilename';N:2),
    (I:PP_NEW_BUFFER;S:'sysnewbuffer';N:0),
    (I:PP_DEL_BUFFER;S:'sysdelbuffer';N:0),
    (I:PP_SELECT_INPUT;S:'sysselectinput';N:1),
    (I:PP_SELECT_OUTPUT;S:'sysselectoutput';N:1),
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_OPEN;S:'sysopennew';N:4),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN_TERM;S:'sysinterm';N:2),
    (I:PP_IN_CHAR;S:'sysinchar';N:2),
    (I:PP_IN_CHAR_SKIP_SPACES;S:'sysincharskipspaces';N:2),
    (I:PP_OUTPUT_IS;S:'sysoutputis';N:1),
    (I:PP_CLOSE_OUTPUT;S:'syscloseoutput';N:1),
    (I:PP_FLUSH;S:'sysflush';N:0),
    (I:PP_QUIT;S:'sysquit';N:0),
    (I:PP_INSERT;S:'sysinsert';N:1),
    (I:PP_LIST;S:'syslist';N:1),
    (I:PP_OUT;S:'sysout';N:1),
    (I:PP_OUTM;S:'sysoutm';N:1),
    (I:PP_LINE;S:'sysline';N:0),
    (I:PP_BACKTRACE;S:'sysbacktrace';N:0),
    (I:PP_CLRSRC;S:'sysclrsrc';N:0),
    (I:PP_EVAL;S:'syseval';N:2),
    (I:PP_OP;S:'sysop';N:4), { TODO: 3-arg version }
    (I:PP_ASSIGN;S:'sysassign';N:2),
    (I:PP_DUMP;S:'sysdump';N:0),
    (I:PP_DIF;S:'sysdif';N:2),
    (I:PP_UNIV;S:'sysuniv';N:2) { '=..', Edinburgh only, p.221 }
  );

{ lookup for a predefined predicates; set the found record; 
  return True if found  }
Function LookupPP( str : TString; Var rec : TPPRec) : Boolean;
Var 
  i : 0..NB_PP;
  Found : Boolean;
Begin
  i := 0;
  Found := False;
  While (Not Found) And (i < NB_PP) Do
  Begin
    i := i + 1;
    If PPArray[i].S = str Then
    Begin
      Found := True;
      rec := PPArray[i]
    End
  End;
  LookupPP := Found
End;


{ is an identifier a syscall? }
Function IdentifierIsSyscall( I : IdPtr ) : Boolean;
Begin
  IdentifierIsSyscall := IdentifierEqualTo(I,SYSCALL_IDENT_AS_STRING)
End;

{ install all predefined, persistent constants }
Procedure RegisterPredefined( P : ProgPtr );
Var 
  I : IdPtr;
Begin
  I := InstallIdentifier(P^.PP_DCON,Str_NewFromString(SYSCALL_IDENT_AS_STRING),True)
End;


{----------------------------------------------------------------------------}
{ helpers for predef's arguments                                             }
{----------------------------------------------------------------------------}

{ get n-th argument of the predicate represented by tuple U }
Function GetPArg( n : Byte; U : TermPtr ) : TermPtr;
Begin
  GetPArg := TupleArgN(2+n,U)
End;

{ evaluate the n-th argument of the predicate represented by tuple U }
Function EvalPArg( n : Byte; U : TermPtr ) : TermPtr;
Var
  T : TermPtr;
Begin
  T := GetPArg(n,U);
  EvalPArg := RepresentativeOf(T)
End;

{ evaluate an argument as an identifier or Nil }
Function EvalPArgAsIdent( n : Byte; U : TermPtr ) : IdPtr;
Begin
  EvalPArgAsIdent := EvaluateToIdentifier(GetPArg(n,U))
End;

{ evaluate an argument as a string or Nil }
Function EvalPArgAsString( n : Byte; U : TermPtr ) : ConstPtr;
Begin
  EvalPArgAsString := EvaluateToString(GetPArg(n,U))
End;

{ evaluate an argument as an integer or Nil }
Function EvalPArgAsInt( n : Byte; U : TermPtr ) : ConstPtr;
Begin
  EvalPArgAsInt := EvaluateToInteger(GetPArg(n,U))
End;

{ return as a string pointer (or Nil) argument n of a predicate, which can be 
 a string or an identifier; if Quotes is False, identifier is returned 
 unquoted }
Function GetAtomArgAsStr( n : Byte; T : TermPtr; 
    Quotes : Boolean ) : StrPtr;
Var
  I : IdPtr;
  C : ConstPtr;
  s : StrPtr;
Begin
  s := Nil;
  I := EvalPArgAsIdent(n,T);
  If I <> Nil Then
    s := GetIdentAsString(I,Quotes)
  Else
  Begin
    C := EvalPArgAsString(n,T);
    If C <> Nil Then
      s := GetConstAsString(C,False)
  End;
  GetAtomArgAsStr := s
End;

{ return True if argument n of a predicate can be assigned to Pascal
 string str; if Quotes is False, identifier is returned unquoted}
Function GetAtomArgAsShortStr( n : Byte; T : TermPtr; Quotes : Boolean;
    Var str : TString ) : Boolean;
Var
  s : StrPtr;
Begin
  GetAtomArgAsShortStr := False;
  s := GetAtomArgAsStr(n,T,Quotes);
  If s = Nil Then
    Exit;
  str := Str_GetFirstData(s);
  If Str_Length(s) > StringMaxSize Then
  Begin
    CWriteWarning('string too long: ');
    CWrite(str);
    CWrite('...');
    CWriteLn;
    Exit
  End;
  GetAtomArgAsShortStr := True
End;

{ return True if argument n of a predicate can be path  }
Function GetPathArgAsString( n : Byte; T : TermPtr; 
    Var str : TString ) : Boolean;
Begin
  GetPathArgAsString := GetAtomArgAsShortStr(n,T,False,str)
End;

{ get a positive integer argument n }
Function GetPosIntArg( n : Byte; T : TermPtr; Var v : PosInt ) : Boolean;
Var
  C : ConstPtr;
  code : Integer;
  str : TString;
Begin
  GetPosIntArg := False;
  C := EvalPArgAsInt(n,T);
  If C = Nil Then
    Exit;
  str := ConstGetPStr(C);
  Val(str,v,code);
  GetPosIntArg := code = 0
End;

{ get a stream from argument n, or Nil }
Function GetStreamArg( P : ProgPtr; n : Byte; T : TermPtr ) : StreamPtr;
Var
  f : StreamPtr;
  Alias : TAlias;
  Desc : TFileDescriptor;
Begin
  f := Nil;
  If GetPosIntArg(n,T,Desc) Then { case 1: file descriptor }
    f := GetStreamByDescriptor(P,Desc)
  Else { case 2: alias; leave quotes if any }
    If GetAtomArgAsShortStr(n,T,True,Alias) Then 
      f := GetStreamByAlias(P,Alias);
  GetStreamArg := f
End;

{ call to predefined T = syscall(Code,Arg1,...ArgN), meaning Code(Arg1,...,ArgN) 
 is well-formed; set Predef to the predefined predicate identified }
Function PredefCallIsOk( P : ProgPtr; T : TermPtr; Var Predef : TPP ) : Boolean;
Var
  Ident : StrPtr;
  NbArgs : Integer;
  SysCallCode : StrPtr;
  T1,T2 : TermPtr;
  rec : TPPRec;
  str : TString;
Begin
  PredefCallIsOk := False; { default is to fail }

  { coded as a functional symbol }
  CheckCondition(TypeOfTerm(T) = FuncSymbol,
      'PredefCallIsOk: functional symbol expected');
  
  { first parameter is syscall }
  T1 := TupleArgN(1,T);
  CheckCondition(TypeOfTerm(T1) = Identifier,
      'PredefCallIsOk: constant expected');
  SysCallCode := IdentifierGetStr(IdPtr(T1));
  CheckCondition(Str_EqualToString(SysCallCode,SYSCALL_IDENT_AS_STRING),
      'PredefCallIsOk: Not a syscall');

  { there are at least two arguments: 'syscall' and the identifier }
  NbArgs := TupleArgCount(T);
  If NbArgs < 2 Then 
    Exit;  

  { get the identifier }
  T2 := TupleArgN(2,T);
  Ident := IdentifierGetStr(IdPtr(T2));

  { predicate identifier is not too long }
  If Str_Length(Ident) > MAX_PP_LENGHT Then
    Exit;

  { predicate is known }
  str := Str_AsString(Ident);
  If Not LookupPP(str,rec) Then
    Exit;

  { predicate has the correct number of parameters }
  If NbArgs - 2 <> rec.N Then
    Exit;

  Predef := rec.I;
  PredefCallIsOk := True
End;


{----------------------------------------------------------------------------}
{                                                                            }
{ clear predefined predicates (except insert)                                }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ types                                                                      }
{----------------------------------------------------------------------------}

{ ident(T)}
Function ClearIsIdent( T : TermPtr ) : Boolean;
Begin
  ClearIsIdent := EvalPArgAsIdent(1,T) <> Nil
End;

{ integer(T)}
Function ClearIsInteger( T : TermPtr ) : Boolean;
Begin
  ClearIsInteger := EvalPArgAsInt(1,T) <> Nil
End;

{ string(T)}
Function ClearIsString( T : TermPtr ) : Boolean;
Begin
  ClearIsString := EvalPArgAsString(1,T) <> Nil
End;

{----------------------------------------------------------------------------}
{ strings                                                                    }
{----------------------------------------------------------------------------}

{ char-code(c,12) }
{ FIXME: does not work with UTF-8 }
Function ClearCharCode( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  Tc,Tn : TermPtr;
  C : ConstPtr;
  s : StrPtr;
  ch : TString;
  val : PosInt;
Begin
  ClearCharCode := False;
  { 1: char variable or value }
  T1 := GetPArg(1,T);
  { 2: char code variable or value }
  T2 := GetPArg(2,T);

  { char-code("A",n) }
  C := EvalPArgAsString(1,T);
  If C <> Nil Then
  Begin
    s := ConstGetStr(C);
    If Str_Length(s) <> 1 Then
      Exit;
    ch := Str_AsString(s);
    Tn := EmitConst(P,Str_NewFromString(PosIntToStr(Ord(ch[1]))),CI,False);
    ClearCharCode := ReduceOneEq(T2,Tn);
    Exit
  End;

  { char-code(c,65) }
  If Not GetPosIntArg(2,T,val) Then
    Exit;
  If val > 255 Then
    Exit;
  Tc := EmitConst(P,Str_NewFromString(Chr(val)),CS,False);
  ClearCharCode := ReduceOneEq(T1,Tc)
End;

{----------------------------------------------------------------------------}
{ worlds                                                                     }
{----------------------------------------------------------------------------}

{ world(V) }
Function ClearWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearWorld := False;
  { 1: the world's name }
  T1 := GetPArg(1,T);
  { create a constant string from the name of the current world }
  T2 := EmitConst(P,World_GetName(GetCurrentWorld(P)),CS,False);
  ClearWorld := ReduceOneEq(T1,T2)
End;

{ parent-world(V) }
Function ClearParentWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  W : WorldPtr;
Begin
  ClearParentWorld := False;
  { 1: the world's name }
  T1 := GetPArg(1,T);
  { parent of the current world }
  W := World_GetParent(GetCurrentWorld(P));
  If W = Nil Then
    Exit;
  { create a constant string from the name of parent of the current world }
  T2 := EmitConst(P,World_GetName(W),CS,False);
  ClearParentWorld := ReduceOneEq(T1,T2)
End;

{ new-subworld("Facts") 
 create a new userland subworld}
Function ClearNewSubWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
Begin
  ClearNewSubWorld := False;
  { 1: the world's name (string) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
    Exit;
  Name := ConstGetStr(C);
  { create the world if it is not a subworld of the current world }
  W := World_FindChildByName(GetCurrentWorld(P),Name);
  If W <> Nil Then
  Begin
    SetCurrentWorld(P,W);
    ClearNewSubWorld := True
  End
  Else
    ClearNewSubWorld := CreateNewSubWorld(P,Name,True) { OPT: par to check for dup }
End;

{ new-subworld("Facts") }
Function ClearKillSubWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
Begin
  ClearKillSubWorld := False;
  { 1: the world's name (string) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
    Exit;
  Name := ConstGetStr(C);
  W := World_FindChildByName(GetCurrentWorld(P),Name);
  If W = Nil Then
  Begin
    CWriteWarning('no subworld with name "');
    Str_CWrite(Name);
    CWrite('"');
    CWriteLn;
    Exit
  End;
  World_SuppressChild(GetCurrentWorld(P),W);
  ClearKillSubWorld := True
End;

{ climb("Facts") }
Function ClearClimbWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
Begin
  ClearClimbWorld := False;
  { 1: the world's name (string) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
    Exit;
  Name := ConstGetStr(C);
  { go up to this world if it exists }
  W := World_GetParent(GetCurrentWorld(P));
  If W = Nil Then
    Exit;
  If Not Str_Equal(World_GetName(W),Name) Then
    Exit;
  SetCurrentWorld(P,W);
  ClearClimbWorld := True
End;

{ down("Facts") }
Function ClearDownWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
Begin
  ClearDownWorld := False;
  { 1: the world's name (string) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
    Exit;
  Name := ConstGetStr(C);
  { go down to this world if it exists }
  W := World_FindChildByName(GetCurrentWorld(P),Name);
  If W = Nil Then
  Begin
    CWriteWarning('no subworld with name "');
    Str_CWrite(Name);
    CWrite('"');
    CWriteLn;
    Exit
  End;
  SetCurrentWorld(P,W);
  ClearDownWorld := True
End;

{----------------------------------------------------------------------------}
{ statements                                                                 }
{----------------------------------------------------------------------------}

{ top }
Function ClearTopStatement( P : ProgPtr ) : Boolean;
Var 
  W : WorldPtr;
  S : StmtPtr;
Begin
  W := GetCurrentWorld(P);
  S := Statement_GetNext(World_GetFirstStatement(W));
  World_SetCurrentStatement(W,S);
  ClearTopStatement := True
End;

{ bottom }
Function ClearBottomStatement( P : ProgPtr ) : Boolean;
Var 
  W : WorldPtr;
  S : StmtPtr;
Begin
  W := GetCurrentWorld(P);
  S := World_GetLastStatement(W);
  World_SetCurrentStatement(W,S);
  ClearBottomStatement := True
End;

{ up(5) }
Function ClearUpStatement( P : ProgPtr; T : TermPtr ) : Boolean;
Var 
  W : WorldPtr;
  S : StmtPtr;
  i,n : PosInt;
Begin
  ClearUpStatement := False;
  { 1: steps (positive integer) }
  If Not GetPosIntArg(1,T,n) Then
    Exit;
  ClearUpStatement := True;
  W := GetCurrentWorld(P);
  S := World_GetCurrentStatement(W);
  For i := 1 to n Do
  Begin
    S := Statement_FindPrevOfType(S,[Comment,Rule]);
    If S = Nil Then
      Exit;
    World_SetCurrentStatement(W,S)
  End
End;

{ down(5) }
Function ClearDownStatement( P : ProgPtr; T : TermPtr ) : Boolean;
Var 
  W : WorldPtr;
  S : StmtPtr;
  i,n : PosInt;
Begin
  ClearDownStatement := False;
  { 1: steps (positive integer) }
  If Not GetPosIntArg(1,T,n) Then
    Exit;
  ClearDownStatement := True;
  W := GetCurrentWorld(P);
  S := World_GetCurrentStatement(W);
  For i := 1 to n Do
  Begin
    S := Statement_FindNextOfType(S,[Comment,Rule]);
    If S = Nil Then
      Exit;
    World_SetCurrentStatement(W,S)
  End
End;

{ find-rule(parent): set the current rule (statement) to the first rule 
 matching the identifier }
Function ClearFindRule( P : ProgPtr; T : TermPtr ) : Boolean;
Var 
  I : IdPtr;
  R : RulePtr;
Begin
  ClearFindRule := False;
  { 1: rule head (identifier) }
  I := EvalPArgAsIdent(1,T);
  If I = Nil Then
    Exit;

  { find the rule with that head, in the current world only }
  R := FirstRuleWithHead(P,I,True);
  If R = Nil Then
    Exit;

  { set the current statement to this rule's }
  World_SetCurrentStatement(GetCurrentWorld(P),Rule_GetStatement(R));
End;

{ list(10) or list }
Function ClearList( P : ProgPtr; Q : QueryPtr; T : TermPtr ) : Boolean;
Var 
  n : PosInt;
  W : WorldPtr;
  S : StmtPtr;
  ListAll : Boolean;
Begin
  { 1: number of statements to list or 0 for all }
  If Not GetPosIntArg(1,T,n) Then
    Exit;
  W := GetCurrentWorld(P);
  ListAll := n = 0;

  { first statement to print }
  If ListAll Then
    S := World_GetFirstStatement(W)
  Else
    S := World_GetCurrentStatement(W);
  If Not (Statement_GetType(S) In [Comment,Rule]) Then
    S := Statement_FindNextOfType(S,[Comment,Rule]);

  { print }
  While (S <> Nil) And ((n > 0) Or ListAll) Do
  Begin
    Case Statement_GetType(S) Of
    Comment:
      OutOneComment(CurrentOutput(P),CommPtr(Statement_GetObject(S)));
    Rule:
      OutOneRule(CurrentOutput(P),RulePtr(Statement_GetObject(S)));
    End;
    If Not ListAll Then
      n := n - 1;
    S := Statement_FindNextOfType(S,[Comment,Rule]);
  End;
  ClearList := True
End;

{----------------------------------------------------------------------------}
{ rules                                                                      }
{----------------------------------------------------------------------------}

{ asserta(T,Q), assertz(T,Q); TODO: handle rules (we handle facts, for now) }
Function ClearAssert( P : ProgPtr; T : TermPtr; first : Boolean ) : Boolean;
Var
  T1 : TermPtr;
  Ih : IdPtr;
  R,Ri : RulePtr;
  Sti,Stb : StmtPtr;
  Wi : WorldPtr;
Begin
  ClearAssert := False;
  { 1: the head (ident or tuple with ident as first element) }
  T1 := GetPArg(1,T);

  { deep copy with eval }
  T1 := CopyTerm(T1);

  { get the head }
  Ih := AccessIdentifier(T1);
  If Ih = Nil Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule head');
    Exit
  End;

  { create the rule from the term }
  R := Rule_New(GetSyntax(P));
  Rule_SetTerms(R,BTerm_New(T1));

  { compute the insertion point Sti }
  Sti := Nil;
  If first Then { asserta }
  Begin
    Ri := FirstRuleWithHead(P,Ih,False);
    If Ri <> Nil Then
      Sti := Rule_GetStatement(Ri)
  End
  Else { assertz }
  Begin
    Ri := LastRuleWithHead(P,Ih,False);
    If Ri <> Nil Then
    Begin
      Sti := Rule_GetStatement(Ri);
      Sti := Statement_GetNext(Sti)
    End
  End;
  If Sti = Nil Then { no rules yet; insert at the end of the current world }
    Sti := World_GetLastStatement(GetCurrentWorld(P));

  { backup the insertion point of the target world, insert, and restore }
  Wi := Statement_GetWorld(Sti);
  Stb := World_GetCurrentStatement(Wi);

  { insert the rule }
  World_SetCurrentStatement(Wi,Sti);
  ProgInsertRule(P,R);
  World_SetCurrentStatement(Wi,Stb);

  ClearAssert := True
End;

{ dif(T1,T2) }
Function ClearDif( T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  T1 := GetPArg(1,T);
  T2 := GetPArg(2,T);
  ClearDif := ReduceOneIneq(T1,T2)
End;

{ assign(file_name, "myfile.txt") }
Function ClearAssign( T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  I : IdPtr;
Begin
  ClearAssign := False;
  { note: re-assignments are tricky to handle, as the reduced system, after
    e.g. "assign(test,1)", contains i=1 (w/o any remaining reference to the 
    identifier) }
  { get the identifier }
  T1 := GetPArg(1,T);
  Case TypeOfTerm(T1) Of
  Identifier: { an identifier, thus unbound (first assignment) }
    Begin
      Dict_SetGlobal(IdPtr(T1)^.TV_DVAR,True); { dict entry is now persistent }
      I := IdPtr(T1)
    End;
  Variable:
    Begin
      I := VarPtr(T1)^.TV_IRED;
      If I = Nil Then { variable has never been bound to an identifier }
        I := EvaluateToIdentifier(T1);
      If I = Nil Then
        Exit;
      { unbound the variable (which was bounded to the term) }
      UnbindVar(VarPtr(T1))
    End;
  Else
    Exit
  End;
  { neutralize its role (if any) in the reduced system as variable-like, 
    assigned identifier }
  UnbindVar(I);
  I^.TV_ASSI := True;
  { second parameter }
  T2 := GetPArg(2,T);
  { assign }
  ClearAssign := ReduceOneEq(TermPtr(I),T2) { "ident = term" }
End;

{ val(100,x) }
Function ClearEval( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearEval := False;
  { evaluate the term; it may includes variables and constraints;
    thus, our assign is similar to "cassign(i,t)";
    see p113 of the PrologII+ documentation }
  T1 := EvaluateExpression(GetPArg(1,T),P); { FIXME: do a copy and unbound variables? }
  If T1 = Nil Then
    Exit;

  T2 := GetPArg(2,T);
  ClearEval := ReduceOneEq(T2,T1) { FIXME: shouldn't it be backtrackable? }
End;

{ '=..'(foo(a,b),[foo,a,b]) }
Function ClearUniv( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  L : TermPtr;
Begin
  ClearUniv := False;
  T1 := EvalPArg(1,T);
  T2 := EvalPArg(2,T);
  { create a new list from T1 }
  Case TypeOfTerm(T1) Of
  Constant,Identifier:
    L := NewList2(P,T1,Nil); { foo =.. Y gives Y = [foo] }
  FuncSymbol:
    L := TupleToList(P,T1); { foo(a,b) =.. Y gives Y = [foo,a,b] }
  Else
    L := T1
  End;
  { create a new tuple from list T2 }
  Case TypeOfTerm(T2) Of
  FuncSymbol:
    T := ListToTuple(T2); { X =.. [foo,a,b] gives X = foo(a,b) }
  Else
    T := T2
  End;
  ClearUniv := ReduceOneEq(T,L) { TODO: backtrackable? }
End;

{ op(700,xfx,"<",inf) } { TODO: implement full specs PII+ p137 }
Function ClearOp( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C1,C3 : ConstPtr;
  I2,I3,I4 : IdPtr;
  Id2,Id3,Id4 : TString;
  str : TString;
  v,code : Integer;
  o : OpPtr;
  ot : TOpType;
Begin
  ClearOp := False;
  { 1: precedence (integer value between 1 and 1200) }
  C1 := EvalPArgAsInt(1,T);
  If C1 = Nil Then
    Exit;
  str := ConstGetPStr(C1);
  If Length(str) > 4 Then
    Exit;
  Val(str,v,code);
  If code <> 0 Then
    Exit;
  If (v < 0) Or (v > 1200) Then
    Exit;
  { 2: type of operator (identifier in a list) }
  I2 := EvalPArgAsIdent(2,T);
  If I2 = Nil Then
    Exit;
  Id2 := IdentifierGetPStr(I2);
  If Not IsOpTypeString(Id2) Then
    Exit;
  ot := PStrToOpType(Id2);
  { 3: operator (string or identifier) }
  I3 := EvalPArgAsIdent(3,T);
  If I3 <> Nil Then
    Id3 := IdentifierGetPStr(I3)
  Else
  Begin
    C3 := EvalPArgAsString(3,T);
    If C3 = Nil Then
      Exit;
    Id3 := ConstGetPStr(C3) { limits to StringMaxSize chars }
  End;
  { 4: functional symbol (identifier) }
  I4 := EvalPArgAsIdent(4,T);
  If I4 = Nil Then
    Exit;
  Id4 := IdentifierGetPStr(I4);
  { not allowed: existing function with same number of parameters }
  o := Op_Lookup(P^.PP_OPER,'',Id4,[],TOpTypeToArity(ot),1200);
  If o <> Nil Then { TODO: do not fail when both declarations match }
    Exit;
  { register the new operator }
  o := Op_Append(P^.PP_OPER,Id3,Id4,ot,v);
  ClearOp := True
End;

{ quit }
Function ClearQuit : Boolean;
Begin 
  SetQuitOn(0);
  ClearQuit := True
End;

{----------------------------------------------------------------------------}
{ paths                                                                      }
{----------------------------------------------------------------------------}

{ expand_file_name("~/*.¨", L). 
 https://www.swi-prolog.org/pldoc/doc_for?object=expand_file_name/2 
 Note: only handles ~ (home) and DOS-style wildcards (e.g., *.*) }
Function ClearExpandFileName( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  Pattern,Path : TPath;
  s : StrPtr;
  L : TermPtr;
  DirInfo: SearchRec;
Begin
  ClearExpandFileName := False;
  If Not GetPathArgAsString(1,T,Pattern) Then 
    Exit;
  Pattern := OSFilename(Pattern); { handle ~ }
  Path := ExtractPath(Pattern); { path part, with training sep }
  L := NewEmptyList(P);
  FindFirst(Pattern, AnyFile, DirInfo);
  While DosError = 0 do
  Begin
    s := Str_NewFromString(Path);
    Str_Append(s,DirInfo.Name);
    If GetSyntax(P) = Edinburgh Then { Edinburgh uses quoted ident }
    Begin
      Str_Quote(s);
      T1 := EmitIdent(P,s,False)
    End
    Else
      T1 := EmitConst(P,s,CS,False);
    L := NewList2(P,T1,L);
    FindNext(DirInfo)
  End;
  ClearExpandFileName := ReduceOneEq(L,GetPArg(2,T))
End;

{----------------------------------------------------------------------------}
{ buffers                                                                    }
{----------------------------------------------------------------------------}

{ to implement new-buffer(T); this buffer is put on top but has no mode so it 
 should not interfere with primitives working on default i or o streams }
Function ClearNewBuffer( P : ProgPtr ) : Boolean;
Var
  f : StreamPtr;
Begin
  ClearNewBuffer := False;
  If GetSyntax(P) = Edinburgh Then
  Begin
    CWriteLnWarning('not supported in Edinburgh mode');
    Exit
  End;
  f := NewBuffer;
  If f = Nil Then
  Begin
    CWriteLnWarning('fail to create a new buffer');
    Exit
  End;
  SetStreamAsCurrent(P,f);
  ClearNewBuffer := True
End;

{ delete the top buffer }
Function ClearDelBuffer( P : ProgPtr ) : Boolean;
Begin
  ClearDelBuffer := False;
  If GetSyntax(P) = Edinburgh Then
  Begin
    CWriteLnWarning('not supported in Edinburgh mode');
    Exit
  End;
  DeleteTopBuffer(P);
  ClearDelBuffer := True
End;

{----------------------------------------------------------------------------}
{ files                                                                      }
{----------------------------------------------------------------------------}

{ select a stream as the current one if it exists, otherwise fails;
 used to implement input/output("data.txt"); silently fails when the stream 
 does not exist; when it exists and has a mode different from Mode, switch the
 stream to the new mode }
Function ClearSelectStream( P : ProgPtr; T : TermPtr; 
    Mode : TStreamMode ) : Boolean;
Var
  f : StreamPtr;
Begin
  ClearSelectStream := False;
  If GetSyntax(P) = Edinburgh Then
  Begin
    CWriteLnWarning('not supported in Edinburgh mode');
    Exit
  End;
  f := GetStreamArg(P,1,T);
  If f = Nil Then
    Exit;
  If StreamIsLocked(f) Then
  Begin
    CWriteWarning('file already in use: ''');
    CWrite(StreamAlias(f));
    CWrite('''');
    CWriteLn;
    Exit
  End;
  SetStreamAsCurrent(P,f);
  { if target mode is different from the file's current mode, switch it }
  If (StreamDeviceType(f) In [DEV_FILE,DEV_BUFFER]) 
      And (StreamMode(f) <> Mode) Then
  Begin
    StreamClose(f);
    StreamSetMode(f,Mode)
  End;
  ClearSelectStream := True
End;

{ close_input("data.txt"), close_output("data.txt"); since a user filename 
 cannot be opened in more than one mode, only one syscall is needed; do nothing
 if it is a terminal (PII+ p.130: "unless it is the console or a window") }
Function ClearCloseUserFile( P : ProgPtr; T : TermPtr; 
    Mode : TStreamMode ) : Boolean;
Var
  f : StreamPtr;
Begin
  ClearCloseUserFile := False;
  f := GetStreamArg(P,1,T);
  If f = Nil Then
  Begin
    CWriteLnWarning('unknown file');
    Exit
  End;
  { close user files; do nothing for consoles and buffers }
  If StreamDeviceType(f) = DEV_FILE Then
  Begin
    If StreamIsLocked(f) Then
    Begin
      CWriteWarning('file already in use: ''');
      CWrite(StreamAlias(f));
      CWrite('''');
      CWriteLn;
      Exit
    End;
    If StreamMode(f) <> Mode Then
    Begin
      CWriteWarning('file not opened in this mode: ''');
      CWrite(StreamAlias(f));
      CWrite('''');
      CWriteLn;
      Exit
    End;
    CloseAndDeleteStream(P,f)
  End;
  ClearCloseUserFile := True
End;


{ create a new user stream;
 sysopennew(F,Mode,Fd,Opt) to implement:
 - open("data.txt",read/write,Fd,[alias(data)])
 - input/output("data.txt") 
 see 
 https://www.swi-prolog.org/pldoc/man?predicate=open/4 }
Function ClearOpenNewUserStream( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  f : StreamPtr;
  Path : TPath;
  Alias : TAlias;
  OpenMode : TString;
  Mode : TStreamMode;
  T3,T4,Td,Ta : TermPtr;
  P1,P2 : TermPtr;
  I2,I : IdPtr;
Begin
  ClearOpenNewUserStream := False;
  
  { 1: file name }
  If Not GetPathArgAsString(1,T,Path) Then 
  Begin
    CWriteLnWarning('incorrect file path argument');
    Exit
  End;
  { 2: mode (read/write) }
  I2 := EvalPArgAsIdent(2,T);
  If I2 = Nil Then
  Begin
    CWriteLnWarning('incorrect file mode argument');
    Exit
  End;
  OpenMode := IdentifierGetPStr(I2);
  If OpenMode = 'read' Then
    Mode := MODE_READ
  Else If OpenMode = 'write' Then
    Mode := MODE_WRITE
  Else
  Begin
    CWriteWarning('unsupported file mode: ''');
    CWrite(OpenMode);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { 3: file descriptor; must be a free variable }
  T3 := GetPArg(3,T);
  If Not IsFree(T3) Then
  Begin
    CWriteLnWarning('file descriptor argument must be a free variable');
    Exit
  End;
  { 4: list of options; for now, only [alias(ident)] is supported }
  Alias := Path; { default }
  T4 := EvalPArg(4,T);
  If Not IsNil(T4) Then { [] }
  Begin
    If Not GetList(T4,P1,P2,True) Then
    Begin
      CWriteLnWarning('file option must be a list');
      Exit
    End;
    If Not IsNil(P2) Then { only one option is allowed, for now }
    Begin
      CWriteLnWarning('two many options, as only ''alias'' is supported');
      Exit
    End;
    If Not GetFunc1(P1,'alias',Ta,True) Then
    Begin
      CWriteLnWarning('''alias'' is the only supported option');
      Exit
    End;
    I := EvaluateToIdentifier(Ta);
    If I = Nil Then
    Begin
      CWriteLnWarning('alias must be an identifier');
      Exit
    End;
    Alias := IdentifierGetPStr(IdPtr(Ta))
  End;

  { ok, we have Filename, Mode, Td (free var for file descriptor), Alias }

  { warn and fail: a stream with the same path exists }
  If GetStreamByPath(P,Path) <> Nil Then
  Begin
    CWriteWarning('a stream with that path already exist: ''');
    CWrite(Path);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { warn and fail: a stream with the same alias exists; this also prevents
   PII/PII+ users from fiddling with consoles or buffers }
  If GetStreamByAlias(P,Alias) <> Nil Then
  Begin
    CWriteWarning('a stream with that name already exist: ''');
    CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;

  { create the new user stream }
  f := NewStream(Alias,Path,DEV_FILE,Mode,False,True);
  If f = Nil Then
  Begin
    CWriteWarning('fail to create stream: ''');
    CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  If Not StreamIsOpen(f) Then
  Begin
    CWriteWarning('fail to open: ''');
    CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;

  { bind the free variable with the file descriptor }
  Td := EmitConst(P,Str_NewFromString(PosIntToStr(StreamDescriptor(f))),CI,True);
  If Not ReduceOneEq(T3,Td) Then
  Begin
    CWriteWarning('failed to bind the file descriptor: ''');
    CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;

  { push the new stream on top of the stack; this sets it as the new current 
   input or output }
  PushStream(P,f);

  ClearOpenNewUserStream := True
End;

{ input/output_is(s) }
Function ClearStreamIs( P : ProgPtr; T : TermPtr; Mode : TStreamMode ) : Boolean;
Var
  T1 : TermPtr;
  f : StreamPtr;
  Alias : TAlias;
Begin
  ClearStreamIs := False;
  f := GetStreamByMode(P,Mode);
  If Error Then
    Exit;
  Alias := StreamAlias(f);
  T1 := EmitConst(P,Str_NewFromString(Alias),CS,False);
  ClearStreamIs := ReduceOneEq(GetPArg(1,T),T1)
End;

{ clear_input }
Function ClearClearInput( P : ProgPtr ) : Boolean;
Begin
  ClearInputFromStream(CurrentInput(P));
  ClearClearInput := True
End;

{ flush }
Function ClearFlush( P : ProgPtr ) : Boolean;
Begin
  StreamFlush(CurrentOutput(P));
  ClearFlush := True
End;

{ out("hello") }
Function ClearOut( P : ProgPtr; T : TermPtr ) : Boolean;
Begin
  OutTerm(CurrentOutput(P),GetSyntax(P),GetPArg(1,T));
  ClearOut := True
End;

{ outm("hello"), or put_char('a'); TODO: put_char(Stream,Char) }
Function ClearOutm( P : ProgPtr; T : TermPtr ) : Boolean;
Begin
  OutTermBis(CurrentOutput(P),GetSyntax(P),GetPArg(1,T),False,False);
  ClearOutm := True
End;

{ line }
Function ClearLine( P : ProgPtr ) : Boolean;
Begin
  OutCR(CurrentOutput(P));
  ClearLine := True
End;

{ clear }
Function ClearClrScr( P : ProgPtr ) : Boolean;
Begin
  If OutputIsConsole(P) Then
    CrtClrSrc;
  ClearClrScr := True
End;

{ read_term(Stream,T) }
Function ClearInTerm( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  f : StreamPtr;
  T2,Tr : TermPtr;
Begin
  ClearInTerm := False;
  { 1: stream }
  f := GetStreamArg(P,1,T);
  If f = Nil Then
    Exit;
  If StreamMode(f) <> MODE_READ Then
  Begin
    CWriteWarning('stream not open for read: ''');
    CWrite(StreamAlias(f));
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { 2: term variable }
  T2 := GetPArg(2,T);

  { buffer in console input, when necessary; skip leading spaces }
  CheckConsoleInputStream(f,True);
  { read the term }
  Tr := ParseOneTerm(f,P);
  If Error Then
    Exit;
  ClearInTerm := ReduceOneEq(T2,Tr)
End;

{ get_char(Stream,C) }
Function ClearInChar( P : ProgPtr; T : TermPtr; SkipSpaces : Boolean ) : Boolean;
Var
  T2,Tc : TermPtr;
  c : TChar;
  f : StreamPtr;
Begin
  ClearInChar := False;
  { 1: stream }
  f := GetStreamArg(P,1,T);
  If f = Nil Then
    Exit;
  If StreamMode(f) <> MODE_READ Then
  Begin
    CWriteWarning('stream not open for read: ''');
    CWrite(StreamAlias(f));
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { 2: char variable }
  T2 := GetPArg(2,T);

  { buffer in console input, when necessary }
  CheckConsoleInputStream(f,SkipSpaces);
  { get the char }
  If SkipSpaces Then
    c := GetCharNbFromStream(f,c)
  Else
    c := GetCharFromStream(f,c);
  If Error Then
    Exit;
  Tc := EmitConst(P,Str_NewFromString(c),CS,False);
  ClearInChar := ReduceOneEq(T2,Tc)
End;

{ bt }
Function ClearBacktrace : Boolean;
Begin
  DumpBacktrace;
  ClearBacktrace := True
End;

{ dump }
Function ClearDump : Boolean;
Begin
  DumpRegisteredObject;
  ClearDump := True
End;

{ clear a predefined predicate syscall(Code,Arg1,...ArgN), 
 meaning Code(Arg1,...,ArgN), except insert }
Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr ) : Boolean;
Var
  Ok : Boolean;
Begin
  Case Predef Of
  PP_IS_IDENT:
    Ok := ClearIsIdent(T);
  PP_IS_INTEGER:
    Ok := ClearIsInteger(T);
  PP_IS_STRING:
    Ok := ClearIsString(T);
  PP_CHAR_CODE:
    Ok := ClearCharCode(P,T);
  PP_DIF:
    Ok := ClearDif(T);
  PP_ASSIGN:
    Ok := ClearAssign(T);
  PP_EVAL:
    Ok := ClearEval(P,T);
  PP_UNIV:
    Ok := ClearUniv(P,T);
  PP_OP:
    Ok := ClearOp(P,T);
  PP_QUIT:
    Ok := ClearQuit;
  PP_WORLD:
    Ok := ClearWorld(P,T);
  PP_PARENT_WORLD:
    Ok := ClearParentWorld(P,T);
  PP_NEW_SUBWORLD:
    Ok := ClearNewSubWorld(P,T);
  PP_KILL_SUBWORLD:
    Ok := ClearKillSubWorld(P,T);
  PP_CLIMB_WORLD:
    Ok := ClearClimbWorld(P,T);
  PP_DOWN_WORLD:
    Ok := ClearDownWorld(P,T);
  PP_TOP_STATEMENT:
    Ok := ClearTopStatement(P);
  PP_BOTTOM_STATEMENT:
    Ok := ClearBottomStatement(P);
  PP_UP_STATEMENT:
    Ok := ClearUpStatement(P,T);
  PP_DOWN_STATEMENT:
    Ok := ClearDownStatement(P,T);
  PP_FIND_RULE:
    Ok := ClearFindRule(P,T);
  PP_ASSERTA:
    Ok := ClearAssert(P,T,True);
  PP_ASSERTZ:
    Ok := ClearAssert(P,T,False);
  PP_EXPAND_FILENAME:
    Ok := ClearExpandFileName(P,T);
  PP_NEW_BUFFER:
    Ok := ClearNewBuffer(P);
  PP_DEL_BUFFER:
    Ok := ClearDelBuffer(P);
  PP_SELECT_INPUT:
    Ok := ClearSelectStream(P,T,MODE_READ);
  PP_SELECT_OUTPUT:
    Ok := ClearSelectStream(P,T,MODE_WRITE);
  PP_OPEN:
    Ok := ClearOpenNewUserStream(P,T);
  PP_INPUT_IS:
    Ok := ClearStreamIs(P,T,MODE_READ);
  PP_CLOSE_INPUT: 
    Ok := ClearCloseUserFile(P,T,MODE_READ);
  PP_CLEAR_INPUT: 
    Ok := ClearClearInput(P);
  PP_OUTPUT_IS: 
    Ok := ClearStreamIs(P,T,MODE_WRITE);
  PP_CLOSE_OUTPUT:
    Ok := ClearCloseUserFile(P,T,MODE_WRITE);
  PP_FLUSH:
    Ok := ClearFlush(P);
  PP_LIST:
    Ok := ClearList(P,Q,T);
  PP_OUT:
    Ok := ClearOut(P,T);
  PP_OUTM:
    Ok := ClearOutm(P,T);
  PP_LINE:
    Ok := ClearLine(P);
  PP_CLRSRC:
    Ok := ClearClrScr(P);
  PP_IN_TERM:
    Ok := ClearInTerm(P,T);
  PP_IN_CHAR:
    Ok := ClearInChar(P,T,False);
  PP_IN_CHAR_SKIP_SPACES:
    Ok := ClearInChar(P,T,True);
  PP_BACKTRACE:
    Ok := ClearBacktrace;
  PP_DUMP:
    Ok := ClearDump
  End;
    
  ClearPredef := Ok
End;

End.