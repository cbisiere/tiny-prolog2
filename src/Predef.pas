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
  Common,
  IChar,
  Memory,
  PObj,
  PObjTerm,
  PObjFCVI,
  PObjIO,
  PObjOp,
  PObjStr,
  PObjTok,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjBter,
  PObjRule,
  PObjQury,
  PObjStmt,
  PObjWrld,
  PObjProg,
  PObjRest,
  Tuple,
  Encoding,
  Unparse,
  Reduc,
  Expr,
  Tokenize,
  Parse,
  Debug;

Type
  TPP = (
    PP_IS_FREE,
    PP_IS_TYPE,
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
    PP_SUPPRESS,
    PP_RULE,
    PP_RETRACT,
    PP_EXPAND_FILENAME,
    PP_NEW_BUFFER,
    PP_DEL_BUFFER,
    PP_SELECT_INPUT,
    PP_SELECT_OUTPUT,
    PP_INPUT_IS,
    PP_OPEN,
    PP_CLOSE_INPUT,
    PP_CLEAR_INPUT,
    PP_IN,
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
    PP_DEF_ARRAY,
    PP_ECHO,
    PP_TRACE,
    PP_DEBUG,
    PP_DUMP,
    PP_DIF,
    PP_UNIV,
    PP_ATOM_CHARS,
    PP_FREEZE,
    PP_FAIL
  );

Procedure RegisterPredefined( P : ProgPtr );
Function IdentifierIsSyscall( I : IdPtr ) : Boolean;
Function PredefCallIsOk( P : ProgPtr; T : TermPtr; Var Predef : TPP ) : Boolean;

Function GetAtomArgAsStr( n : Byte; T : TermPtr; 
    Quotes : Boolean ) : StrPtr;

Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr; Var V,G : TermPtr; Var L : RestPtr; 
    Var Choices : Pointer ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ predefined predicates                                                      }
{----------------------------------------------------------------------------}

Const
  NB_PP = 53;
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
    (I:PP_IS_FREE;S:'sysfree';N:2),
    (I:PP_IS_TYPE;S:'sysis';N:2),
    (I:PP_CHAR_CODE;S:'syscharcode';N:2),
    (I:PP_WORLD;S:'sysworld';N:1),
    (I:PP_PARENT_WORLD;S:'sysparentworld';N:1),
    (I:PP_NEW_SUBWORLD;S:'sysnewworld';N:1),
    (I:PP_KILL_SUBWORLD;S:'syskillworld';N:2),
    (I:PP_CLIMB_WORLD;S:'sysclimbworld';N:1),
    (I:PP_DOWN_WORLD;S:'sysdownworld';N:2),
    (I:PP_TOP_STATEMENT;S:'systopstatement';N:0),
    (I:PP_BOTTOM_STATEMENT;S:'sysbottomstatement';N:0),
    (I:PP_UP_STATEMENT;S:'sysupstatement';N:1),
    (I:PP_DOWN_STATEMENT;S:'sysdownstatement';N:1),
    (I:PP_FIND_RULE;S:'sysfindrule';N:1),
    (I:PP_ASSERTA;S:'sysasserta';N:2),
    (I:PP_ASSERTZ;S:'sysassertz';N:2),
    (I:PP_SUPPRESS;S:'syssuppress';N:1),
    (I:PP_RULE;S:'sysrule';N:2),
    (I:PP_RETRACT;S:'sysretract';N:2),
    (I:PP_EXPAND_FILENAME;S:'sysexpandfilename';N:2),
    (I:PP_NEW_BUFFER;S:'sysnewbuffer';N:0),
    (I:PP_DEL_BUFFER;S:'sysdelbuffer';N:0),
    (I:PP_SELECT_INPUT;S:'sysselectinput';N:1),
    (I:PP_SELECT_OUTPUT;S:'sysselectoutput';N:1),
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_OPEN;S:'sysopennew';N:4),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN;S:'sysin';N:5),
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
    (I:PP_DEF_ARRAY;S:'sysdefarray';N:2),
    (I:PP_ECHO;S:'sysecho';N:1),
    (I:PP_TRACE;S:'systrace';N:1),
    (I:PP_DEBUG;S:'sysdebug';N:1),
    (I:PP_DUMP;S:'sysdump';N:0),
    (I:PP_DIF;S:'sysdif';N:2),
    (I:PP_UNIV;S:'sysuniv';N:2), { '=..', Edinburgh only, p.221 }
    (I:PP_ATOM_CHARS;S:'sysatomchars';N:2), { '=..', Edinburgh only, p.222 }
    (I:PP_FREEZE;S:'sysfreeze';N:2),
    (I:PP_FAIL;S:'sysfail';N:0)
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
  IdentifierIsSyscall := IdentifierEqualToShortString(I,SYSCALL_IDENT_AS_STRING)
End;

{ install the predefined identifier syscall }
Procedure RegisterPredefined( P : ProgPtr );
Var 
  T : TermPtr;
Begin
  T := EmitIdent(P,Str_NewFromShortString(SYSCALL_IDENT_AS_STRING),True)
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
  EvalPArg := ProtectedRepOf(T)
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

{ return the goal (predicate or identifier) argument n of a predicate, or a 
 variable if AcceptVar is True, or Nil if the argument is not what is expected }
Function GetGoal( n : Byte; T : TermPtr; AcceptVar : Boolean ) : TermPtr;
Var
  T1 : TermPtr;
Begin
  GetGoal := Nil;
  T1 := EvalPArg(n,T);
  If IsTuple(T1) Then
  Begin
    If Not IsIdentifier(ProtectedGetTupleHead(T1,True)) Then
      Exit
  End
  Else
    If Not IsIdentifier(T1) And Not (IsVariable(T1) And AcceptVar) Then
      Exit;
  GetGoal := T1
End;

{ return list argument n of a list, or a variable if AcceptVar is True,
 or Nil if the argument is not what is expected }
Function GetList( n : Byte; T : TermPtr; AcceptVar : Boolean ) : TermPtr;
Var
  T1 : TermPtr;
Begin
  GetList := Nil;
  T1 := EvalPArg(n,T);
  If IsNil(T1) Or ProtectedIsList(T1,True) Or (AcceptVar And IsVariable(T1)) Then
    GetList := T1
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
    s := GetIdentAsStr(I,Quotes)
  Else
  Begin
    C := EvalPArgAsString(n,T);
    If C <> Nil Then
      s := GetConstAsStr(C,False)
  End;
  GetAtomArgAsStr := s
End;

{ return True if argument n of a predicate can be assigned to Str s; 
 if Quotes is False, identifier is returned unquoted}
Function GetShortAtomArgAsStr( n : Byte; T : TermPtr; Quotes : Boolean;
    Var s : StrPtr ) : Boolean;
Begin
  GetShortAtomArgAsStr := False;
  s := GetAtomArgAsStr(n,T,Quotes);
  If s = Nil Then
    Exit;
  If Str_Length(s) > StringMaxSize Then
  Begin
    CWriteWarning('string too long: ');
    Str_CWrite(s);
    CWrite('...');
    CWriteLn;
    Exit
  End;
  GetShortAtomArgAsStr := True
End;

{ return in Result the boolean argument passed as true/false in argument n 
 of a predicate; return false if the argument is not a boolean }
Function GetBoolean( n : Byte; T : TermPtr; 
  Var Result : Boolean ) : Boolean;
Var
  I : IdPtr;
  s : TString;
Begin
  GetBoolean := False;
  I := EvalPArgAsIdent(n,T);
  If I = Nil Then
  Begin
    CWriteLnWarning('incorrect argument: boolean expected');
    Exit
  End;
  s := IdentifierGetShortString(I);
  If s = 'true' Then
    Result := True
  Else If s = 'false' Then
    Result := False
  Else
  Begin
    CWriteWarning('invalid boolean argument: ''');
    CWrite(s);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  GetBoolean := True
End;

Type
  TPrologDataType = (TYPE_CHAR,TYPE_INTEGER,TYPE_IDENT,TYPE_REAL,TYPE_STRING,
      TYPE_TERM,TYPE_DOT,TYPE_TUPLE);

{ return in Result the type argument passed as an identifier in argument n 
 of a predicate; return false if the argument is not a type identifier }
Function GetType( n : Byte; T : TermPtr; 
  Var Result : TPrologDataType ) : Boolean;
Var
  I : IdPtr;
  s : TString;
Begin
  GetType := False;
  I := EvalPArgAsIdent(n,T);
  If I = Nil Then
  Begin
    CWriteLnWarning('incorrect argument: type expected');
    Exit
  End;
  s := IdentifierGetShortString(I);
  If s = 'term' Then
    Result := TYPE_TERM
  Else If s = 'char' Then
    Result := TYPE_CHAR
  Else If s = 'ident' Then
    Result := TYPE_IDENT
  Else If s = 'integer' Then
    Result := TYPE_INTEGER
  Else If s = 'real' Then
    Result := TYPE_REAL
  Else If s = 'string' Then
    Result := TYPE_STRING
  Else If s = 'dot' Then
    Result := TYPE_DOT
  Else If s = 'tuple' Then
    Result := TYPE_TUPLE
  Else
  Begin
    CWriteWarning('invalid type argument: ''');
    CWrite(s);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  GetType := True
End;

{ return True if argument n of a predicate can be path  }
Function GetShortPathArgAsStr( n : Byte; T : TermPtr; 
    Var s : StrPtr ) : Boolean;
Begin
  GetShortPathArgAsStr := GetShortAtomArgAsStr(n,T,False,s)
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
  str := ConstGetShortString(C);
  v := ShortStringToPosInt(str,code);
  GetPosIntArg := code = 0
End;

{ get a codepoint argument n }
Function GetCodePointArg( n : Byte; T : TermPtr; Var cp : TCodePoint ) : Boolean;
Begin
  GetCodePointArg := GetPosIntArg(n,T,cp)
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
    If GetShortAtomArgAsStr(n,T,True,Alias) Then 
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
  CheckCondition(IsIdentifier(T1),
      'PredefCallIsOk: constant expected');
  SysCallCode := IdentifierGetStr(IdPtr(T1));
  CheckCondition(Str_EqualToShortString(SysCallCode,SYSCALL_IDENT_AS_STRING),
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
  str := Str_AsShortString(Ident);
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
{ terms                                                                      }
{----------------------------------------------------------------------------}

{ free(x), bound(y) }
Function ClearIsFree( T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  Free : Boolean;
Begin
  ClearIsFree := False;
  { 1: term }
  T1 := GetPArg(1,T);
  { 2: true/false }
  If Not GetBoolean(2,T,Free) Then
    Exit;
  ClearIsFree := Free And IsFree(T1) Or Not Free And IsBound(T1)
End;

{ ident(T), integer(T), string(T), real(T), dot(T), tuple(T) }
Function ClearIsType( T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  What : TPrologDataType;
  TypeOK : Boolean;
Begin
  ClearIsType := False;
  { 1: variable; should be a get as ident() is special in terms of rep }
  T1 := GetPArg(1,T);
  { 2: type of data to check for }
  If Not GetType(2,T,What) Then
    Exit;
  { check }
  Case What Of
  TYPE_INTEGER:
    TypeOK := EvaluateToInteger(T1) <> Nil;
  TYPE_REAL:
    TypeOK := EvaluateToReal(T1) <> Nil;
  TYPE_STRING:
    TypeOK := EvaluateToString(T1) <> Nil;
  TYPE_IDENT: { an assigned ident is still an ident (tested on PII+) }
    TypeOK := EvaluateToIdentifier(T1) <> Nil;
  TYPE_DOT: { list: 'nil' is not a list (tested on PII+) }
    Begin
      T1 := ProtectedRepOf(T1);
      TypeOK := ProtectedIsList(T1,True)
    End;
  TYPE_TUPLE: { tuple: a list is *not* a tuple (tested on PII+) }
    Begin
      T1 := ProtectedRepOf(T1);
      TypeOK := IsTuple(T1) And Not ProtectedIsList(T1,True)
    End;
  Else
    Begin
      CWriteLnWarning('unsupported check data type');
      Exit
    End
  End;
  ClearIsType := TypeOK
End;

{----------------------------------------------------------------------------}
{ strings                                                                    }
{----------------------------------------------------------------------------}

{ char-code(c,12) }
Function ClearCharCode( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  y : TSyntax;
  T1,T2 : TermPtr;
  Tc,Tn : TermPtr;
  C : ConstPtr;
  I : IdPtr;
  s : StrPtr;
  cc : TChar;
  cp :  TCodePoint;
  Enc : TEncoding;
Begin
  ClearCharCode := False;
  { 1: char variable or value }
  T1 := GetPArg(1,T);
  { 2: char code variable or value }
  T2 := GetPArg(2,T);

  y := GetSyntax(P);
  { case 1: char is known: char-code("A",n), char_code('A',65) }
  { get the char as a string }
  s := Nil;
  C := EvalPArgAsString(1,T);
  If C <> Nil Then
    s := ConstGetStr(C)
  Else If y = Edinburgh Then { Edinburgh: one-char atom }
    Begin
      I := EvalPArgAsIdent(1,T);
      If I <> Nil Then
        s := GetIdentAsStr(I,False)
    End;
  If s <> Nil Then
  Begin
    If (Str_Length(s) <> 1) Or Not Str_FirstChar(s,cc) Then
    Begin
      CWriteLnWarning('exactly one character is expected');
      Exit
    End;
    If Not TCharToCodePoint(cc,cp) Then
    Begin
      CWriteLnWarning('invalid codepoint');
      Exit
    End;
    Tn := EmitConst(P,Str_NewFromShortString(CodePointToShortString(cp)),CI,False);
    ClearCharCode := ReduceOneEq(T2,Tn,GetDebugStream(P));
    Exit
  End;

  { case 2: char is free: char-code(c,65), char_code(c,65) }
  If Not GetCodePointArg(2,T,cp) Then
    Exit;
  { get the TChar; we use the current input encoding, since we might unify this
   character with a character in the input source; 
   FIXME: not super-clean, as we should be able to identify the stream from 
   which the goal char_code came }
  Enc := Stream_GetEncoding(CurrentOutput(P));
  If Not CodePointToTChar(cp,cc,Enc) Then
  Begin
    CWriteLnWarning('invalid codepoint');
    Exit
  End;
  { create a constant from this TChar }
  s := Str_New(Enc);
  Str_AppendChar(s,cc);
  If y = Edinburgh Then
    Tc := EmitIdent(P,s,False)
  Else
    Tc := EmitConst(P,s,CS,False);
  { unify the term c with this constant }
  ClearCharCode := ReduceOneEq(T1,Tc,GetDebugStream(P))
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
  ClearWorld := ReduceOneEq(T1,T2,GetDebugStream(P))
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
  ClearParentWorld := ReduceOneEq(T1,T2,GetDebugStream(P))
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

{ kill-subworld("Facts"), purge("Facts") }
Function ClearKillSubWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
  CheckLeaf : Boolean;
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
  { 2: should we test for the subworld being a leaf? }
  If Not GetBoolean(2,T,CheckLeaf) Then
    Exit;
  { when requested, check the subworld is a leaf }
  If CheckLeaf And (World_GetFirstChild(W) <> Nil) then
  Begin
    CWriteWarning('cannot kill subworld "');
    Str_CWrite(Name);
    CWrite('", as it has itself one or more subworlds');
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

{ down("Facts"); optionally create the subworld if it does not exist }
Function ClearDownWorld( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  Name : StrPtr;
  W : WorldPtr;
  Create : Boolean;
Begin
  ClearDownWorld := False;
  { 1: the world's name (string) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
    Exit;
  Name := ConstGetStr(C);
  { 2: should we create it if it does not exist? }
  If Not GetBoolean(2,T,Create) Then
    Exit;
  W := World_FindChildByName(GetCurrentWorld(P),Name);
  If W <> Nil Then { this subworld exists }
    SetCurrentWorld(P,W)
  Else If Not Create Then 
  Begin
    CWriteWarning('no subworld with name "');
    Str_CWrite(Name);
    CWrite('"');
    CWriteLn;
    Exit
  End
  Else { create this subworld }
    If Not CreateNewSubWorld(P,Name,True) Then
    Begin
      CWriteWarning('fails to create subworld with name "');
      Str_CWrite(Name);
      CWrite('"');
      CWriteLn;
      Exit
    End;
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
Function ClearList( P : ProgPtr; T : TermPtr ) : Boolean;
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

{ suppress(10) : suppress 10 statements starting at the current statement; the 
 current statement is set to the next non-deleted statement; only the
 current world is affected (see Manuel d'Utilisation, section 3.2, page 5);
 PrologII only }
Function ClearSuppress( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  n : PosInt;
  W : WorldPtr;
Begin
  ClearSuppress := False;
  { 1: number of statements to suppress }
  If Not GetPosIntArg(1,T,n) Then
    Exit;
  { suppress }
  W := GetCurrentWorld(P);
  While (n > 0) And 
      (Statement_GetType(World_GetCurrentStatement(W)) <> StatementEnd) Do
  Begin
    World_SuppressCurrentStatement(W);
    n := n - 1
  End;
  ClearSuppress := True
End;

{ Return True if rule R's head and queue can be unified with term T1 and T2, 
 respectively; it is more than a test, as it changes the reduced system in 
 case of success; if Undo is True, the function makes the unification undoable
 by appending the necessary info to the restore list L }
Function UnifyHeadAndQueue( P : ProgPtr; R : RulePtr; T1,T2 : TermPtr;
    Undo : Boolean; Var L : RestPtr ) : Boolean;
Var
  Th,Tq : TermPtr;
  S : SysPtr;
  DummyM : TermsPtr;
Begin
  { extract rule's head and queue-as-a-list }
  Th := BTerm_GetTerm(Rule_GetHead(R));
  Tq := BTermsToList(P,Rule_GetQueue(R));

  { setup a system of equations to unify head and queue }
  S := Sys_New;
  Sys_InsertOneEq(S,Eq_New(REL_EQUA,T1,Th));
  Sys_InsertOneEq(S,Eq_New(REL_EQUA,T2,Tq));

  UnifyHeadAndQueue := ReduceSystem(S,Undo,L,DummyM,GetDebugStream(P))
End;

{ return a list of statements containing all the (local) rules whose head 
 and queue match terms T1 and T2, respectively }
Function GetListOfRulesMatchingHeadAndQueue( P : ProgPtr; 
    T1,T2 : TermPtr ) : StmtPtr;
Var
  Tc1,Tc2 : TermPtr;
  Ri,Rc : RulePtr;
  Ih : IdPtr;
  a : PosInt;
  Sl,St,Sth : StmtPtr;
  DummyL : RestPtr;
Begin
  GetListOfRulesMatchingHeadAndQueue := Nil;
  Ih := AccessIdentifier(T1);
  If Ih = Nil Then
    Exit;
  a := Arity(T1);
  { build the list }
  Sl := Nil; { list to be returned }
  Sth := Nil; { current list's head }
  Ri := FirstRule(P,True); { first rule in the current world }
  While Ri <> Nil Do
  Begin
    Ri := FindRuleWithHeadAndArity(Ri,Ih,a,True);
    If Ri <> Nil Then
    Begin
      { copy before reducing, to avoid binding original terms and rule;
       FIXME: CopyTerm does not work, as unification fails on *second* pass }
      Tc1 := TermPtr(DeepCopy(TObjectPtr(T1)));
      Tc2 := TermPtr(DeepCopy(TObjectPtr(T2)));
      Rc := RulePtr(DeepCopy(TObjectPtr(Ri)));

      { unifiable? note that we do not make it undoable as we work on copies }
      If UnifyHeadAndQueue(P,Rc,Tc1,Tc2,False,DummyL) Then 
      Begin
        St := Statement_New(Rule,TObjectPtr(Ri));
        If Sl = Nil Then
          Sl := St
        Else
          Statement_ChainWith(Sth,St);
        Sth := St { statement head (last generated statement) }
      End;
      Ri := NextRule(Ri,True)
    End
  End;
  GetListOfRulesMatchingHeadAndQueue := Sl
End;

{ rule(H,Q), clause(H,Q), retract(H,Q)
 succeeds for each rule matching a head and a queue (queue can be an anonymous 
 variable), at the time of first call (logical update view); if Retract is True, 
 suppress the rule upon success }
Function ClearRule( P : ProgPtr; T : TermPtr; Retract : Boolean; 
    Var L : RestPtr; Var Choices : Pointer ) : Boolean;
Var
  T1,T2 : TermPtr;
  R,Rc : RulePtr;
  St : StmtPtr;
Begin
  ClearRule := False;
  { 1: the head (ident or tuple with ident as first element) }
  T1 := GetGoal(1,T,False);
  If T1 = Nil Then
  Begin
    CWriteLnWarning('cannot unify with rule: invalid rule head');
    Exit
  End;
  { 2: the queue (list or anonymous variable) }
  T2 := GetList(2,T,True);
  If (T2 = Nil) Or (IsVariable(T2) And Not IsAnonymous(VarPtr(T2))) Then
  Begin
    CWriteLnWarning('cannot unify with rule: invalid rule queue');
    Exit
  End;

  { on first call, gather all rules matching the head and the queue, using a
   list of statements }
  If Choices = Nil Then
    Choices := GetListOfRulesMatchingHeadAndQueue(P,T1,T2);

  { no (or no more) solutions: fail }
  If Choices = Nil Then
    Exit;

  { backup the current stored solution and move to the next one for further 
   calls }
  St := StmtPtr(Choices);
  Choices := Statement_GetNext(St);

  { copy the rule }
  Rc := RulePtr(DeepCopy(Statement_GetObject(St)));

  { unification (must succeed); this test is important as it will append in L 
   what is needed to undo any bindings created by this solution, during 
   backtracking }
  If Not UnifyHeadAndQueue(P,Rc,T1,T2,True,L) Then
  Begin
    CWriteLnWarning('cannot enforce logical update view');
    Exit
  End;

  { retract rule when requested }
  If Retract Then
  Begin
    { get the actual statement }
    R := RulePtr(Statement_GetObject(St));
    St := Rule_GetStatement(R);
    { delete it }
    Statement_Suppress(St)
  End;

  ClearRule := True
End;

{ asserta(T,Q), assertz(T,Q) }
Function ClearAssert( P : ProgPtr; T : TermPtr; first : Boolean ) : Boolean;
Var
  T1,T2 : TermPtr;
  B,Bq : BTermPtr;
  Ih : IdPtr;
  R,Ri : RulePtr;
  Sti,Stb : StmtPtr;
  Wi : WorldPtr;
Begin
  ClearAssert := False;
  { 1: the head (ident or tuple with ident as first element) }
  T1 := GetGoal(1,T,False);
  If T1 = Nil Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule head');
    Exit
  End;
  { 2: the queue (list); cannot be a variable, even anonymous }
  T2 := GetList(2,T,False);
  If T2 = Nil Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule queue');
    Exit
  End;

  { make a copy of the head and the queue, applying the reduced system;  
   - use of the reduced system solves for the variables appearing in 
    assert/2: 
     -> eq(x,1) assert(data(x),nil));
    creates
      data(1) ->;
   - making a copy prevents variables to be bound to further constraints 
    after assert/2; this has been tested on PII+, and SWI as well:
     ?- assert(abc(N)), N=1.
     N = 1.
     ?- listing.
     abc(_).
     true.
    Note that assignments of identifiers are ignored, as assignments only 
    affect val/2
   }
  T1 := CopyTerm(T1,False);
  T2 := CopyTerm(T2,False);

  { create a list of BTerms representing the rule }
  B := BTerm_New(T1);
  Bq := ListToBTerms(P,T2);
  BTerms_SetNext(B,Bq);

  { create the rule from the list of BTerms }
  R := Rule_New(GetSyntax(P));
  Rule_SetTerms(R,B);
  { make the variables appear as non-temporary, as this is nicer }
  SetObjectsAsGenuine(TObjectPtr(R));

  { compute the insertion point Sti }
  Ih := AccessIdentifier(T1);
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

{----------------------------------------------------------------------------}
{ dif                                                                        }
{----------------------------------------------------------------------------}

{ dif(T1,T2) }
Function ClearDif( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  T1 := GetPArg(1,T);
  T2 := GetPArg(2,T);
  ClearDif := ReduceOneIneq(T1,T2,GetDebugStream(P))
End;

{----------------------------------------------------------------------------}
{ assignment                                                                 }
{----------------------------------------------------------------------------}

{ def_array(stack,100) }
Function ClearDefArray( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  I : IdPtr;
  n : PosInt;
  j : TArrayIndex;
  Tz : TermPtr;
Begin
  ClearDefArray := False;
  { 1: array name (identifier) }
  T1 := GetPArg(1,T);
  I := EvaluateToIdentifier(T1);
  If I = Nil Then
    Exit;
  { 2: size (positive integer) }
  If Not GetPosIntArg(2,T,n) Then
  Begin
    CWriteWarning('array size must be a positive integer');
    CWriteLn;
    Exit
  End;
  If n > MaxChildren Then
  Begin
    CWriteWarning('array too large: maximum number of elements is ');
    CWrite(PosIntToShortString(MaxChildren));
    CWriteLn;
    Exit
  End;
  If IsAssigned(I) And Not IsArray(I) Then
  Begin
    CWriteLnWarning('identifier already assigned');
    Exit
  End;
  If IsArray(I) And (GetArraySize(I) <> n) Then
  Begin
    CWriteLnWarning('array already exists but has a different size');
    Exit
  End;
  { If an array with the same name already exists and the arrays have the same 
   size, nothing happens; cf. pII+ pdf doc p.114 }
  If GetArraySize(I) = n Then
  Begin
    ClearDefArray := True;
    Exit
  End;
  SetAsAssigned(I);
  { set the identifier as an array of n zeros }
  SetArray(I,n,Array_New(n));
  Tz := EmitConst(P,Str_NewFromShortString('0'),CI,False);
  For j := 1 To n Do
    SetArrayElement(I,j,Tz);
  { success }
  ClearDefArray := True
End;

{ assign(file_name, "myfile.txt"), assign(stack(i),v))
 note: re-assignments are tricky to handle, as the reduced system, after
 e.g. "assign(test,1)", contains i=1 (w/o any remaining reference to the 
 identifier) }
Function ClearAssign( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  I : IdPtr;
  ArrIndex : PosInt;
  code : Integer;
Begin
  ClearAssign := False;
  { 1: identifier or array(index) }
  T1 := GetGoal(1,T,False);
  If T1 = Nil Then
  Begin
    CWriteLnWarning('identifier or array element expected');
    Exit
  End;
  { 2: assigned value (term) }
  T2 := EvalPArg(2,T);
  { make a clean copy, using the reduced system to eliminate intermediate 
   variables, and getting rid of bindings }
  T2 := CopyTerm(T2,False);

  If IsTuple(T1) Then { array(index) }
  Begin
    I := AccessIdentifier(T1);
    If Not IsArray(I) Then
    Begin
      CWriteLnWarning('not an array');
      Exit
    End;
    If ArgCount(T1) <> 2 Then
    Begin
      CWriteLnWarning('arrays have only one dimension');
      Exit
    End;
    T1 := ProtectedRepOf(TupleArgN(2,T1));
    If (Not IsConstant(T1)) Or 
        (ConstType(ConstPtr(T1)) <> IntegerNumber) Then
    Begin
      CWriteLnWarning('array index is not an integer');
      Exit
    End;
    ArrIndex := ShortStringToPosInt(ConstGetShortString(ConstPtr(T1)),code);
    If (code <> 0) Or (ArrIndex < 1) Or 
        (ArrIndex > GetArraySize(I)) Then
    Begin
      CWriteLnWarning('incorrect array index');
      Exit
    End;
    { do the assignment }
    SetArrayElement(I,ArrIndex,T2)
  End
  Else
  Begin
    T1 := TermPtr(EvaluateToIdentifier(T1));
    If T1 = Nil Then
    Begin
      CWriteLnWarning('identifier expected');
      Exit
    End;
    SetAsAssigned(IdPtr(T1));
    { do the assignment }
    SetValue(IdPtr(T1),T2)
  End;
  ClearAssign := True
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
  { 1: value to assign (usually contains things like add(1,2)) }
  T1 := GetPArg(1,T);
  T1 := EvaluateExpression(T1,P); 
  { do a copy, unbound any embedded variables }
  T1 := CopyTerm(T1,False);

  If T1 = Nil Then
    Exit;
  { 2: term this value is equal to (usually a variable) }
  T2 := GetPArg(2,T);
  ClearEval := ReduceOneEq(T2,T1,GetDebugStream(P)) { FIXME: shouldn't it be backtrackable? }
End;

{ '=..'(foo(a,b),[foo,a,b]) }
{ FIXME: check the logic when both arguments are set }
Function ClearUniv( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  L : TermPtr;
  Th,Tq : TermPtr;
Begin
  ClearUniv := False;
  { 1: predicate, atom or variable (e.g. foo(bb,cc), cc, 1, "hi", V}
  T1 := EvalPArg(1,T);
  { 2: list or free variable }
  T2 := GetList(2,T,True);
  If T2 = Nil Then
  Begin
    CWritelnWarning('univ: second argument must be a list or a variable');
    Exit
  End;
  If ProtectedGetList(T2,Th,Tq,True) And Not IsAtomic(Th) Then
  Begin
    CWritelnWarning('univ: first element of the list must be atomic');
    Exit
  End;

  If IsVariable(T1) And IsVariable(T2) Then
  Begin
    CWritelnWarning('univ: insufficiently instantiated arguments');
    Exit
  End;

  L := T1;  { T =.. [foo,a,b] gives T = foo(a,b) }
  T := T2;  { foo(a,b) =.. L gives L = [foo,a,b] }

  If IsVariable(T1) Then
  Begin
    T := ListToTuple(T2);       { T =.. [foo,a,b] gives T = foo(a,b) }
    If TupleArgCount(T) = 1 Then
      T := TupleArgN(1,T)       { T =.. [foo] gives T = foo }
  End
  Else If IsAtomic(T1) Then
    L := NewList2(P,T1,Nil)     { foo =.. L gives L = [foo] }
  Else If IsTuple(T1) Then
    L := TupleToList(P,T1);     { foo(a,b) =.. L gives L = [foo,a,b] }

  ClearUniv := ReduceOneEq(T,L,GetDebugStream(P)) { TODO: backtrackable? }
End;

{ atom_chars('hello',['h','e','l','l','o']) }
Function ClearAtomChars( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearAtomChars := False;
  T1 := EvalPArg(1,T);
  T2 := EvalPArg(2,T);
  { T1 known }
  If IsIdentifier(T1) Then
  Begin
    ClearAtomChars := ReduceOneEq(IdentifierToList(P,IdPtr(T1)),T2,
        GetDebugStream(P));
    Exit
  End;
  { T1 unknown }
  T2 := ListToIdentifier(P,T2);
  If T2 = Nil Then
    Exit;
  ClearAtomChars := ReduceOneEq(T1,T2,GetDebugStream(P))
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
  str := ConstGetShortString(C1);
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
  Id2 := IdentifierGetShortString(I2);
  If Not IsOpTypeString(Id2) Then
    Exit;
  ot := PStrToOpType(Id2);
  { 3: operator (string or identifier) }
  I3 := EvalPArgAsIdent(3,T);
  If I3 <> Nil Then
    Id3 := IdentifierGetShortString(I3)
  Else
  Begin
    C3 := EvalPArgAsString(3,T);
    If C3 = Nil Then
      Exit;
    Id3 := ConstGetShortString(C3) { limits to StringMaxSize chars }
  End;
  { 4: functional symbol (identifier) }
  I4 := EvalPArgAsIdent(4,T);
  If I4 = Nil Then
    Exit;
  Id4 := IdentifierGetShortString(I4);
  { not allowed: existing function with same number of parameters }
  o := Op_Lookup(P^.PP_OPER,[OP_FUNCTION,OP_ARITY,OP_PRECEDENCE],
      '',Id4,[],TOpTypeToArity(ot),1200);
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
  Path,Pattern : TPath;
  ShortPath : TShortPath;
  ShortPat : TString;
  s,s2 : StrPtr;
  L : TermPtr;
  DirInfo: SearchRec;
Begin
  ClearExpandFileName := False;
  { 1: pattern to expand }
  If Not GetShortPathArgAsStr(1,T,Pattern) Then 
    Exit;
  { extract path part }
  ShortPat := Str_AsShortString(Pattern);
  ShortPat := OSFilename(ShortPat); { handle ~ }
  
  ShortPath := ExtractPath(ShortPat); { path part, with training sep }
  Path := Str_NewFromShortString(ShortPath);

  L := NewEmptyList(P);
  FindFirst(ShortPat,AnyFile,DirInfo);
  While DosError = 0 do
  Begin
    { build the full path }
    s := Str_Clone(Path);
    s2 := Str_NewFromBytes(DirInfo.Name,GetSystemCEncoding);
    Str_Concat(s,s2);
    If GetSyntax(P) = Edinburgh Then { Edinburgh uses ident }
      T1 := EmitIdent(P,s,False)
    Else
      T1 := EmitConst(P,s,CS,False);
    L := NewList2(P,T1,L);
    FindNext(DirInfo)
  End;
  ClearExpandFileName := ReduceOneEq(L,GetPArg(2,T),GetDebugStream(P))
End;

{----------------------------------------------------------------------------}
{ buffers                                                                    }
{----------------------------------------------------------------------------}

{ to implement new-buffer(T); this buffer is put on top but has no mode so it 
 should not interfere with primitives working on default i or o streams }
Function ClearNewBuffer( P : ProgPtr ) : Boolean;
Var
  f : StreamPtr;
  y : TSyntax;
Begin
  ClearNewBuffer := False;
  y := GetSyntax(P);
  If y = Edinburgh Then
  Begin
    CWriteLnWarning('not supported in Edinburgh mode');
    Exit
  End;
  f := Stream_NewBuffer(BufferAlias(y));
  If f = Nil Then
  Begin
    CWriteLnWarning('fail to create a new buffer');
    Exit
  End;
  PushStream(P,f);
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
  If Stream_IsLocked(f) Then
  Begin
    CWriteWarning('file already in use: ''');
    Str_CWrite(Stream_GetAlias(f));
    CWrite('''');
    CWriteLn;
    Exit
  End;
  SetStreamAsCurrent(P,f);
  { if target mode is different from the file's current mode, switch it }
  If (Stream_GetDeviceType(f) In [DEV_FILE,DEV_BUFFER]) 
      And (Stream_GetMode(f) <> Mode) Then
  Begin
    Stream_Close(f);
    Stream_SetMode(f,Mode)
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
  If Stream_GetDeviceType(f) = DEV_FILE Then
  Begin
    If Stream_IsLocked(f) Then
    Begin
      CWriteWarning('file already in use: ''');
      Str_CWrite(Stream_GetAlias(f));
      CWrite('''');
      CWriteLn;
      Exit
    End;
    If Stream_GetMode(f) <> Mode Then
    Begin
      CWriteWarning('file not opened in this mode: ''');
      Str_CWrite(Stream_GetAlias(f));
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
  If Not GetShortPathArgAsStr(1,T,Path) Then 
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
  OpenMode := IdentifierGetShortString(I2);
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
  Alias := Path; { default; note: no copy is made }
  T4 := EvalPArg(4,T);
  If Not IsNil(T4) Then { [] }
  Begin
    If Not ProtectedGetList(T4,P1,P2,True) Then
    Begin
      CWriteLnWarning('file option must be a list');
      Exit
    End;
    If Not IsNil(P2) Then { only one option is allowed, for now }
    Begin
      CWriteLnWarning('two many options, as only ''alias'' is supported');
      Exit
    End;
    If Not ProtectedGetFunc1(P1,'alias',Ta,True) Then
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
    Alias := IdentifierGetStr(IdPtr(Ta))
  End;

  { ok, we have Filename, Mode, Td (free var for file descriptor), Alias }

  { warn and fail: a stream with the same path exists }
  If GetStreamByPath(P,Path) <> Nil Then
  Begin
    CWriteWarning('a stream with that path already exist: ''');
    Str_CWrite(Path);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { warn and fail: a stream with the same alias exists; this also prevents
   PII/PII+ users from fiddling with consoles or buffers }
  If GetStreamByAlias(P,Alias) <> Nil Then
  Begin
    CWriteWarning('a stream with that name already exist: ''');
    Str_CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;

  { create the new user stream }
  f := Stream_New(Alias,Path,DEV_FILE,Mode,False,True);
  If f = Nil Then
  Begin
    CWriteWarning('fail to create stream: ''');
    Str_CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  If Not Stream_IsOpen(f) Then
  Begin
    CWriteWarning('fail to open: ''');
    Str_CWrite(Alias);
    CWrite('''');
    CWriteLn;
    Exit
  End;

  { bind the free variable with the file descriptor }
  Td := EmitConst(P,Str_NewFromShortString(PosIntToShortString(Stream_GetDescriptor(f))),CI,True);
  If Not ReduceOneEq(T3,Td,GetDebugStream(P)) Then
  Begin
    CWriteWarning('failed to bind the file descriptor: ''');
    Str_CWrite(Alias);
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
  Alias := Stream_GetAlias(f);
  T1 := EmitConst(P,Alias,CS,False);
  ClearStreamIs := ReduceOneEq(GetPArg(1,T),T1,GetDebugStream(P))
End;

{ clear_input }
Function ClearClearInput( P : ProgPtr ) : Boolean;
Begin
  Stream_ClearInput(CurrentInput(P));
  ClearClearInput := True
End;

{ flush }
Function ClearFlush( P : ProgPtr ) : Boolean;
Begin
  Stream_Flush(CurrentOutput(P));
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
  Outln(CurrentOutput(P));
  ClearLine := True
End;

{ clear }
Function ClearClrScr( P : ProgPtr ) : Boolean;
Begin
  If OutputIsConsole(P) Then
    CrtClrSrc;
  ClearClrScr := True
End;


{----------------------------------------------------------------------------}
{ in                                                                         }
{----------------------------------------------------------------------------}

{ read_term(Stream,T), get_char(Stream,C), next_char(Stream,C)... }
Function ClearIn( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T2,Tr : TermPtr;
  What : TPrologDataType;
  SkipSpaces : Boolean;
  LookAhead : Boolean; { advance read requested, undo all the reads }
  f : StreamPtr;
  c : TChar;
  K : TokenPtr;
  e : TIChar;
  Success : Boolean;
  s : StrPtr;
Begin
  ClearIn := False;
  { 1: stream }
  f := GetStreamArg(P,1,T);
  If f = Nil Then
    Exit;
  If Stream_GetMode(f) <> MODE_READ Then
  Begin
    CWriteWarning('stream not open for read: ''');
    Str_CWrite(Stream_GetAlias(f));
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { 2: variable }
  T2 := GetPArg(2,T);
  { 3: what type of data to read }
  If Not GetType(3,T,What) Then
    Exit;
  { 4: skip spaces? (true/false) }
  If Not GetBoolean(4,T,SkipSpaces) Then
    Exit;
  { 5: push back all the characters read? (true/false) }
  If Not GetBoolean(5,T,LookAhead) Then
    Exit;

  { buffer in console input, when necessary }
  Stream_CheckConsoleInput(f,SkipSpaces);

  { skip blank characters when requested; even if a lookahead is requested, 
   leading spaces are not unread (PII+ p.126 reads: "Reads all blank 
   characters, if any, and then behaves like next_char(t)") }
  If SkipSpaces Then
    ReadBlanks(f);

  { undo point  }
  Stream_NextIChar(f,e);

  { read target in Tr}
  Case What Of
  TYPE_CHAR:
    Begin
      Stream_GetChar(f,c);
      Success := Not Error;
      If Success Then
      Begin
        s := Stream_NewStr(f);
        If GetSyntax(P) = Edinburgh Then { Edinburgh: return a one-char atom }
        Begin
          Str_AppendChar(s,c);
          Tr := EmitIdent(P,s,False)
        End
        Else
        Begin
          Str_AppendChar(s,c);
          Tr := EmitConst(P,s,CS,False)
        End
      End
    End;
  TYPE_INTEGER:
    Begin
      K := ReadInteger(f);
      Success := (Not Error) And (K <> Nil);
      If Success Then
        Tr := EmitConst(P,Token_GetStr(K),CI,False)
    End;
  TYPE_REAL:
    Begin
      K := ReadNumber(f,GetSyntax(P));
      Success := (Not Error) And (K <> Nil) And (Token_GetType(K) = TOKEN_REAL);
      If Success Then
        Tr := EmitConst(P,Token_GetStr(K),CR,False)
    End;
  TYPE_STRING: { FIXME: PII+ p127 }
    Begin
      K := ReadString(f);
      Success := (Not Error) And (K <> Nil);
      If Success Then
        Tr := EmitConst(P,Token_GetStr(K),CS,False)
    End;
  TYPE_IDENT:
    Begin
      K := ReadVariableOrIdentifier(f,GetSyntax(P));
      Success := (Not Error) And (K <> Nil) And (Token_GetType(K) = TOKEN_IDENT);
      If Success Then
        Tr := EmitIdent(P,Token_GetStr(K),False)
    End;
  TYPE_TERM:
    Begin
      Tr := ParseOneTerm(f,P);
      Success := Not Error
    End;
  Else
    Begin
      CWriteLnWarning('unsupported read data type');
      Success := False
    End
  End;

  { try to bound the variable to the term read }
  If Success Then
    Success := ReduceOneEq(T2,Tr,GetDebugStream(P));

  { undo read when requested or in case of failure }
  If LookAhead Or Not Success Then
    Stream_UngetChars(f,e.Lnb,e.Pos);

  ClearIn := Success
End;

{----------------------------------------------------------------------------}
{ trace, debug                                                               }
{----------------------------------------------------------------------------}

{ echo; no-echo }
Function ClearEcho( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  State : Boolean;
Begin
  ClearEcho := False;
  { 1: true/false }
  If Not GetBoolean(1,T,State) Then
    Exit;
  SetEcho(P,State);
  ClearEcho := True
End;

{ trace; no-trace }
Function ClearTrace( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  State : Boolean;
Begin
  ClearTrace := False;
  { 1: true/false }
  If Not GetBoolean(1,T,State) Then
    Exit;
  SetTrace(P,State);
  ClearTrace := True
End;

{ debug; no-debug }
Function ClearDebug( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  State : Boolean;
Begin
  ClearDebug := False;
  { 1: true/false }
  If Not GetBoolean(1,T,State) Then
    Exit;
  SetDebug(P,State);
  ClearDebug := True
End;

{ bt }
Function ClearBacktrace( P : ProgPtr; Q : QueryPtr ) : Boolean;
Begin
  DumpBacktrace(GetOutputConsole(P),Q);
  ClearBacktrace := True
End;

{ dump }
Function ClearDump( P : ProgPtr ) : Boolean;
Begin
  CoreDumpProg(P,'CORE DUMP:',False);
  ClearDump := True
End;

{----------------------------------------------------------------------------}
{ control                                                                    }
{----------------------------------------------------------------------------}

{ freeze(x,goal) }
Function ClearFreeze( P : ProgPtr; T : TermPtr; 
    Var V,G : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearFreeze := False;
  V := Nil;
  G := Nil;
  { 1: variable on which to freeze (actually, any term is accepted) }
  T1 := GetPArg(1,T);
  { 2: goal (identifier or predicate) }
  T2 := GetGoal(2,T,False);
  If T2 = Nil Then
  Begin
    CWritelnWarning('freeze: second argument must be a goal');
    Exit
  End;
  { returned values }
  V := T1; { term (usually a variable) controlling the frozen term }
  G := T2; { term that must be either cleared or frozen }
  ClearFreeze := True
End;

{----------------------------------------------------------------------------}
{ dispatch                                                                   }
{----------------------------------------------------------------------------}

{ clear a predefined predicate syscall(Code,Arg1,...ArgN), meaning 
 Code(Arg1,...,ArgN), except insert; G returns the new goal to freeze or clear }
Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr; Var V,G : TermPtr; 
    Var L : RestPtr; Var Choices : Pointer ) : Boolean;
Var
  Ok : Boolean;
Begin
  Case Predef Of
  PP_IS_FREE:
    Ok := ClearIsFree(T);
  PP_IS_TYPE:
    Ok := ClearIsType(T);
  PP_CHAR_CODE:
    Ok := ClearCharCode(P,T);
  PP_DIF:
    Ok := ClearDif(P,T);
  PP_ASSIGN:
    Ok := ClearAssign(P,T);
  PP_DEF_ARRAY:
    Ok := ClearDefArray(P,T);
  PP_EVAL:
    Ok := ClearEval(P,T);
  PP_UNIV:
    Ok := ClearUniv(P,T);
  PP_ATOM_CHARS:
    Ok := ClearAtomChars(P,T);
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
  PP_SUPPRESS:
    Ok := ClearSuppress(P,T);
  PP_RULE:
    Ok := ClearRule(P,T,False,L,Choices);
  PP_RETRACT:
    Ok := ClearRule(P,T,True,L,Choices);
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
    Ok := ClearList(P,T);
  PP_OUT:
    Ok := ClearOut(P,T);
  PP_OUTM:
    Ok := ClearOutm(P,T);
  PP_LINE:
    Ok := ClearLine(P);
  PP_CLRSRC:
    Ok := ClearClrScr(P);
  PP_IN:
    Ok := ClearIn(P,T);
  PP_ECHO:
    Ok := ClearEcho(P,T);
  PP_TRACE:
    Ok := ClearTrace(P,T);
  PP_DEBUG:
    Ok := ClearDebug(P,T);
  PP_BACKTRACE:
    Ok := ClearBacktrace(P,Q);
  PP_DUMP:
    Ok := ClearDump(P);
  PP_FREEZE:
    Ok := ClearFreeze(P,T,V,G);
  PP_FAIL:
    Ok := False
  End;

  ClearPredef := Ok
End;

End.