{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Predef.pas                                                 }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                  P R E D E F I N E D   P R E D I C A T E S                 }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Predef;

Interface

Uses
  Dos,
  ShortStr,
  Num,
  DateTime,
  Errs,
  Chars,
  Crt2,
  Files,
  CWrites,
  Common,
  IChar,
  Memory,
  PObj,
  PObjList,
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
  Dumper;

Type
  TPP = (
    PP_IS_FREE,
    PP_IS_TYPE,
    PP_CHAR_CODE,
    PP_SUBSTRING,
    PP_FIND_PATTERN,
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
    PP_SUBWORLDS,
    PP_FIND_RULE,
    PP_ASSERT2,
    PP_ASSERT1,
    PP_SUPPRESS,
    PP_RULE,
    PP_RETRACT1,
    PP_RETRACT2,
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
    PP_SUB_EOL,
    PP_OUTPUT_IS,
    PP_CLOSE_OUTPUT,
    PP_FLUSH,
    PP_QUIT,
    PP_INSERT,
    PP_LIST,
    PP_OUT,
    PP_OUTM,
    PP_LINE,
    PP_GOTOXY,
    PP_GET_LINE_WIDTH,
    PP_SET_LINE_WIDTH,
    PP_SET_LINE_CURSOR,
    PP_BACKTRACE,
    PP_CLRSRC,
    PP_EVAL,
    PP_OP,
    PP_ASSIGN,
    PP_DEF_ARRAY,
    PP_SET_STATE,
    PP_DUMP,
    PP_DIF,
    PP_UNIV,
    PP_ATOM_CHARS,
    PP_STRING_IDENT,
    PP_LIST_STRING,
    PP_LIST_TUPLE,
    PP_SPLIT,
    PP_ATOM_LENGTH,
    PP_NUMBER_CHARS,
    PP_ARG,
    PP_FREEZE,
    PP_FIND_ALL,
    PP_BLOCK,
    PP_BLOCK_EXIT,
    PP_FAIL,
    PP_TIME
  );

Procedure RegisterPredefined( P : ProgPtr );
Function PredefCallIsOk( P : ProgPtr; T : TermPtr; Var Predef : TPP ) : Boolean;

Function GetAtomArgAsStr( n : Byte; T : TermPtr; 
    Quotes : Boolean ) : StrPtr;

Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr; Var V,G : TermPtr; 
    Var L : RestPtr; SuccessCount : PosInt; Var Choices : Pointer; 
    Var More : Boolean ) : Boolean;

Implementation
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ predefined predicates                                                      }
{----------------------------------------------------------------------------}

Const
  NB_PP = 71;
  MAX_PP_LENGTH = 21; { max string length }
Type
  TPPRec = Record
    I : TPP; { identifier }
    S : String[MAX_PP_LENGTH]; { identifier as string }
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
    (I:PP_SUBSTRING;S:'syssubstring';N:4),
    (I:PP_FIND_PATTERN;S:'sysfindpattern';N:3),
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
    (I:PP_SUBWORLDS;S:'syssubworlds';N:1),
    (I:PP_FIND_RULE;S:'sysfindrule';N:1),
    (I:PP_ASSERT1;S:'sysassert1';N:2), { Edinburgh only }
    (I:PP_ASSERT2;S:'sysassert2';N:3),
    (I:PP_SUPPRESS;S:'syssuppress';N:1),
    (I:PP_RULE;S:'sysrule';N:2),
    (I:PP_RETRACT1;S:'sysretract1';N:1),
    (I:PP_RETRACT2;S:'sysretract2';N:2),
    (I:PP_EXPAND_FILENAME;S:'sysexpandfilename';N:2),
    (I:PP_NEW_BUFFER;S:'sysnewbuffer';N:0),
    (I:PP_DEL_BUFFER;S:'sysdelbuffer';N:0),
    (I:PP_SELECT_INPUT;S:'sysselectinput';N:1),
    (I:PP_SELECT_OUTPUT;S:'sysselectoutput';N:1),
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_OPEN;S:'sysopennew';N:4),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN;S:'sysin';N:6),
    (I:PP_SUB_EOL;S:'syssubeol';N:1),
    (I:PP_OUTPUT_IS;S:'sysoutputis';N:1),
    (I:PP_CLOSE_OUTPUT;S:'syscloseoutput';N:1),
    (I:PP_FLUSH;S:'sysflush';N:0),
    (I:PP_QUIT;S:'sysquit';N:0),
    (I:PP_INSERT;S:'sysinsert';N:1),
    (I:PP_LIST;S:'syslist';N:1),
    (I:PP_OUT;S:'sysout';N:1),
    (I:PP_OUTM;S:'sysoutm';N:1),
    (I:PP_LINE;S:'sysline';N:0),
    (I:PP_GOTOXY;S:'sysgotoxy';N:2),
    (I:PP_GET_LINE_WIDTH;S:'sysgetlinewidth';N:1),
    (I:PP_SET_LINE_WIDTH;S:'syssetlinewidth';N:1),
    (I:PP_SET_LINE_CURSOR;S:'syssetlinecursor';N:1),
    (I:PP_BACKTRACE;S:'sysbacktrace';N:0),
    (I:PP_CLRSRC;S:'sysclrsrc';N:0),
    (I:PP_EVAL;S:'syseval';N:2),
    (I:PP_OP;S:'sysop';N:4), { TODO: 3-arg version }
    (I:PP_ASSIGN;S:'sysassign';N:2),
    (I:PP_DEF_ARRAY;S:'sysdefarray';N:2),
    (I:PP_SET_STATE;S:'sysonoff';N:2),
    (I:PP_DUMP;S:'sysdump';N:0),
    (I:PP_DIF;S:'sysdif';N:2),
    (I:PP_UNIV;S:'sysuniv';N:2), { '=..', Edinburgh only, p.221 }
    (I:PP_STRING_IDENT;S:'sysstringident';N:2),
    (I:PP_LIST_STRING;S:'sysliststring';N:2),
    (I:PP_LIST_TUPLE;S:'syslisttuple';N:2),
    (I:PP_SPLIT;S:'syssplit';N:2),
    (I:PP_ATOM_CHARS;S:'sysatomchars';N:2),
    (I:PP_ATOM_LENGTH;S:'sysatomlength';N:2),
    (I:PP_NUMBER_CHARS;S:'sysnumberchars';N:2),
    (I:PP_ARG;S:'sysarg';N:3),
    (I:PP_FREEZE;S:'sysfreeze';N:2),
    (I:PP_FIND_ALL;S:'sysfindall';N:3),
    (I:PP_BLOCK;S:'sysblock';N:2), { block(T,G) }
    (I:PP_BLOCK_EXIT;S:'sysblockexit';N:1), { block-exit(T) }
    (I:PP_FAIL;S:'sysfail';N:0),
    (I:PP_TIME;S:'systime';N:3)
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


{ install the predefined identifier syscall }
Procedure RegisterPredefined( P : ProgPtr );
Var 
  T : TermPtr;
Begin
  T := EmitIdent(P,Str_NewFromShortString(SPECIAL_IDENT_SYSCALL),True,True);
  CheckCondition(T <> Nil,'RegisterPredefined: not a valid identifier')
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
 variable if AcceptVar is True, or Nil if the argument is not what is expected;
 cut is not allowed }
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
  Else If IsIdentifier(T1) Then
  Begin
    If TermIsCut(T1) Then
      Exit
  End
  Else If IsVariable(T1) Then
  Begin
    If Not AcceptVar Then
      Exit
  End;
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


{ on-off states }
Type
  TPrologState = (STATE_ECHO,STATE_PAPER,STATE_TRACE,STATE_DEBUG);

{ return in Result the state argument passed as an identifier in argument n 
 of a predicate; return false if the argument is not a type identifier }
Function GetOnOffStateName( n : Byte; T : TermPtr; 
  Var Result : TPrologState ) : Boolean;
Var
  I : IdPtr;
  s : TString;
Begin
  GetOnOffStateName := False;
  I := EvalPArgAsIdent(n,T);
  If I = Nil Then
  Begin
    CWriteLnWarning('incorrect argument: on/off state expected');
    Exit
  End;
  s := IdentifierGetShortString(I);
  If s = 'echo' Then
    Result := STATE_ECHO
  Else If s = 'paper' Then
    Result := STATE_PAPER
  Else If s = 'trace' Then
    Result := STATE_TRACE
  Else If s = 'debug' Then
    Result := STATE_DEBUG
  Else
  Begin
    CWriteWarning('invalid on/off state argument: ''');
    CWrite(s);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  GetOnOffStateName := True
End;


{ type to read or return }
Type
  TPrologDataType = (TYPE_CHAR,TYPE_CODE_CHAR,TYPE_INTEGER,TYPE_IDENT,TYPE_REAL,
      TYPE_STRING,TYPE_TERM,TYPE_DOT,TYPE_TUPLE,TYPE_WORD,TYPE_SENTENCE);

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
  Else If s = 'code' Then
    Result := TYPE_CODE_CHAR
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
  Else If s = 'word' Then
    Result := TYPE_WORD
  Else If s = 'sentence' Then
    Result := TYPE_SENTENCE
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

Type
  TTimeType = (TIME_TYPE_TODAY,TIME_TYPE_EPOCH);

{ return in Result the time type argument passed as an identifier in argument n 
 of a predicate; return false if the argument is not a type identifier }
Function GetTimeType( n : Byte; T : TermPtr; 
  Var Result : TTimeType ) : Boolean;
Var
  I : IdPtr;
  s : TString;
Begin
  GetTimeType := False;
  I := EvalPArgAsIdent(n,T);
  If I = Nil Then
  Begin
    CWriteLnWarning('incorrect argument: type expected');
    Exit
  End;
  s := IdentifierGetShortString(I);
  If s = 'startofday' Then
    Result := TIME_TYPE_TODAY
  Else If s = 'epoch' Then
    Result := TIME_TYPE_EPOCH
  Else
  Begin
    CWriteWarning('invalid time type argument: ''');
    CWrite(s);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  GetTimeType := True
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

{ get a positive integer argument n bounded in [b1,b2] }
Function GetPosIntArgIn( n : Byte; T : TermPtr; b1,b2 : PosInt; 
    Var v : PosInt ) : Boolean;
Begin
  GetPosIntArgIn := False;
  If Not GetPosIntArg(n,T,v) Then
    Exit;
  If (v < b1) Or (v > b2) Then
    Exit;
  GetPosIntArgIn := True
End;

{ get a codepoint argument n }
Function GetCodePointArg( n : Byte; T : TermPtr; Var cp : TCodePoint ) : Boolean;
Begin
  GetCodePointArg := GetPosIntArg(n,T,cp)
End;

{ get a stream from argument n, or Nil; if Mode is not MODE_ANY, then search
 only for streams in mode Mode }
Function GetStreamArg( P : ProgPtr; n : Byte; T : TermPtr; 
    Mode : TStreamMode ) : StreamPtr;
Var
  f : StreamPtr;
  Alias : TAlias;
  Desc : TFileDescriptor;
Begin
  f := Nil;
  If GetPosIntArg(n,T,Desc) Then { case 1: file descriptor }
    f := GetStreamByDescriptorAndMode(P,Desc,Mode)
  Else { case 2: alias; leave quotes if any }
    If GetShortAtomArgAsStr(n,T,True,Alias) Then 
      f := GetStreamByAliasAndMode(P,Alias,Mode);
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
  CheckCondition(Str_EqualToShortString(SysCallCode,SPECIAL_IDENT_SYSCALL),
      'PredefCallIsOk: Not a syscall');

  { there are at least two arguments: 'syscall' and the identifier }
  NbArgs := TupleArgCount(T);
  If NbArgs < 2 Then 
    Exit;  

  { get the identifier }
  T2 := TupleArgN(2,T);
  Ident := IdentifierGetStr(IdPtr(T2));

  { predicate identifier is not too long }
  If Str_Length(Ident) > MAX_PP_LENGTH Then
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
  If Not TCharSetFromCodePoint(cc,cp,Enc) Then
  Begin
    CWriteLnWarning('invalid codepoint');
    Exit
  End;
  { create a constant from this TChar }
  s := Str_New(Enc);
  Str_AppendChar(s,cc);
  If y = Edinburgh Then
  Begin
    Tc := EmitIdent(P,s,True,False);
    CheckCondition(Tc <> Nil,
        'ClearCharCode: unable to create an identifier from a char')
  End
  Else
    Tc := EmitConst(P,s,CS,False);
  { unify the term c with this constant }
  ClearCharCode := ReduceOneEq(T1,Tc,GetDebugStream(P))
End;

{ substring/4
 substring("hello",1,3,s) => s = "hell" 
 substring("hello",10,1) => fail
}
Function ClearSubstring( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C1 : ConstPtr;
  T4,R : TermPtr;
  s,ss : StrPtr;
  len : TStrLength;
  n,m : PosInt;
Begin
  ClearSubstring := False;
  { 1: the string }
  C1 := EvalPArgAsString(1,T);
  If C1 = Nil Then 
    Exit;
  s := ConstGetStr(C1);
  len := Str_Length(s);
  { note: range tests below are not overkill, as we need to reject early in 
   order to avoid out-of-range on PosInt values (FIXME: still n+m might 
   overflow) }
  { 2: index of the first char }
  If Not GetPosIntArg(2,T,n) Or (n < 1) Or (n > len) Then
    Exit;
  { 3: length of the substring }
  If Not GetPosIntArg(3,T,m) Or (m < 1) Or (m > len) Or (n + m - 1 > len) Then
    Exit;
  { 4: result (the substring) }
  T4 := GetPArg(4,T);
  { result in R }
  ss := Str_Substring(s,n,m);
  R := EmitConst(P,ss,CS,False);
  ClearSubstring := ReduceOneEq(T4,R,GetDebugStream(P))
End;

{ find_pattern/3
 find_pattern("hello","ll",x) => x = 3 }
Function ClearFindPattern( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T3,R : TermPtr;
  C1,C2 : ConstPtr;
  s1,s2 : StrPtr;
  n : TStrLength;
Begin
  ClearFindPattern := False;
  { 1: the string }
  C1 := EvalPArgAsString(1,T);
  If C1 = Nil Then 
    Exit;
  s1 := ConstGetStr(C1);
  { 2: the pattern }
  C2 := EvalPArgAsString(2,T);
  If C2 = Nil Then 
    Exit;
  s2 := ConstGetStr(C2);
  { 3: the result, as position in s }
  T3 := GetPArg(3,T);
  { compute the result }
  If Not Str_FindPattern(s1,s2,n) Then
    Exit;
  R := EmitPositiveInteger(P,n);
  ClearFindPattern := ReduceOneEq(T3,R,GetDebugStream(P))
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

{ sous-mondes(l), subworlds(l); unify the list l with the list of subworlds 
 of the current words; 
 - we assume it is not recursive (i.e. it only returns immediate children) 
 - unsure about the name of the predicate in PIIv2 }
Function ClearSubWorlds( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  L : TermPtr;
  W : WorldPtr;
  C : TermPtr;
Begin
  ClearSubWorlds := False;
  { 1: list (usually a free variable) }
  T1 := EvalPArg(1,T);
  { build and set list L }
  L := NewEmptyList(P);
  W := World_GetLastChild(GetCurrentWorld(P));
  While W <> Nil Do
  Begin
    C := EmitConst(P,World_GetName(W),CS,False);
    L := NewList2(P,C,L);
    W := World_GetPrev(W)
  End;
  ClearSubWorlds := ReduceOneEq(T1,L,GetDebugStream(P))
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

{ list(10) or list; note: we do not update cursor position or use line 
 continuation character (TODO: to be tested on PII+) }
Function ClearList( P : ProgPtr; T : TermPtr ) : Boolean;
Var 
  f : StreamPtr;
  y : TSyntax;
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
  f := CurrentOutput(P);
  y := GetSyntax(P);
  While (S <> Nil) And ((n > 0) Or ListAll) Do
  Begin
    Case Statement_GetType(S) Of
    Comment:
      PutOneComment(f,y,CommPtr(Statement_GetObject(S)));
    Rule:
      PutOneRule(f,y,RulePtr(Statement_GetObject(S)));
    End;
    Stream_LineBreak(f);
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
 PrologIIv2 only }
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

{ Return True if R1 and R2's heads and queues can be unified; it is more than 
 a test, as it changes the reduced system in case of success; if Undo is True, 
 the function makes the unification undoable by appending the necessary info to 
 the restore list L }
Function UnifyRules( P : ProgPtr; R1,R2 : RulePtr;
    Undo : Boolean; Var L : RestPtr ) : Boolean;
Var
  Th1,Tq1,Th2,Tq2 : TermPtr;
  S : SysPtr;
  DummyM : TermsPtr;
Begin
  { extract rules' heads and queues-as-a-list }
  Th1 := BTerm_GetTerm(Rule_GetHead(R1));
  Tq1 := BTermsToList(P,Rule_GetQueue(R1));
  Th2 := BTerm_GetTerm(Rule_GetHead(R2));
  Tq2 := BTermsToList(P,Rule_GetQueue(R2));

  { setup a system of equations to unify heads and queues }
  S := Sys_New;
  Sys_InsertOneEq(S,Eq_New(REL_EQUA,Th1,Th2));
  Sys_InsertOneEq(S,Eq_New(REL_EQUA,Tq1,Tq2));

  UnifyRules := ReduceSystem(S,Undo,L,DummyM,GetDebugStream(P))
End;

{ return a list of statements containing all the (local) rules matching R }
Function GetListOfMatchingRules( P : ProgPtr; R : RulePtr ) : StmtPtr;
Var
  Ri,R1,R2 : RulePtr;
  Ih : IdPtr;
  a : TArity;
  Sl,St,Sth : StmtPtr;
  DummyL : RestPtr;
Begin
  GetListOfMatchingRules := Nil;

  Rule_GetSignature(R,Ih,a);
  CheckCondition(Ih <> Nil,'rule w/o access');

  { build the list }
  Sl := Nil; { list to be returned }
  Sth := Nil; { current list's head }
  Ri := FirstRule(P,True); { first rule in the current world }
  While Ri <> Nil Do
  Begin
    Ri := FindRuleWithHeadAndArity(Ri,Ih,a,True);
    If Ri <> Nil Then
    Begin
      { copy before reducing, to avoid binding original terms and rule }
      R1 := RulePtr(DeepCopy(TObjectPtr(R)));
      R2 := RulePtr(DeepCopy(TObjectPtr(Ri)));

      { unifiable? note that we do not make it undoable as we work on copies }
      If UnifyRules(P,R1,R2,False,DummyL) Then 
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
  GetListOfMatchingRules := Sl
End;

{ rule(H,Q), clause(H,Q), retract(H,Q),Retract(R)
 succeeds for each rule matching a head and a queue (queue can be an anonymous 
 variable), at the time of first call (logical update view); if Retract is True, 
 suppress the rule upon success }
Function ClearRule( P : ProgPtr; T : TermPtr; TwoParam : Boolean; 
    Retract : Boolean; Var L : RestPtr; SuccessCount : PosInt; 
    Var Choices : Pointer; Var More : Boolean ) : Boolean;
Var
  T1,T2 : TermPtr;
  B,B2,Bq : BTermPtr;
  R,Rc : RulePtr;
  St : StmtPtr;
Begin
  ClearRule := False;
  More := False;

  { 1: the head (ident or tuple with ident as first element) }
  T1 := GetGoal(1,T,False);
  If T1 = Nil Then
  Begin
    CWriteLnWarning('cannot unify with rule: invalid rule head');
    Exit
  End;

  If TwoParam Then
  Begin
    { 2: the queue (list or anonymous variable) }
    T2 := GetList(2,T,True);
    If (T2 = Nil) Or (IsVariable(T2) And Not IsAnonymous(VarPtr(T2))) Then
    Begin
      CWriteLnWarning('cannot unify with rule: invalid rule queue');
      Exit
    End;
    { create a list of BTerms representing the rule }
    B := BTerm_New(T1);
    Bq := ListToBTerms(P,T2);
    BTerms_SetNext(B,Bq);
  End
  Else
  Begin
    { create the list of BTerms }
    B := RuleExpToBTerms(P,T1);
    If B = Nil Then
    Begin
      CWriteLnWarning('invalid rule');
      Exit
    End;
    { retract((T:-true)) is equivalent to retract(T) }
    If (BTerms_GetNext(B) <> Nil) Then
    Begin 
      B2 := BTerms_GetNext(B);
      If (BTerms_GetNext(B2) = Nil) And 
          (BTerm_GetAccessTerm(B2) <> Nil) And
          IdentifierEqualToShortString(BTerm_GetAccessTerm(B2),'true') Then
        BTerms_SetNext(B,Nil)
    End
  End;

  { create the rule from the list of BTerms }
  R := Rule_New(GetSyntax(P));
  Rule_SetHeadAndQueue(R,B);

  If Not Rule_HeadIsValid(R) Then
  Begin
    CWriteLnWarning('invalid rule head');
    Exit
  End;

  { on first call, gather all rules matching the head and the queue, using a
   list of statements }
  If SuccessCount = 0 Then
    Choices := Pointer(GetListOfMatchingRules(P,R));

  { no (or no more) solutions: fail, and don't try again }
  If Choices = Nil Then
    Exit;

  { backup the current stored solution and move to the next one for further 
   calls }
  St := StmtPtr(Choices);
  Choices := Pointer(Statement_GetNext(St));

  { copy the rule }
  Rc := RulePtr(DeepCopy(Statement_GetObject(St)));

  { unification (must succeed); this test is important as it will append in L 
   what is needed to undo any bindings created by this solution, during 
   backtracking }
  If Not UnifyRules(P,Rc,R,True,L) Then
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

  More := True; { please call me back, I am not done }
  ClearRule := True
End;

{ insert a new rule;
 terms passed in B must be  beforehand, for the following reasons:
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
    Note that assignments of identifiers must be ignored, as assignments only 
    affect val/2
}
Procedure AssertRule( P : ProgPtr; R : RulePtr; First : Boolean );
Var
  Ih : IdPtr;
  Ri : RulePtr;
  Sti,Stb : StmtPtr;
  Wi : WorldPtr;
  a : TArity;
Begin
  { make the variables appear as non-temporary, as this is nicer }
  SetObjectsAsGenuine(TObjectPtr(R));

  { compute the insertion point Sti }
  Rule_GetSignature(R,Ih,a);
  Sti := Nil;
  If First Then { asserta }
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
End;

{ asserta(H), asserta((H :- Q1,Q2)), assertz(H), assertz((H :- Q1,Q2)) }
Function ClearAssert1( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
  First : Boolean;
  B : BTermPtr;
  R : RulePtr;
Begin
  ClearAssert1 := False;
  { 1: the rule (identifier, predicates, or ':-' expression) }
  T1 := GetGoal(1,T,False);
  If T1 = Nil Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule expression');
    Exit
  End;
  { 2: insert at the beginning of the group of rules having the same access and
   arity? }
  If Not GetBoolean(2,T,First) Then
    Exit;

  { make a copy to isolate the rule from further bindings }
  T1 := CopyTerm(T1,False);

  { create the list of BTerms }
  B := RuleExpToBTerms(P,T1);

  { create the rule from the list of BTerms }
  R := Rule_New(GetSyntax(P));
  Rule_SetHeadAndQueue(R,B);

  If Not Rule_HeadIsValid(R) Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule head');
    Exit
  End;

  AssertRule(P,R,First);
  ClearAssert1 := True
End;

{ asserta(T,Q), assertz(T,Q) }
Function ClearAssert2( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  First : Boolean;
  B,Bq : BTermPtr;
  R : RulePtr;
Begin
  ClearAssert2 := False;
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
  { 3: insert at the beginning of the group of rules having the same access and
   arity? }
  If Not GetBoolean(3,T,First) Then
    Exit;

  { make a copy to isolate the rule from further bindings }
  T1 := CopyTerm(T1,False);
  T2 := CopyTerm(T2,False);

  { create a list of BTerms representing the rule }
  B := BTerm_New(T1);
  Bq := ListToBTerms(P,T2);
  BTerms_SetNext(B,Bq);

  { create the rule from the list of BTerms }
  R := Rule_New(GetSyntax(P));
  Rule_SetHeadAndQueue(R,B);

  If Not Rule_HeadIsValid(R) Then
  Begin
    CWriteLnWarning('cannot create rule: invalid rule head');
    Exit
  End;

  AssertRule(P,R,First);
  ClearAssert2 := True
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
  If n > MaxPrologArraySize Then
  Begin
    CWriteWarning('array too large: maximum number of elements is ');
    CWrite(PosIntToShortString(MaxPrologArraySize));
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
  ArrIndex : TArity;
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

{----------------------------------------------------------------------------}
{ conversion                                                                 }
{----------------------------------------------------------------------------}

{ '=..'(foo(a,b),[foo,a,b]) }
{ FIXME: check the logic when both arguments are set }
Function ClearUniv( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  L : TermPtr;
  Th,Tq : TermPtr;
  n : TListArgNumber;
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

  If IsVariable(T1) And IsVariable(T2) Then
  Begin
    CWritelnWarning('univ: insufficiently instantiated arguments');
    Exit
  End;

  { pII+ p.222: "If X is free, Y must be instantiated with a list in which 
   the first element is atomic." }
  If IsVariable(T1) And 
      Not (ProtectedGetList(T2,Th,Tq,True) And IsAtomic(Th) And 
        ProtectedIsListOfKnownSize(T2,True,n)) Then
  Begin
    CWritelnWarning('univ: second argument must be a list whose first argument is atomic');
    Exit
  End;

  L := T1;  { T =.. [foo,a,b] gives T = foo(a,b) }
  T := T2;  { foo(a,b) =.. L gives L = [foo,a,b] }

  If IsVariable(T1) Then
  Begin
    T := ProtectedListToTuple(T2,True); { T =.. [foo,a,b] gives T = foo(a,b) }
    If TupleArgCount(T) = 1 Then
      T := TupleArgN(1,T)       { T =.. [foo] gives T = foo }
  End
  Else If IsAtomic(T1) Then
    L := NewList2(P,T1,Nil)     { foo =.. L gives L = [foo] }
  Else If IsTuple(T1) Then
    L := TupleToList(P,T1);     { foo(a,b) =.. L gives L = [foo,a,b] }

  ClearUniv := ReduceOneEq(T,L,GetDebugStream(P)) { TODO: backtrackable? }
End;

{ boum(hello,"hello"), string_ident("hello",hello)
 Tested on PII+: the identifier in the string must use the simplified syntax, 
 even when quoted, e.g. "aaa", "'aaa'" are valid, "123", "'123'" are not }
Function ClearStringIdent( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearStringIdent := False;
  T1 := EvalPArg(1,T);
  T2 := EvalPArg(2,T);
  { check at least one argument is bound }
  If IsVariable(T1) And IsVariable(T2) Then
  Begin
    CWritelnWarning('at least one argument must be instantiated');
    Exit
  End;
  { T1 is a string }
  If IsString(T1) Then
  Begin
    T1 := StringToIdentifier(P,ConstPtr(T1));
    If T1 = Nil Then { fails to create an identifier from the string }
    Begin
      CWritelnWarning('the string cannot be converted to an identifier');
      Exit
    End;
    ClearStringIdent := ReduceOneEq(T1,T2,GetDebugStream(P));
    Exit
  End;
  { T2 is an ident }
  If IsIdentifier(T2) Then
  Begin
    ClearStringIdent := ReduceOneEq(T1,IdentifierToString(P,IdPtr(T2)),
        GetDebugStream(P));
    Exit
  End
  { wrong type of argument: silently fails (see date/1 in PIIv1 doc p23) }
End;

{ list_string("h"."e"."l"."l"."o".nil,s) => s = "hello" }
Function ClearListString( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2,R : TermPtr;
  s : StrPtr;
Begin
  ClearListString := False;
  { 1: list of characters (must be bound) }
  T1 := EvalPArg(1,T);
  { 2: string (result) }
  T2 := EvalPArg(2,T);
  { build the string s from the list T1 }
  s := ListToStr(P,T1);
  If s = Nil Then { note: should not happen }
    Exit;
  { final result: the constant string R }
  R := EmitConst(P,s,CS,False);
  If R = Nil Then { note: should not happen }
    Exit;
  ClearListString := ReduceOneEq(T2,R,GetDebugStream(P))
End;

{ list_tuple("abc".def.123.nil,s) => s = <"abc",def,123> }
Function ClearListTuple( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2,R : TermPtr;
Begin
  ClearListTuple := False;
  { 1: list of terms (must be bound) }
  T1 := EvalPArg(1,T);
  { 2: tuple (result) }
  T2 := EvalPArg(2,T);
  { build the tuple R from the list T1 }
  R := ProtectedListToTuple(T1,True); { TBD: is True necessary? }
  If R = Nil Then { note: should not happen }
    Exit;
  ClearListTuple := ReduceOneEq(T2,R,GetDebugStream(P))
End;

{ split/2: split a string or a tuple into a list of its elements  
 split("hello",t) => t = "h"."e"."l"."l"."o".nil 
 split(aa(bb,cc),t) => t = aa.bb.cc.nil (since aa(bb,cc) == <aa,bb,cc>) )
}
Function ClearSplit( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2,R : TermPtr;
Begin
  ClearSplit := False;
  { 1: string or tuple to split (must be bound) }
  T1 := EvalPArg(1,T);
  { 2: list of its elements (result) }
  T2 := EvalPArg(2,T);
  { build the list R from the term T1 }
  If IsString(T1) Then
  Begin
    R := StrToList(P,ConstGetStr(ConstPtr(T1)));
    If R = Nil Then { note: should not happen }
      Exit
  End
  Else If IsTuple(T1) Then
  Begin
    R := TupleToList(P,T1);
    If R = Nil Then { note: should not happen }
      Exit
  End
  Else
    Exit;
 ClearSplit := ReduceOneEq(T2,R,GetDebugStream(P))
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

{ atom_length('hello',5) }
Function ClearAtomLength( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  s,sn : StrPtr;
  T1,T2 : TermPtr;
Begin
  ClearAtomLength := False;

  { 1: atom  }
  s := GetAtomArgAsStr(1,T,False);
  If s = Nil Then
  Begin
    CWriteLnWarning('first argument must be an atom');
    Exit
  End;

  { 2: length (positive integer) }
  T2 := EvalPArg(2,T);
  If Not (IsFree(T2) Or IsInteger(T2)) Then
  Begin
    CWriteLnWarning('when bound, second argument must be an integer');
    Exit
  End;

  { create a constant from the length of s }
  T1 := EmitPositiveInteger(P,Str_Length(s));
  If Error Then Exit;

  ClearAtomLength := ReduceOneEq(T1,T2,GetDebugStream(P))
End;

{ number_chars(123,['1','2','3']) }
Function ClearNumberChars( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearNumberChars := False;
  T1 := EvalPArg(1,T);
  T2 := EvalPArg(2,T);
  { T1 known }
  If IsInteger(T1) Or IsReal(T1) Then
  Begin
    ClearNumberChars := ReduceOneEq(NumToList(P,ConstPtr(T1)),T2,
        GetDebugStream(P));
    Exit
  End;
  { T1 unknown }
  T2 := TermPtr(ListToNum(P,T2));
  If T2 = Nil Then
    Exit;
  ClearNumberChars := ReduceOneEq(T1,T2,GetDebugStream(P))
End;

{ arg(n,t1,t2)
 In PIIv1, PIIv2, PII+ (in which predicate is named is arg2):
 string:
   arg(0,"hello",x) => x = 5 (length)
   arg(2,"hello",x) => x = "e" (second character)
 list:
   arg(0,aa.bb.cc.nil,x) => x = 3 (length)
   arg(2,aa.bb.cc.nil,x) => x = bb (second element)
 tuple:
   arg(0,<aa,bb,cc>,x) => x = 3 (length)
   arg(2,<aa,bb,cc>,x) => x = bb (second element)
  In PII+ Edinburgh:
   ISO arg/3?

 cf.: PIIv1: p12, book PIIv2: p148, PII+: p117 (Marseille) and p222 (Edinburgh)
}
Function ClearArg( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T2,T3,TH,TQ,R : TermPtr;
  n,m : PosInt;
  s : StrPtr;
  cc : TChar;
Begin
  ClearArg := False;
  { 1: 0 or index }
  If Not GetPosIntArg(1,T,n) Then
  Begin
    CWriteWarning('Arg: first argument must be an integer');
    CWriteLn;
    Exit
  End;
  { 2: term }
  T2 := EvalPArg(2,T);
  If Not IsBound(T2) Then
  Begin
    CWriteWarning('Arg: second argument must be bounded');
    CWriteLn;
    Exit
  End;
  { 3: result }
  T3 := EvalPArg(3,T);
  { compute result in R }
  If IsString(T2) Then { CASE 1: arg 2 is a string }
  Begin
    s := GetConstAsStr(ConstPtr(T2),False);
    If n = 0 Then { length }
    Begin
      R := EmitPositiveInteger(P,Str_Length(s));
      If Error Then Exit;
    End
    Else { n-th character }
    Begin
      If Not Str_Char(s,n,cc) Then
        Exit;
      R := EmitChar(P,Str_GetEncodingContext(s),cc)
    End
  End
  Else If ProtectedGetList(T2,TH,TQ,False) Then { CASE 2: arg 2 is a list }
  Begin
    Case GetSyntax(P) Of
    PrologIIv1,PrologIIv2: { arg/3 }
      Begin
        Case n Of { PIIv1, maybe v2}
        1: R := TH; { head }
        2: R := TQ; { queue }
        Else
          Exit
        End
      End;
    PrologIIp,Edinburgh: {arg2/3 }
      Begin
        If Not ProtectedIsListOfKnownSize(T2,True,m) Then
          Exit;
        If n = 0 Then { number of elements (excluding the final 'nil') }
          R := EmitPositiveInteger(P,m) 
        Else
        Begin { n-th element }
          If (n < 1) Or (n > m) Then
            Exit;
          R := ListArgN(n,T2) 
        End
      End
    End
  End
  Else If IsTuple(T2) Then { CASE 3: arg 2 is a tuple }
  Begin
    m := TupleArgCount(T2);
    If n = 0 Then { length }
      R := EmitPositiveInteger(P,m)
    Else
    Begin { n-th element }
      If (n < 1) Or (n > m) Then
        Exit;
      R := TupleArgN(n,T2)
    End
  End
  Else
    Exit;

  ClearArg := ReduceOneEq(T3,R,GetDebugStream(P))
End;

{----------------------------------------------------------------------------}
{ operators                                                                  }
{----------------------------------------------------------------------------}

{ op(700,xfx,"<",inf) } { TODO: implement full specs PII+ p137 }
Function ClearOp( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C3 : ConstPtr;
  I2,I3,I4 : IdPtr;
  Id2,Id3,Id4 : TString;
  v : PosInt;
  o : OpPtr;
  ot : TOpType;
Begin
  ClearOp := False;
  { 1: precedence (integer value between 1 and 1200) }
  If Not GetPosIntArgIn(1,T,1,1200,v) Then
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

{----------------------------------------------------------------------------}
{ exit                                                                       }
{----------------------------------------------------------------------------}

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
    s2 := Str_NewFromBytes(DirInfo.Name,GetSystemEncoding,GetSystemEolStyle);
    Str_Concat(s,s2);
    If GetSyntax(P) = Edinburgh Then { Edinburgh uses ident }
    Begin
      T1 := EmitIdent(P,s,True,False);
      CheckCondition(T1 <> Nil,
          'ClearExpandFileName: unable to create an identifier from a file name')
    End
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
  f := Stream_NewBuffer(BufferAlias(y),LineContinuation(y),DefaultLineWidth(y));
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
  CloseTopBuffer(P);
  ClearDelBuffer := True
End;

{----------------------------------------------------------------------------}
{ files                                                                      }
{----------------------------------------------------------------------------}

{ select a stream as the current one if it exists, otherwise fails;
 used to implement input/output("data.txt"); silently fails when the stream 
 does not exist; when it exists and has a mode different from Mode, switch the
 stream to the new mode without changing its position in the stack of streams }
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
  { first chance: steam already having the target mode (so we always catch the 
   correct console when the stream is a console) }
  f := GetStreamArg(P,1,T,Mode);
  { second chance for files or buffers: mode switching }
  If f = Nil Then
    f := GetStreamArg(P,1,T,MODE_ANY);
  If f = Nil Then
    Exit;
  { if target mode is different from the file's current mode, switch it }
  If (Stream_GetDeviceType(f) In [DEV_FILE,DEV_BUFFER]) 
      And (Stream_GetMode(f) <> Mode) Then
  Begin
    { is closing allowed? No if it is a Prolog program }
    If Stream_IsLocked(f) Then
    Begin
      CWriteWarning('file to select already in use: ''');
      Str_CWrite(Stream_GetAlias(f));
      CWrite('''');
      CWriteLn;
      Exit
    End;
    { switch mode }
    Stream_CloseFile(f);
    Stream_SetMode(f,Mode)
  End;
  SetStreamAsCurrent(P,f);
  ClearSelectStream := True
End;

{ close_input("data.txt"), close_output("data.txt"); since a user filename 
 cannot be opened in more than one mode, only one syscall is needed; do nothing
 if it is a terminal (PII+ p.130: "unless it is the console or a window");
 FIXME: PII: can we close a buffer? }
Function ClearCloseUserFile( P : ProgPtr; T : TermPtr; 
    Mode : TStreamMode ) : Boolean;
Var
  f : StreamPtr;
Begin
  ClearCloseUserFile := False;
  f := GetStreamArg(P,1,T,Mode);
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
      CWriteWarning('file to close already in use: ''');
      Str_CWrite(Stream_GetAlias(f));
      CWrite('''');
      CWriteLn;
      Exit
    End;
    CloseStream(P,f)
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
  f := CreateNewStream(P,Alias,Path,DEV_FILE,Mode,False,True);
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
  OutTermUnquoted(CurrentOutput(P),GetSyntax(P),GetPArg(1,T));
  ClearOutm := True
End;

{ line, updating current char position }
Function ClearLine( P : ProgPtr ) : Boolean;
Begin
  Stream_OutNewLine(CurrentOutput(P));
  ClearLine := True
End;

{ en-xy(0,15), set-cursor(0,15), set_cursor(1,16);
 Notes:
 - As PIIv1 doc p. 8 shows that PIIv1 uses a 0-based coordinate system, we 
   assume that it is also the case for PIIv2. PII+ uses a 1-based coordinate 
   system (see PII+ p. R 5-11).
 - PIIv1 limits X to [0,79] and Y to [0,23], w/o mentioning that the actual 
   screen size or the current line width (set by lg-ligne/1) is also a limit. 
   In contrast, PII+ p. R 5-11 seems to state that these limits are taken into 
   account. We assume this is the case for all Prolog flavors. 
 - Experiments show that set_cursor/2 has no effect in PII+. It seems that PII+
   starts in graphics mode (see e.g. current_file/3), in which indeed 
   set_cursor/2 has no effects. I do not know how to switch to console 
   (text) mode. }
Function ClearGotoXY( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  v1,v2 : PosInt;
  MaxWidth : TLineWidth;
  Base : 0..1; { coordinate of the first line/column }
  f : StreamPtr;
Begin
  ClearGotoXY := False;
  If GetSyntax(P) In [PrologIIv1,PrologIIv2] Then
    Base := 0
  Else
    Base := 1;
  f := GetOutputConsole(P);
  MaxWidth := Min(Stream_GetLineWidth(f),CrtGetScreenWidth);
  If Not GetPosIntArgIn(1,T,Base,MaxWidth-1+Base,v1) Then
    Exit;
  If Not GetPosIntArgIn(2,T,Base,CrtGetScreenHeight-1+Base,v2) Then
    Exit;
  CrtGotoXY(v1+1-Base,v2+1-Base);
  ClearGotoXY := True
End;

{ line-width(40), line_width(40)
 Notes:
 - applies to the current output file, whatever its type
 - PIIv1: doc seems to imply that the argument of lg-ligne/1 must be set, so
   there would be no way to get the current line width }
Function ClearGetLineWidth( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
  f : StreamPtr;
  Width : TLineWidth;
  s : StrPtr;
Begin
  ClearGetLineWidth := False;
  { 1: line width (usually a free variable) }
  T1 := EvalPArg(1,T);
  { get the current line width }
  f := CurrentOutput(P);
  Width := Stream_GetLineWidth(f);
  { create a constant from the line width }
  s := Str_NewFromShortString(LongIntToShortString(Width));
  If Not NormalizeConstant(s,IntegerNumber) Then
    Exit;
  T2 := EmitConst(P,s,CI,False);
  ClearGetLineWidth := ReduceOneEq(T1,T2,GetDebugStream(P))
End;

{ lg-ligne(40), set-line-width(40), set_line_width(40) 
 Notes:
 - applies to the current output file, whatever its type
 - no minimum is mentioned in the docs, we set it to 5 to accommodate the most 
   demanding Prolog version, PII: 3-space left margin, truncations of 
   identifiers, identifiers starting with two letters vs. variable names;
 - PIIv2: not documented, not even sure set-line-width/1 exists }
Function ClearSetLineWidth( P : ProgPtr; T : TermPtr ) : Boolean;
Const
  PROLOG_MIN_LINE_WIDTH = 5;
Var
  f : StreamPtr;
  v : PosInt;
  MaxWidth : TLineWidth;
Begin
  ClearSetLineWidth := False;
  { 1: new line width (integer) }
  MaxWidth := MaximumLineWidth(GetSyntax(P));
  If Not GetPosIntArgIn(1,T,PROLOG_MIN_LINE_WIDTH,MaxWidth,v) Then
  Begin
    CWriteWarning('argument must be an integer value between ');
    CWrite(PosIntToShortString(PROLOG_MIN_LINE_WIDTH));
    CWriteWarning(' and ');
    CWrite(PosIntToShortString(MaxWidth));
    CWriteLn;
    Exit
  End;
  { set the line width of the current output stream }
  f := CurrentOutput(P);
  Stream_SetLineWidth(f,v);
  ClearSetLineWidth := True
End;

{ pos(20), set-line-cursor(40), set_line_cursor(40)
 Notes:
 - FIXME: PIIv2: no doc, not even sure set-line-cursor/1 exists 
 - we assumes it applies to the current output unit, whatever its type (console, 
 file...) }
Function ClearSetLineCursor( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  f : StreamPtr;
  v : PosInt;
  Pos, Width : TLineWidth;
Begin
  ClearSetLineCursor := False;
  { current output stream }
  f := CurrentOutput(P);
  { current character position (first character is in position 1) }
  Pos := Stream_GetCharacterPosition(f);
  { stream's current line width}
  Width := Stream_GetLineWidth(f);
  { 1: new position on the current line }
  If Not GetPosIntArgIn(1,T,Pos,Width,v) Then
  Begin
    CWriteWarning('argument must be an integer value between ');
    CWrite(PosIntToShortString(Pos));
    CWrite(' and ');
    CWrite(PosIntToShortString(Width));
    CWriteLn;
    Exit
  End;
  { write blank spaces if any (no spaces to write is still a success) }
  If v-Pos > 0 Then
    Stream_OutNChar(f,CC_BLANK_SPACE,v-Pos);
  ClearSetLineCursor := True
End;

{ page/0: if the current output is a console, clear the screen and move input 
 cursor to the top-left }
Function ClearClrScr( P : ProgPtr ) : Boolean;
Begin
  Stream_Page(CurrentOutput(P));
  ClearClrScr := True
End;


{----------------------------------------------------------------------------}
{ in                                                                         }
{----------------------------------------------------------------------------}


{ fin-ligne(c), end-of-line(c)
 - PIIv1
 - possibly PIIv2 (name "end-of-line" is only a guess)
 - From PIIv1 doc p8: transform EOL into c. Default to " ".
   We interpret this as: substitute EOL by c only when clearing in-char/1 and
   friends (i.e. in-char'/0, car-apres/0, car-apres'/0)
 }
Function ClearSubEOL( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  C : ConstPtr;
  s : StrPtr;
  cc : TChar;
Begin
  ClearSubEOL := False;
  { 1: char (must be set) }
  C := EvalPArgAsString(1,T);
  If C = Nil Then
  Begin
    CWritelnWarning('EOL substitution must be a character');
    Exit
  End;
  s := ConstGetStr(C);
  If Str_Length(s) <> 1 Then
  Begin
    CWritelnWarning('EOL substitution must be exactly one character');
    Exit
  End;
  If Not Str_FirstChar(s,cc) Then { note: this call must never return False }
    Exit;
  { set the substitution character }
  SetSubChar(P,cc);
  ClearSubEOL := True
End;

{ read_term(Stream,T), get_char(Stream,C), next_char(Stream,C)...; 
 note: silently fails if Stream exists but is not an input stream }
Function ClearIn( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,T2,R1,R2 : TermPtr;
  What : TPrologDataType;
  SkipSpaces : Boolean;
  LookAhead : Boolean; { advance read requested, undo all the reads }
  y : TSyntax;
  f : StreamPtr;
  cp : TCodePoint;
  K : TokenPtr;
  e,e1 : TIChar;
  InOk : Boolean; { successfully read a term of the requested type }
  Success : Boolean;
  s : StrPtr;
  ss : TString;
  IsTerminator : Boolean;
Begin
  ClearIn := False;
  R1 := Nil; { to be bound to T1 }
  R2 := Nil; { if not nil, will be bound to T2 }
  { 1: input stream }
  f := GetStreamArg(P,1,T,MODE_READ);
  If f = Nil Then
    Exit;
  { 2: variable }
  T1 := GetPArg(2,T);
  { 3: extra variable (in_word/2, in_sentence/2) }
  T2 := GetPArg(3,T);
  { 4: what type of data to read }
  If Not GetType(4,T,What) Then
    Exit;
  { 5: skip spaces? (true/false) }
  If Not GetBoolean(5,T,SkipSpaces) Then
    Exit;
  { 6: push back all the characters read? (true/false) }
  If Not GetBoolean(6,T,LookAhead) Then
    Exit;

  { buffer in console input, when necessary }
  Stream_CheckConsoleInput(f,SkipSpaces);
  If Error Then Exit; { Ctrl-C? }

  { skip blank characters when requested; even if a lookahead is requested, 
   leading spaces are not unread (PII+ p.126 reads: "Reads all blank 
   characters, if any, and then behaves like next_char(t)") }
  If SkipSpaces Then
    ReadBlanks(f);

  { undo point  }
  Stream_NextChar(f,e);

  y := GetSyntax(P);

  { read target in R1}
  Case What Of
  TYPE_CHAR,TYPE_CODE_CHAR:
    Begin
      Stream_GetChar(f,e1);
      InOk := Not Error;
      If InOk Then
      Begin
        Case What Of
        TYPE_CHAR:
          Begin
            s := Stream_NewStr(f);
            If y = Edinburgh Then { Edinburgh: return a one-char atom }
            Begin
              If TICharIsEndOfInput(e1) Then
                Str_Append(s,'end_of_file')
              Else
                Str_AppendChar(s,e1.Val);
              R1 := EmitIdent(P,s,True,False);
              CheckCondition(R1 <> Nil,
                  'ClearIn: unable to create an identifier from a char')
            End
            Else
            Begin
              { PII: substitute EOL }
              If (y in [PrologIIv1,PrologIIv2]) And (TICharIsEol(e1)) Then
                e1.val := GetSubChar(P);
              Str_AppendChar(s,e1.Val);
              R1 := EmitConst(P,s,CS,False)
            End
          End;
        TYPE_CODE_CHAR:
          Begin
            If (y = Edinburgh) And (TICharIsEndOfInput(e1)) Then
              ss := '-1'
            Else
            Begin
              If Not TICharToCodePoint(e1,cp) Then { TODO: check }
                Exit;
              ss := CodePointToShortString(cp)
            End;
            R1 := EmitConst(P,Str_NewFromShortString(ss),CI,False)
          End
        End
      End
    End;
  TYPE_INTEGER:
    Begin
      K := ReadInteger(f,True);
      InOk := (Not Error) And (K <> Nil) And 
          NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CI));
      If InOk Then
        R1 := EmitConst(P,Token_GetStr(K),CI,False)
    End;
  TYPE_REAL:
    Begin
      K := ReadNumber(f,GetSyntax(P),True);
      InOk := (Not Error) And (K <> Nil) And 
          (Token_GetType(K) = TOKEN_REAL) And 
          NormalizeConstant(K^.TK_STRI,ObjectTypeToConstType(CR));
      If InOk Then
        R1 := EmitConst(P,Token_GetStr(K),CR,False)
    End;
  TYPE_STRING: { FIXME: PII+ p127 }
    Begin
      K := ReadString(f);
      InOk := (Not Error) And (K <> Nil);
      If InOk Then
        R1 := EmitConst(P,Token_GetStr(K),CS,False)
    End;
  TYPE_IDENT:
    Begin
      K := ReadVariableOrIdentifier(f,GetSyntax(P));
      InOk := (Not Error) And (K <> Nil) And (Token_GetType(K) = TOKEN_IDENT);
      If InOk Then
      Begin
        R1 := EmitIdent(P,Token_GetStr(K),True,False);
        CheckCondition(R1 <> Nil,'ClearIn: unable to create an identifier')
      End
    End;
  TYPE_TERM:
    Begin
      R1 := ParseOneTerm(f,P);
      InOk := Not Error
    End;
  TYPE_WORD:
    Begin
      InOk := ParseWord(f,P,R1,R2,IsTerminator) And Not Error
    End;
  TYPE_SENTENCE:
    Begin
      InOk := ParseSentence(f,P,R1,R2) And Not Error
    End
  Else
    Begin
      CWriteLnWarning('unsupported read data type');
      InOk := False
    End
  End;

  { try to bound the variables to the lists }
  Success := InOk And ReduceOneEq(T1,R1,GetDebugStream(P)) And
      ((R2 = Nil) Or ReduceOneEq(T2,R2,GetDebugStream(P)));

  { undo read when requested or in case of failure to bound }
  If InOk And (LookAhead Or Not Success) Then
    Stream_UngetChars(f,e);

  ClearIn := Success
End;

{----------------------------------------------------------------------------}
{ set on/off states: echo, trace, paper, debug                               }
{----------------------------------------------------------------------------}

{ switch a global on/off state }
Function ClearOnOffState( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  StateName : TPrologState;
  StateValue : Boolean;
Begin
  ClearOnOffState := False;
  { 1: state name }
  If Not GetOnOffStateName(1,T,StateName) Then
    Exit;
  { 2: state value: true/false }
  If Not GetBoolean(2,T,StateValue) Then
    Exit;
  { set the on/off state }
  Case StateName Of
  { echo, no_echo: 
   write on console what is read or written from disk files or buffers }
  STATE_ECHO:
    SetEcho(P,StateValue);
  { paper, no_paper:
   write in a special file what appears on screen }
  STATE_PAPER:
    SetPaper(P,StateValue);
  { trace, no_trace:
   write on console all rule calls }
  STATE_TRACE:
    SetTrace(P,StateValue);
  { debug, no_trace:
   additional unification info }
  STATE_DEBUG:
    SetDebug(P,StateValue);
  End;
  ClearOnOffState := True
End;

{----------------------------------------------------------------------------}
{ more debugging info                                                        }
{----------------------------------------------------------------------------}

{ bt }
Function ClearBacktrace( P : ProgPtr; Q : QueryPtr ) : Boolean;
Begin
  DumpBacktrace(Q);
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

{ findall(x,goal(x),l)
 1) on first call, ask back the prolog engine to:
   - store a handle to V 
   - clear the goal G in all possible manners (artificially failing after each
     success in order to trigger the next one immediately)
   - in doing so, accumulate copies of bounded V in a list (as a ListPtr of 
     TermPtr), passed back to the second call as Choices
   - call us back a second time (boolean More), after removing the goal G from
     the list of goals to clear to avoid calling G again
 2) on second call
   - convert Choices to a Prolog list and unify it with the "l" argument 
  }
Function ClearFindAll( P : ProgPtr; T : TermPtr; 
    Var V,G : TermPtr; SuccessCount : PosInt; 
    Var Choices : Pointer; Var More : Boolean ) : Boolean;
Var
  T1,T2,T3,L : TermPtr;
  Success : Boolean;
Begin
  ClearFindAll := False;
  More := False;

  CheckCondition(SuccessCount < 2, 'findall: too many calls');
  If Error Then Exit;

  { 1: variable to collect in the list (actually, any term is accepted) }
  T1 := GetPArg(1,T);
  { 2: goal (identifier or predicate) }
  T2 := GetGoal(2,T,False);
  If T2 = Nil Then
  Begin
    CWritelnWarning('findall: second argument must be a goal');
    Exit
  End;
  { 3: list }
  T3 := GetList(3,T,True);
  If T3 = Nil Then
  Begin
    CWritelnWarning('findall: third argument is not a list');
    Exit
  End;

  If SuccessCount = 0 Then { [FIND:1] first call }
  Begin
    { returned values }
    V := T1; { term (usually a variable) to collect in the list }
    G := T2; { goal that must cleared }
    Choices := Nil; { list in which to collect Vs when clearing G }
    Success := True;
    More := True { call me back }
  End
  Else
  Begin { [FIND:5] second (and last) call }
    { Choices is the list of terms V when G is cleared, in reverse order }
    L := NewEmptyList(P);
    While Choices <> Nil Do
    Begin
      L := NewList2(P,TermPtr(List_GetObject(ListPtr(Choices))),L);
      Choices := Pointer(List_GetPrev(ListPtr(Choices)))
    End;
    Success := ReduceOneEq(T3,L,GetDebugStream(P));
    More := False { I am done, bye}
  End;

  ClearFindAll := Success
End;

{ [BLOCK:1] block(T,G) }
Function ClearBlock( P : ProgPtr; T : TermPtr; Var V,G : TermPtr ) : Boolean;
Var
  T1,T2 : TermPtr;
Begin
  ClearBlock := False;
  { 1: label }
  T1 := GetPArg(1,T);
  { 2: goal (identifier or predicate) }
  T2 := GetGoal(2,T,False);
  If T2 = Nil Then
  Begin
    CWritelnWarning('block: second argument must be a goal');
    Exit
  End;
  { return V, G }
  V := T1;
  G := T2;
  ClearBlock := True
End;

{ [BLOCK:4.2] block_exit(error(1)) }
Function ClearBlockExit( P : ProgPtr; T : TermPtr; Var V : TermPtr ) : Boolean;
Var
  T1 : TermPtr;
Begin
  ClearBlockExit := False;
  { 1: label }
  T1 := GetPArg(1,T);
  { return V }
  V := T1;
  ClearBlockExit := True
End;

{ (time(V), get_time(V) }
Function ClearTime( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T1,Tr : TermPtr;
  TimeRef : TTimeType;
  What : TPrologDataType;
  V : PosInt;
  s : StrPtr;
Begin
  ClearTime := False;
  { 1: time variable (or value, but then likely to fail) }
  T1 := EvalPArg(1,T);
  { 2: time reference ('startofday' or 'epoch') }
  If Not GetTimeType(2,T,TimeRef) Then
  Begin
    CWritelnWarning('invalid reference time argument');
    Exit
  End;
  { 3: type of result ('integer' or 'real')}
  If Not GetType(3,T,What) Then
    Exit;
  If Not (What In [TYPE_INTEGER,TYPE_REAL]) Then
  Begin
    CWritelnWarning('invalid type argument');
    Exit
  End;
  { check T1 is not bound to an non numerical value }
  If IsBound(T1) And Not (((What = TYPE_INTEGER) And IsInteger(T1)) Or 
      ((What = TYPE_REAL) And IsReal(T1))) Then
  Begin
    CWritelnWarning('time argument is bounded to a value with incorrect type');
    Exit
  End;
  { compute the number of seconds since the reference time }
  Case TimeRef Of
  TIME_TYPE_TODAY: { seconds since the start of the day }
    V := SecondsSinceMidnight;
  TIME_TYPE_EPOCH: { seconds since Jan 1, 1970 }
    V := UnixTime;
  End;
  { convert to target type }
  Case What Of
  TYPE_INTEGER:
    Begin
      s := Str_NewFromShortString(LongIntToShortString(V));
      Tr := EmitConst(P,s,CI,False)
    End;
  TYPE_REAL:
    Begin
      s := Str_NewFromShortString(LongRealToShortString(V));
      Tr := EmitConst(P,s,CR,False)
    End
  End;
  ClearTime := ReduceOneEq(T1,Tr,GetDebugStream(P))
End;

{----------------------------------------------------------------------------}
{ dispatch                                                                   }
{----------------------------------------------------------------------------}

{ clear a predefined predicate syscall(Code,Arg1,...ArgN), meaning 
 Code(Arg1,...,ArgN), except insert; G returns the new goal to freeze or clear }
Function ClearPredef( Predef : TPP; P : ProgPtr; Q : QueryPtr; 
    T : TermPtr; Var V,G : TermPtr; 
    Var L : RestPtr; SuccessCount : PosInt; Var Choices : Pointer; 
    Var More : Boolean ) : Boolean;
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
  PP_SUBSTRING:
    Ok := ClearSubstring(P,T);
  PP_FIND_PATTERN:
    Ok := ClearFindPattern(P,T);
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
  PP_STRING_IDENT:
    Ok := ClearStringIdent(P,T);
  PP_LIST_STRING:
    Ok := ClearListString(P,T);
  PP_LIST_TUPLE:
    Ok := ClearListTuple(P,T);
  PP_SPLIT:
    Ok := ClearSplit(P,T);
  PP_ATOM_CHARS:
    Ok := ClearAtomChars(P,T);
  PP_ATOM_LENGTH:
    Ok := ClearAtomLength(P,T);
  PP_NUMBER_CHARS:
    Ok := ClearNumberChars(P,T);
  PP_ARG:
    Ok := ClearArg(P,T);
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
  PP_SUBWORLDS:
    Ok := ClearSubWorlds(P,T);
  PP_FIND_RULE:
    Ok := ClearFindRule(P,T);
  PP_ASSERT1:
    Ok := ClearAssert1(P,T);
  PP_ASSERT2:
    Ok := ClearAssert2(P,T);
  PP_SUPPRESS:
    Ok := ClearSuppress(P,T);
  PP_RULE:
    Ok := ClearRule(P,T,True,False,L,SuccessCount,Choices,More);
  PP_RETRACT1:
    Ok := ClearRule(P,T,False,True,L,SuccessCount,Choices,More);
  PP_RETRACT2:
    Ok := ClearRule(P,T,True,True,L,SuccessCount,Choices,More);
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
  PP_GOTOXY:
    Ok := ClearGotoXY(P,T);
  PP_GET_LINE_WIDTH:
    Ok := ClearGetLineWidth(P,T);
  PP_SET_LINE_WIDTH:
    Ok := ClearSetLineWidth(P,T);
  PP_SET_LINE_CURSOR:
    Ok := ClearSetLineCursor(P,T);
  PP_CLRSRC:
    Ok := ClearClrScr(P);
  PP_IN:
    Ok := ClearIn(P,T);
  PP_SUB_EOL:
    Ok := ClearSubEOL(P,T);
  PP_SET_STATE:
    Ok := ClearOnOffState(P,T);
  PP_BACKTRACE:
    Ok := ClearBacktrace(P,Q);
  PP_DUMP:
    Ok := ClearDump(P);
  PP_FREEZE:
    Ok := ClearFreeze(P,T,V,G);
  PP_FIND_ALL:
    Ok := ClearFindAll(P,T,V,G,SuccessCount,Choices,More);
  PP_BLOCK:
    Ok := ClearBlock(P,T,V,G);
  PP_BLOCK_EXIT:
    Ok := ClearBlockExit(P,T,V);
  PP_TIME:
    Ok := ClearTime(P,T);
  PP_FAIL:
    Ok := False
  End;

  ClearPredef := Ok
End;

End.