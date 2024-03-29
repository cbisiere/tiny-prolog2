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
  OStream,
  OStack,
  IStream,
  IStack,
  Memory,
  PObj,
  PObjOp,
  PObjStr,
  PObjDict,
  PObjTerm,
  PObjProg,
  Encoding,
  Unparse,
  Reduc,
  Expr,
  Parse,
  Debug;

Type
  TPP = (
    PP_ASSERTA,
    PP_ASSERTZ,
    PP_EXPAND_FILENAME,
    PP_INPUT_IS,
    PP_OPEN,
    PP_CLOSE_CURRENT_INPUT,
    PP_CLOSE_INPUT,
    PP_CLEAR_INPUT,
    PP_IN_TERM,
    PP_IN_CHAR,
    PP_OUTPUT_IS,
    PP_CLOSE_CURRENT_OUTPUT,
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
  NB_PP = 28;
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
    (I:PP_ASSERTA;S:'sysasserta';N:1),
    (I:PP_ASSERTZ;S:'sysassertz';N:1),
    (I:PP_EXPAND_FILENAME;S:'sysexpandfilename';N:2),
    (I:PP_INPUT_IS;S:'sysinputis';N:1),
    (I:PP_OPEN;S:'sysopen';N:4),
    (I:PP_CLOSE_CURRENT_INPUT;S:'sysclosecurrentinput';N:0),
    (I:PP_CLOSE_INPUT;S:'syscloseinput';N:1),
    (I:PP_CLEAR_INPUT;S:'sysclearinput';N:0),
    (I:PP_IN_TERM;S:'sysinterm';N:2),
    (I:PP_IN_CHAR;S:'sysinchar';N:2),
    (I:PP_OUTPUT_IS;S:'sysoutputis';N:1),
    (I:PP_CLOSE_CURRENT_OUTPUT;S:'sysclosecurrentoutput';N:0),
    (I:PP_CLOSE_OUTPUT;S:'syscloseoutput';N:1),
    (I:PP_FLUSH;S:'sysflush';N:0),
    (I:PP_QUIT;S:'sysquit';N:0),
    (I:PP_INSERT;S:'sysinsert';N:1),
    (I:PP_LIST;S:'syslist';N:0),
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
  I := InstallIdentifier(P^.PP_DCON,NewStringFrom(SYSCALL_IDENT_AS_STRING),True)
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
  str := StrGetFirstData(s);
  If StrLength(s) > StringMaxSize Then
  Begin
    CWriteWarning('string too long: ');
    CWrite(str);
    CWrite('...');
    CWriteLn;
    Exit
  End;
  GetAtomArgAsShortStr := True
End;

{ return True if argument n of a predicate can be filename  }
Function GetFilenameArgAsString( n : Byte; T : TermPtr; 
    Var str : TString ) : Boolean;
Begin
  GetFilenameArgAsString := GetAtomArgAsShortStr(n,T,False,str)
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

{ get an input stream from argument n, or Nil }
Function GetIStreamArg( n : Byte; T : TermPtr ) : TIStreamPtr;
Var
  f : TIStreamPtr;
  FileAlias : TAlias;
  FileDesc : TFileDescriptor;
Begin
  f := Nil;
  If GetPosIntArg(n,T,FileDesc) Then { case 1: file descriptor }
    f := GetIStreamFromFileDescriptor(FileDesc)
  Else { case 2: alias; leave quotes if any }
    If GetAtomArgAsShortStr(n,T,True,FileAlias) Then 
      f := GetIStreamFromFileAlias(FileAlias);
  GetIStreamArg := f
End;

{ get an output stream from argument n, or Nil }
Function GetOStreamArg( n : Byte; T : TermPtr ) : TOStreamPtr;
Var
  f : TOStreamPtr;
  FileAlias : TAlias;
  FileDesc : TFileDescriptor;
Begin
  f := Nil;
  If GetPosIntArg(n,T,FileDesc) Then { case 1: file descriptor }
    f := GetOStreamFromFileDescriptor(FileDesc)
  Else { case 2: alias; leave quotes if any }
    If GetAtomArgAsShortStr(n,T,True,FileAlias) Then 
      f := GetOStreamFromFileAlias(FileAlias);
  GetOStreamArg := f
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
  CheckCondition(StrEqualTo(SysCallCode,SYSCALL_IDENT_AS_STRING),
      'PredefCallIsOk: Not a syscall');

  { there are at least two arguments: 'syscall' and the identifier }
  NbArgs := TupleArgCount(T);
  If NbArgs < 2 Then 
    Exit;  

  { get the identifier }
  T2 := TupleArgN(2,T);
  Ident := IdentifierGetStr(IdPtr(T2));

  { predicate identifier is not too long }
  If StrLength(Ident) > MAX_PP_LENGHT Then
    Exit;

  { predicate is known }
  str := StrGetString(Ident);
  If Not LookupPP(str,rec) Then
    Exit;

  { predicate has the correct number of parameters }
  If NbArgs - 2 <> rec.N Then
    Exit;

  Predef := rec.I;
  PredefCallIsOk := True
End;


{----------------------------------------------------------------------------}
{ clear predefined predicates (except insert)                                }
{----------------------------------------------------------------------------}

{ asserta(T), assertz(T); TODO: handle rules (we handle facts, for now) }
Function ClearAssert( P : ProgPtr; T : TermPtr; first : Boolean ) : Boolean;
Var
  T1 : TermPtr;
  Ih : IdPtr;
  R,Ri : RulePtr;
Begin
  ClearAssert := False;
  { 1: the fact (ident or predicate) }
  T1 := GetPArg(1,T);

  { deep copy with eval }
  T1 := CopyTerm(T1);

  { get the head }
  Ih := AccessIdentifier(T1);
  If Ih = Nil Then
    Exit;

  { create the rule from the term }
  R := NewRule(RTYPE_USER,GetSyntax(P));
  R^.RU_FBTR := NewBTerm(T1);

  { insert the rule }
  Ri := FirstRuleWithHead(P,Ih);
  If Ri = Nil Then { no rules yet; insert at the end }
    AppendRules(P,R)
  Else If first Then { asserta }
    R := InsertRulesB(P,Ri,R)
  Else { assertz }
  Begin
    Ri := LastRuleWithHead(P,Ih);
    R := InsertRulesA(P,Ri,R)
  End;

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
      DictSetGlobal(IdPtr(T1)^.TV_DVAR,True); { dict entry is now persistent }
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
  If Not GetFilenameArgAsString(1,T,Pattern) Then 
    Exit;
  Pattern := OSFilename(Pattern); { handle ~ }
  Path := ExtractPath(Pattern); { path part, with training sep }
  L := NewEmptyList(P);
  FindFirst(Pattern, AnyFile, DirInfo);
  While DosError = 0 do
  Begin
    s := NewStringFrom(Path);
    StrAppend(s,DirInfo.Name);
    If GetSyntax(P) = Edinburgh Then { Edinburgh uses quoted ident }
    Begin
      StrQuote(s);
      T1 := EmitIdent(P,s,False)
    End
    Else
      T1 := EmitConst(P,s,CS,False);
    L := NewList2(P,T1,L);
    FindNext(DirInfo)
  End;
  ClearExpandFileName := ReduceOneEq(L,GetPArg(2,T))
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
  o := OpLookup(P^.PP_OPER,'',Id4,[],TOpTypeToArity(ot),1200);
  If o <> Nil Then { TODO: do not fail when both declarations match }
    Exit;
  { register the new operator }
  o := OpAppend(P^.PP_OPER,Id3,Id4,ot,v);
  ClearOp := True
End;

{ quit }
Function ClearQuit : Boolean;
Begin 
  SetQuitOn(0);
  ClearQuit := True
End;

{ close_input("buffer") }
Function ClearCloseInput( T : TermPtr ) : Boolean;
Var
  f : TIStreamPtr;
Begin
  ClearCloseInput := False;
  f := GetIStreamArg(1,T);
  If f = Nil Then
    Exit;
  CloseInputByFileDescriptor(GetIStreamDescriptor(f));
  ClearCloseInput := True
End;

{ close_output("buffer") }
Function ClearCloseOutput( T : TermPtr ) : Boolean;
Var
  f : TOStreamPtr;
Begin
  ClearCloseOutput := False;
  f := GetOStreamArg(1,T);
  If f = Nil Then
    Exit;
  CloseOutputByFileDescriptor(GetOStreamDescriptor(f));
  ClearCloseOutput := True
End;

{ sysopen(F,Mode,Fd,Opt) to implement:
 - open("buffer.txt",read/write,Fd,[alias(name)])
 - input/output("buffer") 
 see 
 https://www.swi-prolog.org/pldoc/man?predicate=open/4 }
Function ClearSetStream( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  FileName : TPath;
  FileAlias : TAlias;
  FileDesc : TFileDescriptor;
  T3,T4,Td,Ta : TermPtr;
  P1,P2 : TermPtr;
  I2,I : IdPtr;
  Mode : TString;
  Ok : Boolean;
Begin
  ClearSetStream := False;
  
  { 1: file name }
  If Not GetFilenameArgAsString(1,T,FileName) Then 
    Exit;
  { 2: mode (read/write) }
  I2 := EvalPArgAsIdent(2,T);
  If I2 = Nil Then
    Exit;
  Mode := IdentifierGetPStr(I2);
  If (Mode <> 'read') And (Mode <> 'write') Then { TODO: warning }
  Begin
    CWriteWarning('unsupported file mode: ''');
    CWrite(Mode);
    CWrite('''');
    CWriteLn;
    Exit
  End;
  { 3: file descriptor; must be a free variable }
  T3 := GetPArg(3,T);
  If Not IsFree(T3) Then
    Exit;
  { 4: list of options; only [alias(ident)] is supported }
  FileAlias := FileName; { default }
  T4 := EvalPArg(4,T);
  If Not IsNil(T4) Then { [] }
  Begin
    If Not GetList(T4,P1,P2,True) Then
      Exit;
    If Not IsNil(P2) Then { only one option is allowed }
      Exit;
    If Not GetFunc1(P1,'alias',Ta,True) Then
    Begin
      CWriteWarning('''alias'' is the only supported option');
      CWriteLn;
      Exit
    End;
    I := EvaluateToIdentifier(Ta);
    If I = Nil Then
      Exit;
    FileAlias := IdentifierGetPStr(IdPtr(Ta))
  End;

  { ok, we have Filename, Mode, Td (free var for file descriptor), FileAlias }
  If Mode = 'read' Then
    Ok := SetFileForInput(FileDesc,FileAlias,FileName,True) <> Nil
  Else
    Ok := SetFileForOutput(FileDesc,FileAlias,FileName,True) <> Nil;
  
  If Ok Then
  Begin
    { bind the free variable with the file descriptor }
    Td := EmitConst(P,NewStringFrom(PosIntToStr(FileDesc)),CI,True);
    Ok := ReduceOneEq(T3,Td);
    CheckCondition(Ok,'failed to bind a variable to a file descriptor')
  End;
  { close the file if it was already open in the other mode }
  If Mode = 'read' Then
    CloseOutputByFileName(FileName)
  Else
    CloseInputByFileName(FileName);
  ClearSetStream := Ok
End;

{ input/output_is(s) }
Function ClearStreamIs( P : ProgPtr; T : TermPtr; input : Boolean ) : Boolean;
Var
  T1 : TermPtr;
  FileAlias : TAlias; 
Begin
  If input Then
    FileAlias := InputIs
  Else
    FileAlias := OutputIs;
  T1 := EmitConst(P,NewStringFrom(FileAlias),CS,False);
  ClearStreamIs := ReduceOneEq(GetPArg(1,T),T1)
End;

{ close_input/output }
Function ClearCloseCurrentStream( input : Boolean ) : Boolean;
Begin
  If input Then
    CloseCurrentInput
  Else
    CloseCurrentOutput;
  ClearCloseCurrentStream := True
End;

{ clear_input }
Function ClearClearInput : Boolean;
Begin
  DoClearInput;
  ClearClearInput := True
End;

{ flush }
Function ClearFlush : Boolean;
Begin
  FlushCurrentOutput;
  ClearFlush := True
End;

{ list }
Function ClearList( Q : QueryPtr ) : Boolean;
Begin
  OutRuleRange(FirstRuleInQueryScope(Q),LastRuleInQueryScope(Q),
      RTYPE_USER,False);
  ClearList := True
End;

{ out("hello") }
Function ClearOut( P : ProgPtr; T : TermPtr ) : Boolean;
Begin
  OutTerm(GetSyntax(P),GetPArg(1,T),True);
  ClearOut := True
End;

{ outm("hello"), or put_char('a'); TODO: put_char(Stream,Char) }
Function ClearOutm( P : ProgPtr; T : TermPtr ) : Boolean;
Begin
  OutTermBis(GetSyntax(P),GetPArg(1,T),False,False,True);
  ClearOutm := True
End;

{ line }
Function ClearLine : Boolean;
Begin
  OutCR(True);
  ClearLine := True
End;

{ clear }
Function ClearClrScr : Boolean;
Begin
  If OutputIsTerminal Then
    CrtClrSrc;
  ClearClrScr := True
End;

{ read_term(Stream,T) }
Function ClearInTerm( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  f : TIStreamPtr;
  T2,Tr : TermPtr;
Begin
  ClearInTerm := False;
  { 1: stream }
  f := GetIStreamArg(1,T);
  If f = Nil Then
    Exit;
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
Function ClearInChar( P : ProgPtr; T : TermPtr ) : Boolean;
Var
  T2,Tc : TermPtr;
  c : TChar;
  f : TIStreamPtr;
Begin
  ClearInChar := False;
  { 1: stream }
  f := GetIStreamArg(1,T);
  If f = Nil Then
    Exit;
  { 2: char variable }
  T2 := GetPArg(2,T);

  { buffer in console input, when necessary; do not skip spaces }
  CheckConsoleInputStream(f,False);
  { get the char }
  c := GetCharFromStream(f,c);
  If Error Then
    Exit;
  Tc := EmitConst(P,NewStringFrom(c),CS,False);
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
  PP_ASSERTA:
    Ok := ClearAssert(P,T,True);
  PP_ASSERTZ:
    Ok := ClearAssert(P,T,False);
  PP_EXPAND_FILENAME:
    Ok := ClearExpandFileName(P,T);
  PP_OPEN:
    Ok := ClearSetStream(P,T);
  PP_INPUT_IS:
    Ok := ClearStreamIs(P,T,True);
  PP_CLOSE_CURRENT_INPUT:
    Ok := ClearCloseCurrentStream(True);
  PP_CLOSE_INPUT: 
    Ok := ClearCloseInput(T);
  PP_CLEAR_INPUT: 
    Ok := ClearClearInput;
  PP_OUTPUT_IS: 
    Ok := ClearStreamIs(P,T,False);
  PP_CLOSE_CURRENT_OUTPUT:
    Ok := ClearCloseCurrentStream(False);
  PP_CLOSE_OUTPUT:
    Ok := ClearCloseOutput(T);
  PP_FLUSH:
    Ok := ClearFlush;
  PP_LIST:
    Ok := ClearList(Q);
  PP_OUT:
    Ok := ClearOut(P,T);
  PP_OUTM:
    Ok := ClearOutm(P,T);
  PP_LINE:
    Ok := ClearLine;
  PP_CLRSRC:
    Ok := ClearClrScr;
  PP_IN_TERM:
    Ok := ClearInTerm(P,T);
  PP_IN_CHAR:
    Ok := ClearInChar(P,T);
  PP_BACKTRACE:
    Ok := ClearBacktrace;
  PP_DUMP:
    Ok := ClearDump
  End;
    
  ClearPredef := Ok
End;

End.