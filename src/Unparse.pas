{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022-2026                                                  }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              U N P A R S I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}
{$I define.inc }

Unit Unparse;

Interface

Uses
  Chars,
  Num,
  Serial,
  ShortStr,
  Memory,
  Errs,
  CWrites,
  Common,
  PObj,
  PObjLisA,
  PObjList,
  PObjTerm,
  PObjFCVI,
  PObjIO,
  PObjRest,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjSys,
  PObjDef,
  PObjBter,
  PObjComm,
  PObjRule,
  PObjQury,
  PObjProg,
  PObjOp,
  Tuple,
  Encoding;

{ to long strings }
Function ConstToLongString( enc : TEncoding; P : ProgPtr; 
    C : ConstPtr ) : StrPtr;
Function IdentifierToLongString( enc : TEncoding; P : ProgPtr; 
    I : IdPtr ) : StrPtr;
Function VarNameToLongString( enc : TEncoding; P : ProgPtr; 
    V : VarPtr ) : StrPtr;
Function TermToLongString( enc : TEncoding; P : ProgPtr; 
    T : TermPtr ) : StrPtr;
Function TermUnquotedToLongString( enc : TEncoding; P : ProgPtr; 
    T : TermPtr ) : StrPtr;
Function OneEquationToLongString( enc : TEncoding; P : ProgPtr; 
    E : EqPtr ) : StrPtr;
Function QuerySolutionToLongString( enc : TEncoding; P : ProgPtr; 
    Q : QueryPtr ) : StrPtr;
Function OneRuleToLongString( enc : TEncoding; P : ProgPtr; 
    R : RulePtr ) : StrPtr;
Function OneQueryToLongString( enc : TEncoding; P : ProgPtr; 
    Q : QueryPtr ) : StrPtr;
Function OneCommentToLongString( enc : TEncoding; P : ProgPtr; 
    C : CommPtr ) : StrPtr;


{ write, ignoring the line width system }
Procedure PutConst( f : StreamPtr; P : ProgPtr; C : ConstPtr );
Procedure PutIdentifier( f : StreamPtr; P : ProgPtr; I : IdPtr );
Procedure PutVarName( f : StreamPtr; P : ProgPtr; V : VarPtr );
Procedure PutTerm( f : StreamPtr; P : ProgPtr; T : TermPtr );
Procedure PutTermUnquoted( f : StreamPtr; P : ProgPtr; T : TermPtr );
Procedure PutOneEquation( f : StreamPtr; P : ProgPtr; E : EqPtr );
Procedure PutQuerySolution( f : StreamPtr; P : ProgPtr; Q : QueryPtr );
Procedure PutOneRule( f : StreamPtr; P : ProgPtr; R : RulePtr );
Procedure PutOneQuery( f : StreamPtr; P : ProgPtr; Q : QueryPtr );
Procedure PutOneComment( f : StreamPtr; P : ProgPtr; C : CommPtr );
Procedure PutTraceMessage( P : ProgPtr; Tag : TString; Depth : PosInt; 
    Branch : PosInt; ClearT : TermPtr );

{ write, using the line width system }
Procedure OutTerm( f : StreamPtr; P : ProgPtr; T : TermPtr );
Procedure OutTermUnquoted( f : StreamPtr; P : ProgPtr; T : TermPtr );

{ debug }
Procedure DumpSystemFromDict( DV : DictPtr );

Implementation
{-----------------------------------------------------------------------------}

{ per-syntax output elements }
Type
  TOSyntaxElement = Array[TSyntax] Of Record
    RuleArrow: String[3]; { rule arrow (no condition) }
    GoalArrow: String[3]; { rule arrow when goals are present }
    RuleEnd: Char;
    QueryStart: String[2];
    QueryEnd: Char
  End;
Const
  OSyntax : TOSyntaxElement = (
    (RuleArrow:'->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:'->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:'->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:'';GoalArrow:':-';RuleEnd:'.';QueryStart:':-';QueryEnd:'.')
  );

Var
  CC_LINE_CONTINUATION : TChar; { backslash }

{----------------------------------------------------------------------------}
{                                                                            }
{  Preparation of reduced systems and terms before display                   }
{                                                                            }
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
{ represent a reduced system on a set of variables DV with a system object   }
{----------------------------------------------------------------------------}

{ add to system S all the equations or inequations reachable from term T,
 avoiding loop; we take great care of scanning all the terms linked to T; 
 loop avoidance should also prevent any duplicates in S }
Procedure CollectEquationsFromTerm( T : TermPtr; S : SysPtr; g : TSerial );
Var
  Tr,Tf,T1,T2 : TermPtr;
  Ts : TermsPtr;
  E : EqPtr;
Begin
  If Term_GetSeen(T,1,g) Then
    Exit;
  Term_SetSeen(T,1,g);

  { V = T; also applies to assigned identifiers; it would be abnormal to have
   a F as LHS, as the clock undo those equations; we still collect them so it
   will show that something went wrong }
  Tr := Red(T);
  If Tr <> Nil Then
  Begin
    Sys_InsertOneEq(S,Eq_New(REL_EQUA,T,Tr));
    CollectEquationsFromTerm(Tr,S,g)
  End;

  Case TypeOfTerm(T) Of
  Variable:
    Begin
      { V is frozen and watches frozen terms }
      Ts := GetFrozenTerms(VarPtr(T));
      While Ts <> Nil Do
      Begin
        Tf := TermPtr(List_GetObject(Ts));
        If IsFree(T) Then
          Sys_InsertOneEq(S,Eq_New(REL_FROZ,T,Tf));
        CollectEquationsFromTerm(Tf,S,g);
        Ts := List_GetNext(Ts)
      End;
      { V watches inequations T1 # T2, T3 # T4, ... }
      E := WatchIneq(VarPtr(T));
      While E <> Nil Do
      Begin
        Sys_CopyOneEq(S,E);
        CollectEquationsFromTerm(Eq_GetLhs(E),S,g);
        CollectEquationsFromTerm(Eq_GetRhs(E),S,g);
        E := Eqs_GetNext(E)
      End
    End;
  FuncSymbol:
    Begin
      T1 := Func_GetLeft(FuncPtr(T));
      T2 := Func_GetRight(FuncPtr(T));
      If T1 <> Nil Then
        CollectEquationsFromTerm(T1,S,g);
      If T2 <> Nil Then
        CollectEquationsFromTerm(T2,S,g)
    End
  End
End;

{ return a system object representing a solution on a set of variables DV, 
 avoiding loops }
Function SolutionAsSystem( DV : DictPtr ) : SysPtr;
Var
  S : SysPtr;
  g : TSerial;
  T : TermPtr;
Begin
  S := Sys_New;
  g := NewSerial;
  { collect all equations and inequations }
  While DV <> Nil Do
  Begin
    T := Dict_GetTerm(DV);
    { collect equations attached to the variable, skipping anonymous variable }
    If Not IsAnonymousVariable(T) Then
      CollectEquationsFromTerm(T,S,g);
    DV := Dict_GetNext(DV)
  End;
  SolutionAsSystem := S
End;

Procedure DumpSystemFromDict( DV : DictPtr );
Var
  S : SysPtr;
Begin
  S := SolutionAsSystem(DV);
  Sys_Dump(S)
End;

{----------------------------------------------------------------------------}
{ build an inventory of call sites for all variables in a system             }
{----------------------------------------------------------------------------}

{ build inventory for field T or object Obj; if T is a variable, append 
 T's address to the list of call sites for variable T, otherwise keep scanning 
 for variables }
Procedure CallSiteInventory( Obj : TObjectPtr; Var T : TermPtr; 
    k : TSeenIndex; g : TSerial );
Var
  A1,A2 : TermPtrAddr;
  Seen : Boolean;
Begin
  Seen := Term_GetSeen(T,k,g);
  Term_SetSeen(T,k,g);

  { consider only variables and non-marked terms }
  If Not IsVariable(T) And Seen Then
    Exit;

  { unseen term: reset its list of call sites }
  If Not Seen Then
    Term_SetListA(T,Nil);

  { loop-breaker: check the call site has not been registered yet }
  If ListA_Lookup(Term_GetListA(T),Obj,Addr(T)) <> Nil Then
    Exit;

  { then, register it }
  Term_InsertOneCallSite(T,ListA_New(Obj,Addr(T)));

  { functional symbol: continue the inventory going through both legs }
  If TypeOfTerm(T) = FuncSymbol Then
  Begin
    A1 := Func_GetLeftAddr(FuncPtr(T));
    A2 := Func_GetRightAddr(FuncPtr(T));
    If A1^ <> Nil Then
      CallSiteInventory(TObjectPtr(T),A1^,k,g);
    If A2^ <> Nil Then
      CallSiteInventory(TObjectPtr(T),A2^,k,g)
  End
End;

{ for each variable reachable from a list of equations or inequations, build 
 the list of call sites; note that the equations LHS and RHS are themselves 
 potential call sites }
Procedure CallSiteInventories( E : EqPtr; k : TSeenIndex; g : TSerial );
Var
  A1,A2 : TermPtrAddr;
Begin
  While E <> Nil Do
  Begin
    A1 := Eq_GetLhsAddr(E);
    A2 := Eq_GetRhsAddr(E);
    CallSiteInventory(TObjectPtr(E),A1^,k,g);
    CallSiteInventory(TObjectPtr(E),A2^,k,g);
    E := Eqs_GetNext(E)
  End
End;

{ for each variable reachable from a system, build the list of call sites;
 is allowed to use seen index k }
Procedure BuildCallSiteInventoriesInSystem( S : SysPtr; k : TSeenIndex );
Var
  g : TSerial;
  t : EqType;
Begin
  { use the same serial number for this task as no term should be processed 
   more than once }
  g := NewSerial;
  For t := REL_EQUA To REL_FROZ Do
    CallSiteInventories(Sys_Get(S,t),k,g)
End;

{ replace all call sites of term T with term T2; return True if at least one
 replacement has been actually done }
Function ReplaceCallSites( T,T2 : termPtr; Var L : RestPtr ) : Boolean;
Var
  OneDone : Boolean;
  La : ListAPtr;
  A : TermPtrAddr;
Begin
  OneDone := False;
  La := Term_GetListA(T);

  { T2 inherits from T all its call sites }
  Term_AppendCallSitesWithUndo(T2,La,L,True);
  Term_SetListAWithUndo(T,Nil,L,True);

  While La <> Nil Do
  Begin
    A := TermPtrAddr(ListA_GetAddr(La));
    If Not Term_SameAs(A^,T2) Then { extra security, but should not happen }
    Begin
      Rest_SetMem(L,ListA_GetObject(La),TObjectPtr(A^),TObjectPtr(T2),True);
      OneDone := True
    End;
    La := ListA_GetNext(La)
  End;
  ReplaceCallSites := OneDone
End;

{----------------------------------------------------------------------------}
{ simplify a system                                                          }
{----------------------------------------------------------------------------}

{ return True if term T is a temporary variable, given that query variables 
 have been marked with serial (k,g) }
Function IsTempVar( T : TermPtr; k : TSeenIndex; g : TSerial ) : Boolean;
Begin
  IsTempVar := IsVariable(T) And Not Term_GetSeen(T,k,g)
End;

{ return True if term T is a query( that is, non-temporary variable, given that 
 query variables have been marked with serial (k,g) }
Function IsQueryVar( T : TermPtr; k : TSeenIndex; g : TSerial ) : Boolean;
Begin
  IsQueryVar := IsVariable(T) And Term_GetSeen(T,k,g)
End;

{ mark all variables in a dictionary with a serial number (k,g) }
Procedure Mark( DV : DictPtr; k : TSeenIndex; g : TSerial );
Begin
  While DV <> Nil Do
  Begin
    Term_SetSeen(Dict_GetTerm(DV),k,g);
    DV := Dict_GetNext(DV)
  End
End;

{ simplify a system by removing temporary variables, through repeated 
 replacements; query (that is, non-temporary) variables are marked with 
 serial number (k,g); to simplify a system we apply two successive operations: 
 1) x_i = term ==> replace x_i with term
 2) x = x_i ==> replace x_i with x 
 Each operation is applied as many times as possible }
Procedure SimplifySystem( S : SysPtr; k : TSeenIndex; g : TSerial; 
    Var L : RestPtr );
Var
  Done : Boolean;
  E : EqPtr;
  T1,T2 : TermPtr;
  Phase : 1..2;
Begin
  { update call sites inventory for all variables in the system }
  BuildCallSiteInventoriesInSystem(S,k+1);
  { repeatedly scan the equations, eliminating temporary variables }
  For Phase := 1 to 2 Do
  Begin
    Done := False;
    While Not Done Do
    Begin
      Done := True;
      E := Sys_Get(S,REL_EQUA);
      While E <> Nil Do
      Begin
        If Not Eq_IsTrivial(E) Then
        Begin
          T1 := Eq_GetLhs(E);
          T2 := Eq_GetRhs(E);
          Case Phase Of
          1: If IsTempVar(T1,k,g) Then
              If ReplaceCallSites(T1,T2,L) Then
                Done := False;
          2: If IsQueryVar(T1,k,g) And IsTempVar(T2,k,g) Then 
              If ReplaceCallSites(T2,T1,L) Then
                Done := False
          End
        End;
        E := Eqs_GetNext(E)
      End
    End
  End
End;

{ get a simplified reduced system for a list of variables DV; return a list
 of equations and inequations to display }
Function GetSimplifiedSolution( DV : DictPtr; Var L : RestPtr ) : EqPtr;
Var
  S : SysPtr;
  k : TSeenIndex; 
  g : TSerial;
Begin
  S := SolutionAsSystem(DV);

  { marking query variables with (k,g) }
  k := 1;
  g := NewSerial;
  Mark(DV,k,g);

  SimplifySystem(S,k,g,L);

  GetSimplifiedSolution := Sys_AsList(S)
End;

{----------------------------------------------------------------------------}
{ naming of temporary variables                                              }
{----------------------------------------------------------------------------}

(*) PII+ 4.7 behavior;
  > assign(term,hello(x));
  {}
  >val(term,v);
  {v=hello(v305)}
  >val(term,v305);
  {v=hello(v306)}
*)
{ return the name of a user or temporary variable }
Function GetVarNameAsStr( V : VarPtr ) : StrPtr;
Var 
  TV : TermPtr Absolute V;
  PV : TObjectPtr Absolute V;
  s : StrPtr;
  k : Integer;
  guid : TObjectID;
Begin
  CheckCondition(TypeOfTerm(TV) = Variable,
    'GetVarNameAsStr(V): V is not a variable');
  s := Str_Clone(VariableGetName(V));
  k := ObjectCopyNumber(PV);
  If k > 0 Then
  Begin
    { temporary variable: its displayed name must be an invalid variable name }
    guid := ObjectGuid(PV);
    Str_Append(s, '@' + LongIntToShortString(guid))
  End;
  GetVarNameAsStr := s
End;

{----------------------------------------------------------------------------}
{ formatter                                                                  }
{----------------------------------------------------------------------------}

{ to format a Prolog object properly, we first create a long string with soft
 marks: 
 - atom begin (with length as payload) (extended to arrows)
 - top-level term begin and end (to generate left margin in PII when 
   breaking a term) }

{-----------------------------------------------------------------------}
{ output a Prolog objects represented by a string containing soft marks }
{-----------------------------------------------------------------------}

{ output on a stream a string containing soft marks (e.g. line breaks), 
 and also applying Prolog's formatting rules regarding line width; current 
 cursor position is updated }
Procedure OutFormatted( f : StreamPtr; P : ProgPtr; sf : StrPtr );
Var
  y : TSyntax;
  Iter : StrIter;
  cc : TChar;
  InTopTerm, InAtom : Boolean;
  ToAtomEnd : TSoftMarkPayload; { number of within-atom chars left }
Begin
  y := GetSyntax(P);
  { initialize state }
  InTopTerm := False;
  InAtom := False;
  ToAtomEnd := 0;
  { loop over all characters or soft marks }
  StrIter_ToStart(Iter,sf);
  While StrIter_NextChar(Iter,cc) Do
  Begin
    If TCharIsSoftMark(cc) Then
    Begin
      Case TCharGetSoftMarkCode(cc) Of
      SOFT_MARK_END_OF_LINE:
        Begin
          Stream_OutNewLine(f)
        End;
      SOFT_MARK_ATOM_BEGIN:
        Begin
          CheckCondition(Not InAtom,'Formatter: unclosed ATOM');
          InAtom := True;
          ToAtomEnd := TCharGetSoftMarkPayload(cc);
          { PII: *try* not to break atoms }
          If (y In [PrologIIv1,PrologIIv2]) And 
              (ToAtomEnd > Stream_RemainingSpacesInLine(f)) Then 
            Stream_OutNewLine(f)
        End;
      SOFT_MARK_TOP_TERM_BEGIN:
        Begin
          CheckCondition(Not InTopTerm,'Formatter: unclosed TERM');
          InTopTerm := True
        End;
      SOFT_MARK_TOP_TERM_END:
        Begin
          CheckCondition(InTopTerm,'Formatter: unopened TERM');
          InTopTerm := False
        End;
      End
    End
    Else { regular char (possibly a line break)}
    Begin
      If InAtom Then
      Begin
        If (y In [PrologIIv1,PrologIIv2]) Then
        Begin
          If Stream_RemainingSpacesInLine(f) > 0 Then { else: truncation }
            Stream_OutChar(f,cc)
        End
        Else
        Begin
          { continuation; note that the number of spaces left might be zero if 
           set_line_width/1 is shortened to a value lower than the length of 
           the current line is }
          If (Stream_RemainingSpacesInLine(f) <= 1) Then
          Begin
            Stream_OutChar(f,CC_LINE_CONTINUATION);
            Stream_OutNewLine(f)
          End;
          Stream_OutChar(f,cc);
        End;
        { ok, one more atom char has been treated }
        ToAtomEnd := ToAtomEnd - 1;
        If ToAtomEnd = 0 Then
          InAtom := False
      End
      Else
      Begin
        If Stream_RemainingSpacesInLine(f) = 0 Then { line is full }
        Begin
          Stream_OutNewLine(f);
          If (y In [PrologIIv1,PrologIIv2]) And InTopTerm Then { PII left margin }
            Stream_OutNChar(f,CC_BLANK_SPACE,3);
        End;
        Stream_OutChar(f,cc)
      End
    End
  End
End;

{-----------------------------------------------------------------------}
{ Unparse Prolog objects: text                                          }
{-----------------------------------------------------------------------}

{ write Ascii characters (passed in a short string) }
Procedure UnparseShortString( s : StrPtr; v : TString );
Begin
  Str_Append(s,v)
End;

{ write a string (passed in a long string) }
Procedure UnparseString( s : StrPtr; s2 : StrPtr );
Begin
  Str_Concat(s,s2)
End;

{ write a line break (soft mark) }
Procedure UnparseLineBreak( s : StrPtr );
Begin
  Str_AppendLineBreak(s)
End;

{-----------------------------------------------------------------------}
{ Unparse Prolog objects: atoms                                         }
{-----------------------------------------------------------------------}

{ write an atom, preceded by a soft mark }
Procedure UnparseAtom( s : StrPtr; s2 : StrPtr );
Var
  cc : TChar;
Begin
  TCharSetSoftMark(cc,SOFT_MARK_ATOM_BEGIN,Str_Length(s2));
  Str_AppendChar(s,cc);
  UnparseString(s,s2)
End;

{ write an atom passed as a short, applying Prolog's formatting rules }
Procedure UnparseAtomFromShortString( s : StrPtr; v : TString );
Begin
  UnparseAtom(s,Str_NewFromShortString(v))
End;

{ write the name of a user or temporary variable }
Procedure UnparseVarName( s : StrPtr; V : VarPtr );
Begin
  UnparseAtom(s,GetVarNameAsStr(V))
End;

{ write a constant }
Procedure UnparseConst( s : StrPtr; C : ConstPtr; Quote : Boolean );
Begin
  UnparseAtom(s,GetConstAsStr(C,Quote))
End;

{ write an identifier }
Procedure UnparseIdentifier( s : StrPtr; I : IdPtr; Quotes : Boolean );
Begin
  UnparseAtom(s,GetIdentAsStr(I,Quotes))
End;

{-----------------------------------------------------------------------}
{ Unparse Prolog objects: terms                                         }
{-----------------------------------------------------------------------}

Procedure UnparseTermBis( P : ProgPtr; s : StrPtr; T : TermPtr; Var o : OpPtr;
    InList,ArgList,Quotes : Boolean; 
    Reduce : Boolean; g : TSerial; depth : PosInt ); Forward;

{ write a non-empty comma-separated list of arguments in tuple U }
Procedure UnparseArgument( P : ProgPtr; s : StrPtr; U : TermPtr; 
    Quotes : Boolean; Reduce : Boolean; g : TSerial; depth : PosInt );
Var
  T : TermPtr;
  o : OpPtr; { unused }
Begin
  T := ProtectedGetTupleHead(U,Reduce);
  UnparseTermBis(P,s,T,o,False,False,Quotes,Reduce,g,depth);
  T := ProtectedGetTupleQueue(U,Reduce);
  If Not IsEmptyTuple(T) Then
  Begin
    UnparseShortString(s,',');
    UnparseArgument(P,s,T,Quotes,Reduce,g,depth)
  End
End;

{ write a non-empty tuple }
Procedure UnparseTuple( P : ProgPtr; s : StrPtr; U : TermPtr; 
    Quotes : Boolean; Reduce : Boolean; g : TSerial; depth : PosInt );
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If y = Edinburgh Then
    UnparseShortString(s,'<>(')
  Else
    UnparseShortString(s,'<');
  UnparseArgument(P,s,U,Quotes,Reduce,g,depth);
  If y = Edinburgh Then
    UnparseShortString(s,')')
  Else
    UnparseShortString(s,'>')
End;

{ if Ident is the identifier of an operator and n its arity, return the
 operator, Nil otherwise }
Function GetOpToUnparse( P : ProgPtr; Ident : IdPtr; n : TOpArity ) : OpPtr;
Var
  o : OpPtr;
  functor : TString;
Begin
  GetOpToUnparse := Nil;
  functor := IdentifierGetShortString(Ident);
  o := Op_Lookup(P^.PP_OPER,[OP_FUNCTION,OP_ARITY],'',functor,[],n,0);
  { not a suitable operator? }
  If (o = Nil) Or (Op_GetOperator(o) = '') Then
    Exit;
  GetOpToUnparse := o
End;

{ unparse (append) in s an operant T1 of operator o; 1 denotes the operand  }
Procedure UnparseOperand( P : ProgPtr; s : StrPtr; 
    T1 : TermPtr; o : OpPtr;
    Quotes,Reduce : Boolean; g : TSerial; depth : PosInt);
Var
  s1 : StrPtr;
  o1 : OpPtr; { operands may be operator expressions too }
  pre,pre1 : TPrecedence;
  typ,typ1 : TOpType;
  par : Boolean; { parentheses needed? }
Begin  
  s1 := Str_New(Str_GetEncodingContext(s));
  UnparseTermBis(P,s1,T1,o1,False,False,Quotes,Reduce,g,depth);
  { parentheses around operand needed?}
  par := False;
  If o1 <> Nil Then
  Begin
    pre := Op_GetPrecedence(o);
    typ := Op_GetType(o);
    pre1 := Op_GetPrecedence(o1);
    typ1 := Op_GetType(o1);
    par := (pre1 > pre) Or { priority }
        (pre1 = pre) And ((typ In [fx,xf,xfy]) Or { priority }
          (typ = typ1) And (typ In [fx,xf,fy,yf])) { esthetic }
  End;
  { generate display string }
  If par Then
    UnparseShortString(s,'(');
  UnparseString(s,s1);
  If par Then
    UnparseShortString(s,')')
End;  

{ unparse in s (append to s) an expression T with operator o and operands 
 in tuple U }
Procedure UnparseExpression( P : ProgPtr; s : StrPtr; 
    T : TermPtr; o : OpPtr; U : TermPtr;
    Quotes,Reduce : Boolean; g : TSerial; depth : PosInt);
Var
  Ti : TermPtr; { one operand }
  oper : TString;
  space : Boolean; { space around the operator? }
Begin
  { first operand, if any }
  If Op_GetType(o) In [xf,yf,xfx,yfx,xfy] Then
  Begin
    Ti := ProtectedGetTupleArg(U,Reduce);
    UnparseOperand(P,s,Ti,o,Quotes,Reduce,g,depth)
  End;
  { operator }
  oper := Op_GetOperator(o);
  space := oper[1] In ['a'..'z','A'..'Z','@']; { FIXME: policy + other encodings }
  If space Then
    UnparseShortString(s,' ');
  UnparseShortString(s,oper);
  If space Then
    UnparseShortString(s,' ');
  { second operand, if any }
  If Op_GetType(o) In [fx,fy,xfx,yfx,xfy] Then
  Begin
    Ti := ProtectedGetTupleArg(U,Reduce);
    UnparseOperand(P,s,Ti,o,Quotes,Reduce,g,depth)
  End
End;

{ write in s a string representation of a term T, that could be within a 
 list (InList), or an argument of a predicate (ArgList), and, write quoted 
 terms with or without quotes (Quotes);
 detect and break loop, using the *n format;
 if Reduce is True, use the reduced system by "following" representatives of 
 terms instead of writing the terms themselves;
 if T is an operator / operands construct, set o to the operator, otherwise set
 o to Nil }
Procedure UnparseTermBis( P : ProgPtr; s : StrPtr; T : TermPtr; Var o : OpPtr;
    InList,ArgList,Quotes : Boolean; 
    Reduce : Boolean; g : TSerial; depth : PosInt );
Var
  y : TSyntax;
  { all casts of T: }
  CT : ConstPtr Absolute T;
  VT : VarPtr Absolute T;
  IT : IdPtr Absolute T;
  PT : TObjectPtr Absolute T;
  { others: }
  Tr,Th,Tq : TermPtr;
  ITh : IdPtr Absolute Th;
  T1,T2 : TermPtr;
  Seen : Boolean;
  s2 : StrPtr; { string representation of this term }
  m : PosInt;
  NoExpr : Boolean; { must not be unparsed as an expression }
  NbArgs : TTupleArgNumber;
Begin
  y := GetSyntax(P);
  o := Nil;

  { already seen? }
  Seen := Term_GetSeen(T,1,g);

  { loop detection and management }
  If Seen Then 
  Begin
    s2 := Term_GetDisplay(T);
    { already seen and the string representation is available? just return it }
    If s2 <> Nil Then
    Begin
      UnparseString(s,s2)
    End
    Else { seen but no display yet? that is an infinite loop we must break }
    Begin
      { note: we assume that the parser is not supposed to be capable of 
       reading infinite trees; this is not mentioned in PII doc (p8), but 
       explicit in PII+ doc R 2 -11 (p65); we still treat the integer in this
       notation as an atom, though, to be consistent with the way Prolog 
       numerical constants are displayed }
      m := Term_GetDepth(T);
      UnparseShortString(s,'*');
      UnparseAtomFromShortString(s,PosIntToShortString(depth-m))
    End
  End
  Else
  Begin
    { take note that this term has been seen and initialize depth and display 
     string }
    Term_SetSeen(T,1,g);
    Term_SetDepth(T,depth);
    Term_SetDisplay(T,Nil)
  End;

  If Seen Then
    Exit;

  { build the string representation of this term }
  s2 := Str_New(Str_GetEncodingContext(s));

  { is the LHS of an equation: the string representation is that of the RHS }
  Tr := Red(T);
  If Reduce And (Tr <> Nil) Then
    UnparseTermBis(P,s2,Tr,o,InList,ArgList,Quotes,Reduce,g,depth)
  Else
  Case TypeOfTerm(T) Of
  Constant:
    Begin
      UnparseConst(s2,CT,Quotes)
    End;
  Identifier: { isolated identifier, e.g., hello -> world, or nil }
    Begin
      If IsNil(T) Then
        If y = Edinburgh Then
          UnparseShortString(s2,'[]')
        Else
          UnparseAtomFromShortString(s2,'nil')
      Else
        UnparseIdentifier(s2,IT,Quotes)
    End;
  Variable:
    Begin
      UnparseVarName(s2,VT)
    End;
  FuncSymbol:
    Begin
      If IsEmptyTuple(T) Then
        UnparseShortString(s2,'<>')
      Else If ProtectedGetList(T,T1,T2,Reduce) Then { t is t1.t2 }
      Begin
        If ArgList Then 
          UnparseShortString(s2,'(');
        If (y = Edinburgh) And (Not InList) Then
          UnparseShortString(s2,'[');
        UnparseTermBis(P,s2,T1,o,False,True,Quotes,Reduce,g,depth+1);
        If y <> Edinburgh Then { display as dotted list }
        Begin
          UnparseShortString(s2,'.');
          UnparseTermBis(P,s2,T2,o,True,False,Quotes,Reduce,g,depth+1)
        End
        Else If IsNil(T2) Then { t is t1.nil }
        Begin
          UnparseShortString(s2,']')
        End
        Else If ProtectedIsList(T2,Reduce) Then { t = t1.t2 where t2 is a list }
        Begin
          UnparseShortString(s2,',');
          UnparseTermBis(P,s2,T2,o,True,False,Quotes,Reduce,g,depth+1)
        End
        Else { t = t1.t2 where t2 is a not list }
        Begin
          UnparseShortString(s2,'|');
          UnparseTermBis(P,s2,T2,o,False,False,Quotes,Reduce,g,depth+1);
          UnparseShortString(s2,']')
        End;
        If ArgList Then 
          UnparseShortString(s2,')')
      End
      Else { a non-empty tuple that is not a list }
      Begin 
        Th := ProtectedGetTupleHead(T,Reduce);
        Tq := ProtectedGetTupleQueue(T,Reduce);
        If Not IsEmptyTuple(Tq) And IsIdentifier(Th) And 
            Not IsNil(Th) Then { <ident,a,b,...> == ident(a,b,...) }
        Begin
          NoExpr := y In [PrologIIv1,PrologIIv2];
          If Not NoExpr Then
          Begin
            { look for an existing operator having the right number of operands }
            NbArgs := ProtectedGetTupleArgCount(Tq,Reduce);
            o := GetOpToUnparse(P,IdPtr(Th),NbArgs);
            NoExpr := o = Nil
          End;
          If Not NoExpr Then
            UnparseExpression(P,s2,T,o,Tq,Quotes,Reduce,g,depth) { CHECK: depth}
          Else
          Begin
            { regular ident(arg1,arg2...) form }
            UnparseIdentifier(s2,ITh,Quotes);
            UnparseShortString(s2,'(');
            UnparseArgument(P,s2,Tq,Quotes,Reduce,g,depth+1);
            UnparseShortString(s2, ')')
          End
        End
        Else { <e> or <a,b,...> where a not an identifier or a is 'nil' }
        Begin
          UnparseTuple(P,s2,T,Quotes,Reduce,g,depth+1)
        End
      End
    End
  End;
  UnparseString(s,s2);
  Term_SetDisplay(T,s2)
End;

{ single entry-point for writing a term; all procedures below (and none above) 
 this point must call it; 
 - does not use representatives, since getting rid of temporary variables in 
  solutions is made before calling it; 
 - prevents infinite loops (and thus crashes on stack overflow) using a 'seen' 
  marker;
 - optionally, write infinite trees using "*n" tags;
 - accumulates watched inequations in the global table }

Procedure UnparseTerm( P : ProgPtr; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes : Boolean; Reduce : Boolean );
Var
  g : TSerial;
  o : OpPtr;
Begin
  g := NewSerial;
  UnparseTermBis(P,s,T,o,InList,ArgList,Quotes,Reduce,g,1)
End;

{-----------------------------------------------------------------------}
{ Unparse Prolog objects: high-level objects                            }
{-----------------------------------------------------------------------}

{ Unparse a single equation or inequation }
Procedure UnparseOneEquation( P : ProgPtr; s : StrPtr; E : EqPtr; 
    Reduce : Boolean );
Begin
  UnparseTerm(P,s,Eq_GetLhs(E),False,False,True,Reduce);
  Case Eq_GetType(E) Of
  REL_EQUA:
    UnparseShortString(s,'=');
  REL_INEQ:
    UnparseShortString(s,'#');
  REL_FROZ:
    UnparseShortString(s,'?'); { TODO: PII+ displays a list of frozen goals }
  End;
  UnparseTerm(P,s,Eq_GetRhs(E),False,False,True,Reduce)
End;

{ Unparse non-trivial equations and inequations in the list E, starting with a 
 comma if Comma is True }
Procedure UnparseEquations( P : ProgPtr; s : StrPtr; E : EqPtr; 
    Var Comma : Boolean; Backward : Boolean );
Begin
  While E <> Nil Do
  Begin
    If Not Eq_IsTrivial(E) Then
    Begin
      If Comma Then 
        UnparseShortString(s,', ');
      Comma := True;
      UnparseOneEquation(P,s,E,False)
    End;
    E := Eqs_Next(E,Backward)
  End
End;

{ Unparse a list of equations or inequations; source code only }
Procedure UnparseSystem( P : ProgPtr; s : StrPtr; E : EqPtr; 
    Backward : Boolean );
Var 
  Comma : Boolean;
Begin
  Comma := False;
  UnparseShortString(s,'{ ');
  UnparseEquations(P,s,E,Comma,Backward);
  UnparseShortString(s,' }')
End;

{ Unparse a reduced system for a list of variables DV; if Curl then curly braces 
 are always printed, even if there are no equations or inequations to print }
Procedure UnparseSolution( P : ProgPtr; s : StrPtr; DV : DictPtr );
Var
  L : RestPtr;
  E : EqPtr;
Begin
  L := Nil;
  E := GetSimplifiedSolution(DV,L);
  UnparseSystem(P,s,E,False);
  Rest_Restore(L)
End;

{ Unparse BTerm B (as part of a rule's queue or goals), enclosed in soft marks }
Procedure UnparseOneBTerm( P : ProgPtr; s : StrPtr; B : BTermPtr  );
Var
  cc : TChar;
Begin
  TCharSetSoftMark(cc,SOFT_MARK_TOP_TERM_BEGIN,0);
  Str_AppendChar(s,cc);
  UnparseTerm(P,s,BTerm_GetTerm(B),False,False,True,False);
  TCharSetSoftMark(cc,SOFT_MARK_TOP_TERM_END,0);
  Str_AppendChar(s,cc)
End;

{ Unparse a list of BTerms (rule's queue or goals) }
Procedure UnparseTerms( P : ProgPtr; s : StrPtr; B : BTermPtr; 
    LineBreak : Boolean; sep : TString );
Var 
  y : TSyntax;
  First : Boolean;

  Procedure DoWriteTerms( B : BTermPtr );
  Begin
    If B <> Nil Then
    Begin
      If Not First Then
      Begin
        If y = Edinburgh Then
          UnparseShortString(s,',');
        If LineBreak Then
          UnparseLineBreak(s);
        UnparseShortString(s,sep)
      End;
      UnparseOneBTerm(P,s,B);
      First := False;
      DoWriteTerms(BTerms_GetNext(B))
    End
  End;
Begin
  y := GetSyntax(P);
  First := True;
  DoWriteTerms(B)
End;

{ Unparse a single rule, using its native Prolog syntax; we use 
 UnparseAtomFromShortString to avoid having a line break between the two 
 characters of an arrow, as the parser would fail to read it back }
Procedure UnparseOneRule( P : ProgPtr; s : StrPtr; R : RulePtr  );
Var 
  y : TSyntax;
  B : BTermPtr;
  indent : TString;
Begin
  y := GetSyntax(P);
  indent := '     ';
  B := Rule_GetHead(R);
  UnparseOneBTerm(P,s,B);
  If Length(OSyntax[y].RuleArrow) > 0 Then
  Begin
    UnparseShortString(s,' ');
    UnparseAtomFromShortString(s,OSyntax[y].RuleArrow)
  End;
  B := Rule_GetQueue(R);
  if B <> Nil Then
  Begin
    If Length(OSyntax[y].GoalArrow) > 0 Then
    Begin
      UnparseShortString(s,' ');
      UnparseAtomFromShortString(s,OSyntax[y].GoalArrow)
    End;
    UnparseLineBreak(s);
    UnparseShortString(s,indent)
  End;
  UnparseTerms(P,s,B,True,indent);
  If Rule_GetEqs(R) <> Nil Then
  Begin
    UnparseShortString(s,', ');
    UnparseSystem(P,s,Rule_GetEqs(R),False)
  End;
  UnparseShortString(s,OSyntax[y].RuleEnd)
End;

{ Unparse a single comment }
Procedure UnparseOneComment( P : ProgPtr; s : StrPtr; C : CommPtr );
Begin
  UnparseConst(s,Comment_GetConst(C),True)
End;

{ Unparse a query; we use UnparseAtomFromShortString to avoid having a line break 
 between the two characters of an arrow, as the parser would fail to read it 
 back }
Procedure UnparseOneQuery( P : ProgPtr; s : StrPtr; Q : QueryPtr );
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If Length(OSyntax[y].QueryStart) > 0 Then
  Begin
    UnparseAtomFromShortString(s,OSyntax[y].QueryStart);
    UnparseShortString(s,' ')
  End;
  UnparseTerms(P,s,Query_GetTerms(Q),False,' ');
  If Query_GetSys(Q) <> Nil Then
  Begin
    UnparseShortString(s,' ');
    UnparseSystem(P,s,Query_GetSys(Q),False)
  End;
  UnparseShortString(s,OSyntax[y ].QueryEnd)
End;


{----------------------------------------------------------------------------}
{ output using long strings                                                  }
{----------------------------------------------------------------------------}

{ handles writing Prolog terms to screen or files, echo and paper files,  
 preventing breaking multi-byte characters on screen; final line breaks are 
 expected to be handled by callers }

{-----------------------------------------------------------------------}
{ "ToLongString": string representations                                }
{-----------------------------------------------------------------------}

{ one constant (debugging) }
Function ConstToLongString( enc : TEncoding; P : ProgPtr; 
    C : ConstPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseConst(s,C,True); { with quotes }
  ConstToLongString := s
End;

{ one identifier (debugging) }
Function IdentifierToLongString( enc : TEncoding; P : ProgPtr; 
    I : IdPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseIdentifier(s,I,True);
  IdentifierToLongString := s
End;

{ one variable name (debugging) }
Function VarNameToLongString( enc : TEncoding; P : ProgPtr; 
    V : VarPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseVarName(s,V);
  VarNameToLongString := s
End;

{ a term }
Function TermToLongString( enc : TEncoding; P : ProgPtr; 
    T : TermPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseTerm(P,s,T,False,False,True,True);
  TermToLongString := s
End;

{ a term, unquoted }
Function TermUnquotedToLongString( enc : TEncoding; P : ProgPtr; 
    T : TermPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseTerm(P,s,T,False,False,False,True);
  TermUnquotedToLongString := s
End;

{ one equation (debugging) }
Function OneEquationToLongString( enc : TEncoding; P : ProgPtr; 
    E : EqPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseOneEquation(P,s,E,False); { source code }
  OneEquationToLongString := s
End;

{ the reduced system for the variables in the current query (engine) }
Function QuerySolutionToLongString( enc : TEncoding; P : ProgPtr; 
    Q : QueryPtr ) : StrPtr;
Var
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseSolution(P,s,Query_GetDict(Q));
  QuerySolutionToLongString := s
End;

{ one rule, using its native syntax (list/1); FIXME: use Rule_GetSyntax(R) }
Function OneRuleToLongString( enc : TEncoding; P : ProgPtr; 
    R : RulePtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseOneRule(P,s,R);
  OneRuleToLongString := s
End;

{ one query, using its native syntax; FIXME: use Query_GetSyntax(Q) }
Function OneQueryToLongString( enc : TEncoding; P : ProgPtr; 
    Q : QueryPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseOneQuery(P,s,Q);
  OneQueryToLongString := s
End;

{ one comment (list/1) }
Function OneCommentToLongString( enc : TEncoding; P : ProgPtr; 
    C : CommPtr ) : StrPtr;
Var 
  s : StrPtr;
Begin
  s := Str_New(enc);
  UnparseOneComment(P,s,C);
  OneCommentToLongString := s
End;


{-----------------------------------------------------------------------}
{ "put": write, ignoring the line width system                          }
{-----------------------------------------------------------------------}

{ these write functions ignore the line width system, as for now we impose 
 no requirement to be able to read the output back using in/1 and friends }

{ print one constant }
Procedure PutConst( f : StreamPtr; P : ProgPtr; C : ConstPtr );
Var 
  s : StrPtr;
Begin
  s := ConstToLongString(Stream_GetEncoding(f),P,C);
  Stream_WriteLongString(f,s)
End;

{ print one identifier }
Procedure PutIdentifier( f : StreamPtr; P : ProgPtr; I : IdPtr );
Var 
  s : StrPtr;
Begin
  s := IdentifierToLongString(Stream_GetEncoding(f),P,I);
  Stream_WriteLongString(f,s)
End;

{ print one variable name }
Procedure PutVarName( f : StreamPtr; P : ProgPtr; V : VarPtr );
Var 
  s : StrPtr;
Begin
  s := VarNameToLongString(Stream_GetEncoding(f),P,V);
  Stream_WriteLongString(f,s)
End;

{ print a term }
Procedure PutTerm( f : StreamPtr; P : ProgPtr; T : TermPtr );
Var 
  s : StrPtr;
Begin
  s := TermToLongString(Stream_GetEncoding(f),P,T);
  Stream_WriteLongString(f,s)
End;

{ print a term, unquoted }
Procedure PutTermUnquoted( f : StreamPtr; P : ProgPtr; T : TermPtr );
Var 
  s : StrPtr;
Begin
  s := TermUnquotedToLongString(Stream_GetEncoding(f),P,T);
  Stream_WriteLongString(f,s)
End;

{ print one equation }
Procedure PutOneEquation( f : StreamPtr; P : ProgPtr; E : EqPtr );
Var 
  s : StrPtr;
Begin
  s := OneEquationToLongString(Stream_GetEncoding(f),P,E);
  Stream_WriteLongString(f,s)
End;

{ print the reduced system for the variables in the current query (engine) }
Procedure PutQuerySolution( f : StreamPtr; P : ProgPtr; Q : QueryPtr );
Var
  s : StrPtr;
Begin
  s := QuerySolutionToLongString(Stream_GetEncoding(f),P,Q);
  Stream_WriteLongString(f,s)
End;

{ print one rule, using its native syntax (list/1) }
Procedure PutOneRule( f : StreamPtr; P : ProgPtr; R : RulePtr );
Var 
  s : StrPtr;
Begin
  s := OneRuleToLongString(Stream_GetEncoding(f),P,R);
  Stream_WriteLongString(f,s)
End;

{ print one query, using its native syntax }
Procedure PutOneQuery( f : StreamPtr; P : ProgPtr; Q : QueryPtr );
Var 
  s : StrPtr;
Begin
  s := OneQueryToLongString(Stream_GetEncoding(f),P,Q);
  Stream_WriteLongString(f,s)
End;

{ print one comment (list/1) }
Procedure PutOneComment( f : StreamPtr; P : ProgPtr; C : CommPtr );
Var 
  s : StrPtr;
Begin
  s := OneCommentToLongString(Stream_GetEncoding(f),P,C);
  Stream_WriteLongString(f,s)
End;

{ print a trace message (engine); FIXME: Depth is actually of type TClock  }
Procedure PutTraceMessage( P : ProgPtr; Tag : TString; Depth : PosInt; 
    Branch : PosInt; ClearT : TermPtr );
Var
  f : StreamPtr; 
  s : StrPtr;
Begin
  f := GetTraceStream(P);
  s := Stream_NewStr(f);
  UnparseShortString(s,Tag);
  UnparseShortString(s,': (');
  UnparseShortString(s,PosIntToShortString(Depth));
  UnparseShortString(s,',');
  UnparseShortString(s,PosIntToShortString(Branch));
  UnparseShortString(s,') ');
  UnparseTerm(P,s,ClearT,False,False,False,True);
  Stream_WriteLongString(f,s);
  Stream_LineBreak(f)
End;

{-----------------------------------------------------------------------}
{ "Out": write, using the line width system                             }
{-----------------------------------------------------------------------}

{ output a term }
Procedure OutTerm( f : StreamPtr; P : ProgPtr; T : TermPtr );
Var
  s : StrPtr;
Begin
  s := TermToLongString(Stream_GetEncoding(f),P,T);
  OutFormatted(f,P,s)
End;

{ output a term without quotes }
Procedure OutTermUnquoted( f : StreamPtr; P : ProgPtr; T : TermPtr );
Var
  s : StrPtr;
Begin
  s := TermUnquotedToLongString(Stream_GetEncoding(f),P,T);
  OutFormatted(f,P,s)
End;

Begin
  TCharSetFromAscii(CC_LINE_CONTINUATION,'\')
End.