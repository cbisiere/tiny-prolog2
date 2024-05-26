{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2022,2023,2024                                             }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              U N P A R S I N G                             }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

Unit Unparse;

Interface

Uses
  Chars,
  Num,
  ShortStr,
  Memory,
  Errs,
  Trace,
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
  Tuple,
  Encoding;

Procedure OutString( f : StreamPtr; s : StrPtr );
Procedure OutlnString( f : StreamPtr; s : StrPtr );
Procedure Outln( f : StreamPtr );
Procedure OutConst( f : StreamPtr; C : ConstPtr );
Procedure OutIdentifier( f : StreamPtr; I : IdPtr );
Procedure OutVarName( f : StreamPtr; V : VarPtr );
Procedure OutOneEquation( f : StreamPtr; y : TSyntax; E : EqPtr );
Procedure OutQuerySolution( f : StreamPtr; Q : QueryPtr );
Procedure OutTermBis( f : StreamPtr; y : TSyntax; T : TermPtr; 
    ArgList,Quotes : Boolean );
Procedure OutTerm( f : StreamPtr; y : TSyntax; T : TermPtr );
Procedure OutOneRule( f : StreamPtr; R : RulePtr );
Procedure OutOneQuery( f : StreamPtr; Q : QueryPtr );
Procedure OutOneComment( f : StreamPtr; C : CommPtr );

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
    (RuleArrow:' ->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:' ->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:' ->';GoalArrow:'';RuleEnd:';';QueryStart:'->';QueryEnd:';'),
    (RuleArrow:'';GoalArrow:' :-';RuleEnd:'.';QueryStart:':-';QueryEnd:'.')
  );

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

  { V = T; also applies to assigned identifiers; it would ne abnormal to have
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
Begin
  S := Sys_New;
  g := NewSerial;
  { collect all equations and inequations }
  While DV <> Nil Do
  Begin
    CollectEquationsFromTerm(Dict_GetTerm(DV),S,g);
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
  guid : LongInt;
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
{ write                                                                      }
{----------------------------------------------------------------------------}

Procedure WriteTermBis( y : TSyntax; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes : Boolean; 
    Reduce : Boolean; g : TSerial; depth : PosInt ); Forward;

{ write the name of a user or temporary variable }
Procedure WriteVarName( s : StrPtr; V : VarPtr );
Begin
  Str_Concat(s,GetVarNameAsStr(V))
End;

{ write a constant }
Procedure WriteConst( s : StrPtr; C : ConstPtr; Quote : Boolean );
Begin
  Str_Concat(s,GetConstAsStr(C,Quote))
End;

{ write an identifier }
Procedure WriteIdentifier( s : StrPtr; I : IdPtr; Quotes : Boolean );
Begin
  Str_Concat(s,GetIdentAsStr(I,Quotes))
End;

{ write a comma-separated list of arguments in tuple U }
Procedure WriteArgument( y : TSyntax; s : StrPtr; U : TermPtr; 
    Reduce : Boolean; g : TSerial; depth : PosInt );
Var
  T : TermPtr;
Begin
  T := ProtectedGetTupleHead(U,Reduce);
  WriteTermBis(y,s,T,False,False,True,Reduce,g,depth);
  T := ProtectedGetTupleQueue(U,Reduce);
  If T <> Nil Then
  Begin
    Str_Append(s,',');
    WriteArgument(y,s,T,Reduce,g,depth)
  End
End;

{ write a tuple }
Procedure WriteTuple( y : TSyntax; s : StrPtr; U : TermPtr; 
    Reduce : Boolean; g : TSerial; depth : PosInt );
Begin
  If y = Edinburgh Then
    Str_Append(s,'<>(')
  Else
    Str_Append(s,'<');
  WriteArgument(y,s,U,Reduce,g,depth);
  If y = Edinburgh Then
    Str_Append(s,')')
  Else
    Str_Append(s,'>')
End;

{ write a term, that could be within a list (InList), or an argument of a 
 predicate (ArgList), and, write quoted terms with or without quotes (Quotes);
 detect and break loop, using the *n format;
 if Reduce is True, use the reduced system by "following" representatives of 
 terms instead of writing the terms themselves }
Procedure WriteTermBis( y : TSyntax; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes : Boolean; 
    Reduce : Boolean; g : TSerial; depth : PosInt );
Var
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
Begin
  { already seen? }
  Seen := Term_GetSeen(T,1,g);

  { loop detection and management }
  If Seen Then 
  Begin
    s2 := Term_GetDisplay(T);
    { already seen and the string representation is available? just return it }
    If s2 <> Nil Then
    Begin
      Str_Concat(s,s2)
    End
    Else { seen but no display yet? that is an infinite loop we must break }
    Begin
      m := Term_GetDepth(T);
      Str_Append(s,'*');
      Str_Append(s,PosIntToShortString(depth-m))
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
  s2 := Str_New;

  { is the LHS of an equation: the string representation is that of the RHS }
  Tr := Red(T);
  If Reduce And (Tr <> Nil) Then
    WriteTermBis(y,s2,Tr,InList,ArgList,Quotes,Reduce,g,depth)
  Else
  Case TypeOfTerm(T) Of
  Constant:
    Begin
      WriteConst(s2,CT,Quotes)
    End;
  Identifier: { isolated identifier, e.g., hello -> world, or nil }
    Begin
      If IsNil(T) Then
        If y = Edinburgh Then
          Str_Append(s2,'[]')
        Else
          Str_Append(s2,'nil')
      Else
        WriteIdentifier(s2,IT,Quotes)
    End;
  Variable:
    Begin
      WriteVarName(s2,VT)
    End;
  FuncSymbol:
    Begin
      If IsEmptyTuple(T) Then
        Str_Append(s2,'<>')
      Else If ProtectedGetList(T,T1,T2,Reduce) Then { t is t1.t2 }
      Begin
        If ArgList Then 
          Str_Append(s2,'(');
        If (y = Edinburgh) And (Not InList) Then
          Str_Append(s2,'[');
        WriteTermBis(y,s2,T1,False,True,True,Reduce,g,depth+1);
        If y <> Edinburgh Then { display as dotted list }
        Begin
          Str_Append(s2,'.');
          WriteTermBis(y,s2,T2,True,False,True,Reduce,g,depth+1)
        End
        Else If IsNil(T2) Then { t is t1.nil }
        Begin
          Str_Append(s2,']')
        End
        Else If ProtectedIsList(T2,Reduce) Then { t = t1.t2 where t2 is a list }
        Begin
          Str_Append(s2,',');
          WriteTermBis(y,s2,T2,True,False,True,Reduce,g,depth+1)
        End
        Else { t = t1.t2 where t2 is a not list }
        Begin
          Str_Append(s2,'|');
          WriteTermBis(y,s2,T2,False,False,True,Reduce,g,depth+1);
          Str_Append(s2,']')
        End;
        If ArgList Then 
          Str_Append(s2,')')
      End
      Else { a non-empty tuple that is not a list }
      Begin 
        Th := ProtectedGetTupleHead(T,Reduce);
        Tq := ProtectedGetTupleQueue(T,Reduce);
        If (Tq <> Nil) And (TypeOfTerm(Th) = Identifier) And 
            (Not IsNil(Th)) Then { <ident,a,b,...> == ident(a,b,...) }
        Begin
          WriteIdentifier(s2,ITh,Quotes);
          Str_Append(s2,'(');
          WriteArgument(y,s2,Tq,Reduce,g,depth+1);
          Str_Append(s2, ')')
        End
        Else { <e> or <a,b,...> where a not an identifier or a is 'nil' }
        Begin
          WriteTuple(y,s2,T,Reduce,g,depth+1)
        End
      End
    End
  End;
  Str_Concat(s,s2);
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

Procedure WriteTerm( y : TSyntax; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes : Boolean; Reduce : Boolean );
Var
  g : TSerial;
Begin
  g := NewSerial;
  WriteTermBis(y,s,T,InList,ArgList,Quotes,Reduce,g,1)
End;

{ write a single equation or inequation }
Procedure WriteOneEquation( y : TSyntax; s : StrPtr; E : EqPtr; 
    Reduce : Boolean );
Begin
  WriteTerm(y,s,Eq_GetLhs(E),False,False,True,Reduce);
  Case Eq_GetType(E) Of
  REL_EQUA:
    Str_Append(s,'=');
  REL_INEQ:
    Str_Append(s,'#');
  REL_FROZ:
    Str_Append(s,'?'); { TODO: PII+ displays a list of frozen goals }
  End;
  WriteTerm(y,s,Eq_GetRhs(E),False,False,True,Reduce)
End;

{ write non-trivial equations and inequations in the list E, starting with a 
 comma if Comma is True }
Procedure WriteEquations( y : TSyntax; s : StrPtr; E : EqPtr; 
    Var Comma : Boolean; Backward : Boolean );
Begin
  While E <> Nil Do
  Begin
    If Not Eq_IsTrivial(E) Then
    Begin
      If Comma Then 
        Str_Append(s,', ');
      Comma := True;
      WriteOneEquation(y,s,E,False)
    End;
    E := Eqs_Next(E,Backward)
  End
End;

{ write a list of equations or inequations; source code only }
Procedure WriteSystem( y : TSyntax; s : StrPtr; E : EqPtr; Backward : Boolean );
Var 
  Comma : Boolean;
Begin
  Comma := False;
  Str_Append(s,'{ ');
  WriteEquations(y,s,E,Comma,Backward);
  Str_Append(s,' }')
End;

{ write a reduced system for a list of variables DV; if Curl then curly braces 
 are always printed, even if there are no equations or inequations to print }
Procedure WriteSolution( y : TSyntax; s : StrPtr; DV : DictPtr );
Var
  L : RestPtr;
  E : EqPtr;
Begin
  L := Nil;
  E := GetSimplifiedSolution(DV,L);
  WriteSystem(y,s,E,False);
  Rest_Restore(L)
End;

{ write BTerm B (as part of a rule's queue or goals) }
Procedure WriteOneBTerm( y : TSyntax; s : StrPtr; B : BTermPtr  );
Begin
  WriteTerm(y,s,BTerm_GetTerm(B),False,False,True,False)
End;

{ write a list of BTerms (rule's queue or goals) }
Procedure WriteTerms( y : TSyntax; s : StrPtr; B : BTermPtr; sep : TString );
Var 
  First : Boolean;
  Procedure DoWriteTerms( B : BTermPtr );
  Begin
    If B <> Nil Then
    Begin
      If Not First Then
      Begin
        If y = Edinburgh Then
          Str_Append(s,',');
        Str_Append(s,sep)
      End;
      WriteOneBTerm(y,s,B);
      First := False;
      DoWriteTerms(BTerms_GetNext(B))
    End
  End;
Begin
  First := True;
  DoWriteTerms(B)
End;

{ write a single rule }
Procedure WriteOneRule( s : StrPtr; R : RulePtr  );
Var 
  B : BTermPtr;
  prefix : TString;
  y : TSyntax;
Begin
  y := Rule_GetSyntax(R);
  prefix := NewLine + '        ';
  B := Rule_GetHead(R);
  WriteOneBTerm(y,s,B);
  Str_Append(s,OSyntax[y].RuleArrow);
  B := Rule_GetQueue(R);
  if B <> Nil Then
  Begin
    Str_Append(s,OSyntax[y].GoalArrow);
    Str_Append(s,prefix)
  End;
  WriteTerms(y,s,B,prefix);
  If R^.RU_SYST <> Nil Then
  Begin
    Str_Append(s,', ');
    WriteSystem(y,s,R^.RU_SYST,False)
  End;
  Str_Append(s,OSyntax[y].RuleEnd)
End;

{ write a single comment }
Procedure WriteOneComment( s : StrPtr; C : CommPtr );
Begin
  WriteConst(s,Comment_GetConst(C),True)
End;

{ write a query }
Procedure WriteOneQuery( s : StrPtr; Q : QueryPtr );
Var
  y : TSyntax;
Begin
  y := Query_GetSyntax(Q);
  Str_Append(s,OSyntax[y].QueryStart + ' ');
  WriteTerms(y,s,Query_GetTerms(Q),' ');
  If Query_GetSys(Q) <> Nil Then
  Begin
    Str_Append(s,' ');
    WriteSystem(y,s,Query_GetSys(Q),False)
  End;
  Str_Append(s,OSyntax[y].QueryEnd);
End;


{----------------------------------------------------------------------------}
{ output using long strings                                                  }
{----------------------------------------------------------------------------}

{ write a string to stream f or Crt }
Procedure OutString( f : StreamPtr; s : StrPtr );
Begin
  If (f = Nil) Or Stream_IsConsole(f) Then
    Str_CWrite(s)
  Else
    Str_Write(f,s)
End;

{ write a string followed by a carriage return; do not alter the
  string passed as parameter }
Procedure OutlnString( f : StreamPtr; s : StrPtr );
Begin
  If (f = Nil) Or Stream_IsConsole(f) Then
  Begin
    Str_CWrite(s);
    CWriteLn
  End
  Else
    Str_Writeln(f,s)
End;

{ line }
Procedure Outln( f : StreamPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  OutlnString(f,s)
End;

Procedure OutConst( f : StreamPtr; C : ConstPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteConst(s,C,True); { with quotes }
  OutString(f,s)
End;

Procedure OutIdentifier( f : StreamPtr; I : IdPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteIdentifier(s,I,True);
  OutString(f,s)
End;

Procedure OutVarName( f : StreamPtr; V : VarPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteVarName(s,V);
  OutString(f,s)
End;

{ print one equation (debugging) }
Procedure OutOneEquation( f : StreamPtr; y : TSyntax; E : EqPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneEquation(y,s,E,False); { source code }
  OutString(f,s)
End;

{ output the reduced system for the variables in the current query }
Procedure OutQuerySolution( f : StreamPtr; Q : QueryPtr );
Var
  s : StrPtr;
Begin
  s := Str_New;
  WriteSolution(Query_GetSyntax(Q),s,Query_GetDict(Q));
  OutString(f,s)
End;

{ output a term that is not an argument of a predicate, using the reduced 
 system }
Procedure OutTermBis( f : StreamPtr; y : TSyntax; T : TermPtr; 
    ArgList,Quotes : Boolean );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteTerm(y,s,T,False,ArgList,Quotes,True);
  OutString(f,s)
End;

{ output a term }
Procedure OutTerm( f : StreamPtr; y : TSyntax; T : TermPtr );
Begin
  OutTermBis(f,y,T,False,True)
End;

Procedure OutOneRule( f : StreamPtr; R : RulePtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneRule(s,R);
  OutlnString(f,s)
End;

Procedure OutOneQuery( f : StreamPtr; Q : QueryPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneQuery(s,Q);
  OutlnString(f,s)
End;

Procedure OutOneComment( f : StreamPtr; C : CommPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneComment(s,C);
  OutlnString(f,s)
End;

End.