{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
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
  Memory,
  Strings,
  Errs,
  OStack,
  PObj,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjProg,
  Encoding;

Procedure OutString( s : StrPtr; UseIOStack : Boolean );
Procedure OutStringCR( s : StrPtr; UseIOStack : Boolean );
Procedure OutCR( UseIOStack : Boolean );
Procedure OutConst( C : ConstPtr; UseIOStack : Boolean );
Procedure OutIdentifier( I : IdPtr; UseIOStack : Boolean );
Procedure OutVarName( V : VarPtr; UseIOStack : Boolean );
Procedure OutOneEquation( y : TSyntax; E : EqPtr; UseIOStack : Boolean );
Procedure OutQuerySolution( Q : QueryPtr; UseIOStack : Boolean );
Procedure OutTermBis( y : TSyntax; T : TermPtr; ArgList,Quotes : Boolean; 
    UseIOStack : Boolean );
Procedure OutTerm( y : TSyntax; T : TermPtr; UseIOStack : Boolean );
Procedure OutOneRule( R : RulePtr; UseIOStack : Boolean );
Procedure OutRuleRange( R1,R2 : RulePtr; RuleType : RuType; 
    UseIOStack : Boolean );
Procedure OutOneQuery( Q : QueryPtr; UseIOStack : Boolean );


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
{ inequation stack                                                           }
{----------------------------------------------------------------------------}

Const MaxIneq = 1000;

Type StakIneq = Array[1..MaxIneq] Of EqPtr; { stack of inequations to print }

Var
  Ineq    : StakIneq;                    { inequations to print  }
  IdxIneq : Integer;                     { top of the stack      }

{ reset the stack of inequations to print }
Procedure InitIneq;
Begin
  IdxIneq := 0
End;

{ add one inequation to the stack; avoid duplicates; FIXME: does not scale }
Procedure AddOneIneq( E : EqPtr );
Var
  I : Integer;
  Found : Boolean;
Begin
  Found := False;
  I := 1;
  While (I<=IdxIneq) And Not Found Do
  Begin
    Found := SameEquations(Ineq[I],E);
    I := I + 1
  End;
  If Not Found Then
  Begin
    IdxIneq := IdxIneq + 1;
    CheckCondition(IdxIneq <= MaxIneq,'Maximum number of inequations reached');
    Ineq[IdxIneq] := E
  End
End;

{ add a list of inequations to the stack }
Procedure AddIneq( E : EqPtr );
Begin
  While E<>Nil Do
  Begin
    AddOneIneq(E);
    E := E^.EQ_NEXT
  End
End;

{----------------------------------------------------------------------------}
{ naming of temporary variables                                              }
{----------------------------------------------------------------------------}

{ return the name of a user or temporary variable }
Function GetVarNameAsString( V : VarPtr ) : StrPtr;
Var 
  TV : TermPtr Absolute V;
  PV : TObjectPtr Absolute V;
  s : StrPtr;
  k : Integer;
  ks : TString;
Begin
  CheckCondition(TypeOfTerm(TV)=Variable,
    'GetVarNameAsString(V): V is not a variable');
  s := StrClone(VariableGetName(V));
  k := ObjectCopyNumber(PV);
  If k > 0 Then
  Begin
    Str(k,ks);
    StrAppend(s, '_' + ks) { FIXME: make sure it is an invalid variable name }
  End;
  GetVarNameAsString := s
End;


{----------------------------------------------------------------------------}
{ write                                                                      }
{----------------------------------------------------------------------------}

Procedure WriteTermBis( y : TSyntax; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes,Solution : Boolean); Forward;

{ write a comma-separated list of arguments in tuple U }
Procedure WriteArgument( y : TSyntax; s : StrPtr; U : TermPtr; 
    Solution : Boolean );
Var
  T : TermPtr;
Begin
  T := GetTupleHead(U,Solution);
  WriteTermBis(y,s,T,False,False,True,Solution);
  T := GetTupleQueue(U,Solution);
  If T <> Nil Then
  Begin
    StrAppend(s,',');
    WriteArgument(y,s,T,Solution)
  End
End;

{ write the name of a user or temporary variable }
Procedure WriteVarName( s : StrPtr; V : VarPtr );
Begin
  StrConcat(s,GetVarNameAsString(V))
End;

{ write a constant }
Procedure WriteConst( s : StrPtr; C : ConstPtr; Quote : Boolean );
Begin
  StrConcat(s,GetConstAsString(C,Quote))
End;

{ write an identifier }
Procedure WriteIdentifier( s : StrPtr; I : IdPtr );
Begin
  StrConcat(s,IdentifierGetStr(I))
End;


{ write a tuple }
Procedure WriteTuple( y : TSyntax; s : StrPtr; U : TermPtr; 
    Solution : Boolean );
Begin
  If y = Edinburgh Then
    StrAppend(s,'<>(')
  Else
    StrAppend(s,'<');
  WriteArgument(y,s,U,Solution);
  If y = Edinburgh Then
    StrAppend(s,')')
  Else
    StrAppend(s,'>')
End;

{ write a term, possibly as an argument of a predicate, and with quotes; 
  if Solution is true, we are currently printing a solution (that is, a 
  reduced system), and not the item as given in the source code ) }
Procedure WriteTermBis( y : TSyntax; s : StrPtr; T : TermPtr; 
    InList,ArgList,Quotes,Solution : Boolean );
Var
  { all casts of T: }
  CT : ConstPtr Absolute T;
  VT : VarPtr Absolute T;
  IT : IdPtr Absolute T;
  PT : TObjectPtr Absolute T;
  { others: }
  Th : TermPtr;
  ITh : IdPtr Absolute Th;
  T1,T2 : TermPtr;
Begin
  Case TypeOfTerm(T) Of
  Constant:
    Begin
      WriteConst(s,CT,Quotes)
    End;
  Identifier: { isolated identifier, e.g., hello -> world, or nil }
    Begin
      If IsNil(T) Then
        If y = Edinburgh Then
          StrAppend(s,'[]')
        Else
          StrAppend(s,'nil')
      Else
        WriteIdentifier(s,IT)
    End;
  Variable:
    Begin
      If Not Solution Then
        WriteVarName(s,VT)
      Else
      Begin
        If ObjectCopyNumber(PT) = 0 Then
        Begin
          WriteVarName(s,VT)
        End
        Else
          If VRed(VT) = Nil Then
            WriteVarName(s,VT)
          Else
          Begin
            WriteTermBis(y,s,VRed(VT),False,False,Quotes,Solution)
          End;
        If WatchIneq(VT) <> Nil Then
          AddIneq(WatchIneq(VT))
      End
    End;
  FuncSymbol:
    Begin
      If IsEmptyTuple(T) Then
        StrAppend(s,'<>')
      Else If GetList(T,T1,T2,Solution) Then { t is t1.t2 }
      Begin
        If ArgList Then 
          StrAppend(s,'(');
        If (y = Edinburgh) And (Not InList) Then
          StrAppend(s,'[');
        WriteTermBis(y,s,T1,False,True,True,Solution);
        If y <> Edinburgh Then { display as dotted list }
        Begin
          StrAppend(s,'.');
          WriteTermBis(y,s,T2,True,False,True,Solution)
        End
        Else If IsNil(T2) Then { t is t1.nil }
        Begin
          StrAppend(s,']')
        End
        Else If IsList(T2,Solution) Then { t = t1.t2 where t2 is a list }
        Begin
          StrAppend(s,',');
          WriteTermBis(y,s,T2,True,False,True,Solution)
        End
        Else { t = t1.t2 where t2 is a not list }
        Begin
          StrAppend(s,'|');
          WriteTermBis(y,s,T2,False,False,True,Solution);
          StrAppend(s,']')
        End;
        If ArgList Then 
          StrAppend(s,')')
      End
      Else { a non-empty tuple that is not a list }
      Begin 
        Th := GetTupleHead(T,Solution);
        If TypeOfTerm(Th) = Identifier Then
        Begin
          WriteIdentifier(s,ITh);
          T1 := GetTupleQueue(T,Solution);
          If T1 <> Nil Then { <ident,a,b,...> == ident(a,b,...) }
          Begin
            StrAppend(s,'(');
            WriteArgument(y,s,T1,Solution);
            StrAppend(s, ')')
          End
        End
        Else { <a,b,...> where a not an identifier }
        Begin
          WriteTuple(y,s,T,Solution)
        End
      End
    End
  End
End;

Procedure WriteTerm( y : TSyntax; s : StrPtr; T : TermPtr; 
    Solution : Boolean ); Forward;

{ write a single equation or inequation }
Procedure WriteOneEquation( y : TSyntax; s : StrPtr; E : EqPtr; 
    Solution : Boolean  );
Begin
  WriteTerm(y,s,E^.EQ_LTER,Solution);
  Case E^.EQ_TYPE Of
  REL_EQUA:
    StrAppend(s,' = ');
  REL_INEQ:
    StrAppend(s,' <> ');
  End;
  WriteTerm(y,s,E^.EQ_RTER,Solution)
End;


{ write equations and inequations in the list E, starting with a comma 
  if Comma is True }
Procedure WriteEquationsBis( y : TSyntax; s : StrPtr; E : EqPtr; 
    Var Comma : Boolean );
Begin
  If Comma Then 
    StrAppend(s,', ');
  Comma := True;
  WriteOneEquation(y,s,E,False); { not part of a solution }
  If E^.EQ_NEXT <> Nil Then
    WriteEquationsBis(y,s,E^.EQ_NEXT,Comma)
End;

{ write a list of equations }
Procedure WriteEquations( y : TSyntax; s : StrPtr; E : EqPtr );
Var Comma : Boolean;
Begin
  Comma := False;
  WriteEquationsBis(y,s,E,Comma)
End;

{ write a list of equations or inequations; source code only }
Procedure WriteSourceSystem( s : StrPtr; E : EqPtr );
Begin
  StrAppend(s,'{ ');
  WriteEquations(PrologIIc,s,E);
  StrAppend(s,' }')
End;

{ write a system; source code only; dead code for now }
Procedure WriteSourceSys( s : StrPtr; Sys : SysPtr );
Begin
  StrAppend(s,'{ ');
  WriteEquations(PrologIIc,s,Sys^.SY_EQUA);
  If (Sys^.SY_EQUA<>Nil) And (Sys^.SY_INEQ<>Nil) Then
    StrAppend(s,', ');
  WriteEquations(PrologIIc,s,Sys^.SY_INEQ);
  StrAppend(s,' }')
End;

{ write all inequations in the stack }
Procedure WriteStackedInequations( y : TSyntax; s : StrPtr; 
    Var Before : Boolean );
Var I,K : Integer;
Begin
  K := IdxIneq;
  For I := K DownTo 1 Do 
  Begin
    If Before Then
      StrAppend(s,', ');
    WriteOneEquation(y,s,Ineq[I],True);
    Before := True
  End
End;

{ write a top-level term (i.e. a term that is not an argument of a predicate) }
Procedure WriteTerm( y : TSyntax; s : StrPtr; T : TermPtr; 
    Solution : Boolean );
Begin
  If Solution Then
    T := RepresentativeOf(T);
  WriteTermBis(y,s,T,False,False,True,Solution)
End;

{ print an opening curly brace if it has not been printed yet }
Procedure WriteCurlyBrace( s : StrPtr; SpaceBeforeCurl : Boolean; 
    Var Printed : Boolean );
Begin
  If not Printed Then
  Begin
    Printed := True;
    If SpaceBeforeCurl Then
      StrAppend(s,' ');
    StrAppend(s,'{ ')
  End
End;

{ write constraints on variables from DV1 to DV2 (excluding DV2); Printed is
  true if a curly brace has already been printed; Before is true if an equation
  or inequation has already been printed; if Curl then curly braces are 
  always printed, even if there are no equations or inequations to print  }
Procedure WriteSolutionBis( y : TSyntax; s : StrPtr; DV1,DV2 : DictPtr;
    Curl, SpaceBeforeCurl : Boolean; 
    Var Printed, Before : Boolean );
Var 
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  If (DV1<>Nil) And (DV1<>DV2) Then
  Begin
    WriteSolutionBis(y,s,DV1^.DE_NEXT,DV2,Curl,SpaceBeforeCurl,Printed,Before);
    TV := DV1^.DE_TERM;
    { equation in the reduced system }
    If VRed(V) <> Nil Then
    Begin
      WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
      If Before Then 
        StrAppend(s,', ');
      WriteVarName(s,V);
      StrAppend(s,' = ');
      Before := True;
      WriteTerm(y,s,VRed(V),True)
    End;
    { inequations in the reduced system }
    If WatchIneq(V) <> Nil Then
    Begin
      WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
      AddIneq(WatchIneq(V))
    End
  End
End;

{ write a reduced system; see comments about parameters above }
Procedure WriteSolution( y : TSyntax; s : StrPtr; start, stop : DictPtr;
    Curl, SpaceBeforeCurl : Boolean );
Var
  Printed, Before : Boolean;
Begin
  Printed := False; { no curly brace printed yet }
  Before := False; { no equation printed yet }
  If Curl Then 
    WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
  WriteSolutionBis(y,s,start,stop,Curl,SpaceBeforeCurl,Printed,Before);
  WriteStackedInequations(y,s,Before);
  If Printed Then
    StrAppend(s,' }')
End;

{ write BTerm B (as part of a rule's queue or goals) }
Procedure WriteOneBTerm( y : TSyntax; s : StrPtr; B : BTermPtr );
Begin
  WriteTerm(y,s,B^.BT_TERM,False)
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
          StrAppend(s,',');
        StrAppend(s,sep)
      End;
      WriteOneBTerm(y,s,B);
      First := False;
      DoWriteTerms(B^.BT_NEXT)
    End
  End;
Begin
  First := True;
  DoWriteTerms(B)
End;

{ write a single rule }
Procedure WriteOneRule( s : StrPtr; R : RulePtr );
Var 
  B : BTermPtr;
  prefix : TString;
  y : TSyntax;
Begin
  y := GetRuleSyntax(R);
  prefix := CRLF + '        ';
  InitIneq;
  B := R^.RU_FBTR;
  WriteOneBTerm(y,s,B);
  StrAppend(s,OSyntax[y].RuleArrow);
  if B^.BT_NEXT <> Nil Then
  Begin
    StrAppend(s,OSyntax[y].GoalArrow);
    StrAppend(s,prefix)
  End;
  WriteTerms(y,s,B^.BT_NEXT,prefix);
  If R^.RU_SYST <> Nil Then
  Begin
    StrAppend(s,' ');
    WriteSourceSystem(s,R^.RU_SYST)
  End;
  StrAppend(s,OSyntax[y].RuleEnd)
End;

{ write a list of rules }
Procedure WriteRules( s : StrPtr; R : RulePtr) ;
Begin
  if R <> Nil Then
  Begin
    WriteOneRule(s,R);
    WriteRules(s,NextRule(R))
  End
End;

{ write rules between R1 and R2 }
Procedure WriteRuleRange( s : StrPtr; R1,R2 : RulePtr; RuleType : RuType );
Var
  R : RulePtr;
  Stop : Boolean;
Begin
  R := R1;
  Stop := R = Nil;
  While Not Stop Do
  Begin
    If R^.RU_TYPE = RuleType Then
    Begin
      WriteOneRule(s,R);
      StrAppendCR(s)
    End;
    Stop := R = R2;
    R := NextRule(R);
    Stop := Stop Or (R = Nil)
  End
End;

{ write a query }
Procedure WriteOneQuery( s : StrPtr; Q : QueryPtr );
Var
  y : TSyntax;
Begin
  y := GetQuerySyntax(Q);
  StrAppend(s,OSyntax[y].QueryStart);
  WriteTerms(y,s,Q^.QU_FBTR,' ');
  If Q^.QU_SYST <> Nil Then
  Begin
    StrAppend(s,' ');
    WriteSourceSystem(s,Q^.QU_SYST)
  End;
  StrAppend(s,OSyntax[y].QueryEnd);
End;

{ write a list of query }
Procedure WriteQueries( s : StrPtr; Q : QueryPtr );
Begin
  if Q <> Nil Then
  Begin
    WriteOneQuery(s,Q);
    WriteQueries(s,NextQuery(Q))
  End
End;

{ write all queries in a program }
Procedure WriteProgramQueries( s : StrPtr; P : ProgPtr );
Begin
  WriteQueries(s,FirstProgramQuery(P))
End;


{ write all rules in a program }
Procedure WriteProgramRules( s : StrPtr; P : ProgPtr );
Begin
  WriteRules(s,FirstProgramRule(P))
End;


{----------------------------------------------------------------------------}
{ output using long strings                                                  }
{----------------------------------------------------------------------------}

{ write a string }
Procedure OutString( s : StrPtr; UseIOStack : Boolean );
Begin
  If Not UseIOStack Or OutputIsTerminal Then
    StrWrite(s)
  Else
    StrWriteToCurrentFile(s)
End;

{ write a string followed by a carriage return; do not alter the
  string passed as parameter }
Procedure OutStringCR( s : StrPtr; UseIOStack : Boolean );
Begin
  OutString(s,UseIOStack);
  OutString(NewStringFrom(CRLF),UseIOStack)
End;

{ write a carriage return }
Procedure OutCR( UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  OutStringCR(s,UseIOStack)
End;

Procedure OutConst( C : ConstPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteConst(s,C,True); { with quotes }
  OutString(s,UseIOStack)
End;

Procedure OutIdentifier( I : IdPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteIdentifier(s,I);
  OutString(s,UseIOStack)
End;

Procedure OutVarName( V : VarPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteVarName(s,V);
  OutString(s,UseIOStack)
End;

{ print one equation (debugging) }
Procedure OutOneEquation( y : TSyntax; E : EqPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneEquation(y,s,E,False); { source code }
  OutString(s,UseIOStack)
End;

Procedure OutSolution( y : TSyntax; start,stop : DictPtr; 
    UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteSolution(y,s,start,stop,True,False);
  OutString(s,UseIOStack)
End;

{ output the reduced system for the variables in the current query }
Procedure OutQuerySolution( Q : QueryPtr; UseIOStack : Boolean );
Begin
  InitIneq;
  OutSolution(GetQuerySyntax(Q),Q^.QU_FVAR,Q^.QU_LVAR,UseIOStack)
End;

{ output a term that is not an argument of a predicate }
Procedure OutTermBis( y : TSyntax; T : TermPtr; ArgList,Quotes : Boolean; 
  UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteTermBis(y,s,T,False,ArgList,Quotes,True);
  OutString(s,UseIOStack)
End;

{ output a term }
Procedure OutTerm( y : TSyntax; T : TermPtr; UseIOStack : Boolean );
Begin
  OutTermBis(y,T,False,True,UseIOStack)
End;

Procedure OutTerms( y : TSyntax; B : BTermPtr; sep : TString; 
    UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteTerms(y,s,B,sep);
  OutString(s,UseIOStack)
End;

Procedure OutOneRule( R : RulePtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneRule(s,R);
  OutStringCR(s,UseIOStack)
End;

Procedure OutRuleRange( R1,R2 : RulePtr; RuleType : RuType; 
    UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteRuleRange(s,R1,R2,RuleType);
  OutString(s,UseIOStack)
End;

Procedure OutOneQuery( Q : QueryPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneQuery(s,Q);
  OutStringCR(s,UseIOStack)
End;

End.