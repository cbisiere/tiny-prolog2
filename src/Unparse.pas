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
  ShortStr,
  Errs,
  PObj,
  PObjIO,
  PObjStr,
  PObjDict,
  PObjEq,
  PObjTerm,
  PObjDef,
  PObjBter,
  PObjComm,
  PObjRule,
  PObjQury,
  PObjProg,
  Encoding;

Procedure OutString( f : StreamPtr; s : StrPtr );
Procedure OutStringCR( f : StreamPtr; s : StrPtr );
Procedure OutCR( f : StreamPtr );
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
  s := Str_Clone(VariableGetName(V));
  k := ObjectCopyNumber(PV);
  If k > 0 Then
  Begin
    Str(k,ks);
    Str_Append(s, '_' + ks) { FIXME: make sure it is an invalid variable name }
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
    Str_Append(s,',');
    WriteArgument(y,s,T,Solution)
  End
End;

{ write the name of a user or temporary variable }
Procedure WriteVarName( s : StrPtr; V : VarPtr );
Begin
  Str_Concat(s,GetVarNameAsString(V))
End;

{ write a constant }
Procedure WriteConst( s : StrPtr; C : ConstPtr; Quote : Boolean );
Begin
  Str_Concat(s,GetConstAsString(C,Quote))
End;

{ write an identifier }
Procedure WriteIdentifier( s : StrPtr; I : IdPtr; Quotes : Boolean );
Begin
  Str_Concat(s,GetIdentAsString(I,Quotes))
End;


{ write a tuple }
Procedure WriteTuple( y : TSyntax; s : StrPtr; U : TermPtr; 
    Solution : Boolean );
Begin
  If y = Edinburgh Then
    Str_Append(s,'<>(')
  Else
    Str_Append(s,'<');
  WriteArgument(y,s,U,Solution);
  If y = Edinburgh Then
    Str_Append(s,')')
  Else
    Str_Append(s,'>')
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
          Str_Append(s,'[]')
        Else
          Str_Append(s,'nil')
      Else
        WriteIdentifier(s,IT,Quotes)
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
        Str_Append(s,'<>')
      Else If GetList(T,T1,T2,Solution) Then { t is t1.t2 }
      Begin
        If ArgList Then 
          Str_Append(s,'(');
        If (y = Edinburgh) And (Not InList) Then
          Str_Append(s,'[');
        WriteTermBis(y,s,T1,False,True,True,Solution);
        If y <> Edinburgh Then { display as dotted list }
        Begin
          Str_Append(s,'.');
          WriteTermBis(y,s,T2,True,False,True,Solution)
        End
        Else If IsNil(T2) Then { t is t1.nil }
        Begin
          Str_Append(s,']')
        End
        Else If IsList(T2,Solution) Then { t = t1.t2 where t2 is a list }
        Begin
          Str_Append(s,',');
          WriteTermBis(y,s,T2,True,False,True,Solution)
        End
        Else { t = t1.t2 where t2 is a not list }
        Begin
          Str_Append(s,'|');
          WriteTermBis(y,s,T2,False,False,True,Solution);
          Str_Append(s,']')
        End;
        If ArgList Then 
          Str_Append(s,')')
      End
      Else { a non-empty tuple that is not a list }
      Begin 
        Th := GetTupleHead(T,Solution);
        If TypeOfTerm(Th) = Identifier Then
        Begin
          WriteIdentifier(s,ITh,Quotes);
          T1 := GetTupleQueue(T,Solution);
          If T1 <> Nil Then { <ident,a,b,...> == ident(a,b,...) }
          Begin
            Str_Append(s,'(');
            WriteArgument(y,s,T1,Solution);
            Str_Append(s, ')')
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
    Str_Append(s,' = ');
  REL_INEQ:
    Str_Append(s,' <> ');
  End;
  WriteTerm(y,s,E^.EQ_RTER,Solution)
End;


{ write equations and inequations in the list E, starting with a comma 
  if Comma is True }
Procedure WriteEquationsBis( y : TSyntax; s : StrPtr; E : EqPtr; 
    Var Comma : Boolean );
Begin
  If Comma Then 
    Str_Append(s,', ');
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
  Str_Append(s,'{ ');
  WriteEquations(PrologIIc,s,E);
  Str_Append(s,' }')
End;

{ write a system; source code only; dead code for now }
Procedure WriteSourceSys( s : StrPtr; Sys : SysPtr );
Begin
  Str_Append(s,'{ ');
  WriteEquations(PrologIIc,s,Sys^.SY_EQUA);
  If (Sys^.SY_EQUA<>Nil) And (Sys^.SY_INEQ<>Nil) Then
    Str_Append(s,', ');
  WriteEquations(PrologIIc,s,Sys^.SY_INEQ);
  Str_Append(s,' }')
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
      Str_Append(s,', ');
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
      Str_Append(s,' ');
    Str_Append(s,'{ ')
  End
End;

{ write constraints variables in list DV; Printed is
  true if a curly brace has already been printed; Before is true if an equation
  or inequation has already been printed; if Curl then curly braces are 
  always printed, even if there are no equations or inequations to print  }
Procedure WriteSolutionBis( y : TSyntax; s : StrPtr; DV : DictPtr;
    Curl, SpaceBeforeCurl : Boolean; 
    Var Printed, Before : Boolean );
Var 
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  If DV <> Nil Then
  Begin
    WriteSolutionBis(y,s,Dict_GetNext(DV),Curl,SpaceBeforeCurl,Printed,Before);
    TV := Dict_GetTerm(DV);
    { equation in the reduced system }
    If VRed(V) <> Nil Then
    Begin
      WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
      If Before Then 
        Str_Append(s,', ');
      WriteVarName(s,V);
      Str_Append(s,' = ');
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
Procedure WriteSolution( y : TSyntax; s : StrPtr; DV : DictPtr;
    Curl, SpaceBeforeCurl : Boolean );
Var
  Printed, Before : Boolean;
Begin
  Printed := False; { no curly brace printed yet }
  Before := False; { no equation printed yet }
  If Curl Then 
    WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
  WriteSolutionBis(y,s,DV,Curl,SpaceBeforeCurl,Printed,Before);
  WriteStackedInequations(y,s,Before);
  If Printed Then
    Str_Append(s,' }')
End;

{ write BTerm B (as part of a rule's queue or goals) }
Procedure WriteOneBTerm( y : TSyntax; s : StrPtr; B : BTermPtr );
Begin
  WriteTerm(y,s,BTerm_GetTerm(B),False)
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
Procedure WriteOneRule( s : StrPtr; R : RulePtr );
Var 
  B : BTermPtr;
  prefix : TString;
  y : TSyntax;
Begin
  y := Rule_GetSyntax(R);
  prefix := CRLF + '        ';
  InitIneq;
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
    Str_Append(s,' ');
    WriteSourceSystem(s,R^.RU_SYST)
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
    WriteSourceSystem(s,Query_GetSys(Q))
  End;
  Str_Append(s,OSyntax[y].QueryEnd);
End;

{ write a list of query }
Procedure WriteQueries( s : StrPtr; Q : QueryPtr );
Begin
  if Q <> Nil Then
  Begin
    WriteOneQuery(s,Q);
    WriteQueries(s,Query_GetNext(Q))
  End
End;

{ write all rules in a program }
Procedure WriteProgramRules( s : StrPtr; P : ProgPtr );
Var
  R : RulePtr;
Begin
  R := FirstRule(P,False);
  While R <> Nil Do
  Begin
    WriteOneRule(s,R);
    R := NextRule(R,False)
  End
End;


{----------------------------------------------------------------------------}
{ output using long strings                                                  }
{----------------------------------------------------------------------------}

{ write a string to stream f or Crt }
Procedure OutString( f : StreamPtr; s : StrPtr );
Begin
  If (f = Nil) Or StreamIsConsole(f) Then
    Str_CWrite(s)
  Else
    Str_Write(f,s)
End;

{ write a string followed by a carriage return; do not alter the
  string passed as parameter }
Procedure OutStringCR( f : StreamPtr; s : StrPtr );
Begin
  OutString(f,s);
  OutString(f,Str_NewFromString(CRLF))
End;

{ write a carriage return }
Procedure OutCR( f : StreamPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  OutStringCR(f,s)
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

Procedure OutSolution( f : StreamPtr; y : TSyntax; DV : DictPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteSolution(y,s,DV,True,False);
  OutString(f,s)
End;

{ output the reduced system for the variables in the current query }
Procedure OutQuerySolution( f : StreamPtr; Q : QueryPtr );
Begin
  InitIneq;
  OutSolution(f,Query_GetSyntax(Q),Query_GetDict(Q))
End;

{ output a term that is not an argument of a predicate }
Procedure OutTermBis( f : StreamPtr; y : TSyntax; T : TermPtr; 
    ArgList,Quotes : Boolean );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteTermBis(y,s,T,False,ArgList,Quotes,True);
  OutString(f,s)
End;

{ output a term }
Procedure OutTerm( f : StreamPtr; y : TSyntax; T : TermPtr );
Begin
  OutTermBis(f,y,T,False,True)
End;

Procedure OutTerms( f : StreamPtr; y : TSyntax; B : BTermPtr; sep : TString );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteTerms(y,s,B,sep);
  OutString(f,s)
End;

Procedure OutOneRule( f : StreamPtr; R : RulePtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneRule(s,R);
  OutStringCR(f,s)
End;

Procedure OutOneQuery( f : StreamPtr; Q : QueryPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneQuery(s,Q);
  OutStringCR(f,s)
End;

Procedure OutOneComment( f : StreamPtr; C : CommPtr );
Var 
  s : StrPtr;
Begin
  s := Str_New;
  WriteOneComment(s,C);
  OutStringCR(f,s)
End;

End.