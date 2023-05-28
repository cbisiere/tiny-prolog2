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


{ return a constant as a string, with or without quotes }
Function GetConstAsString( C : ConstPtr; Quotes : Boolean ) : StrPtr;
Var 
  quoted : Boolean; { does the constant need to be quoted? }
  s : StrPtr;
Begin
  quoted := Quotes And (ConstType(C)=QString);
  s := NewString;
  If quoted Then
    StrAppend(s,'"');
  StrConcat(s,ConstGetStr(C));
  If quoted Then
    StrAppend(s,'"');
  GetConstAsString := s
End;


{ return the name of a user or temporary variable }
Function GetVarNameAsString( V : VarPtr ) : StrPtr;
Var 
  TV : TermPtr Absolute V;
  PV : TPObjPtr Absolute V;
  s : StrPtr;
  k : Integer;
  ks : AnyStr;
Begin
  CheckCondition(TypeOfTerm(TV)=Variable,
    'GetVarNameAsString(V): V is not a variable');
  s := StrClone(V^.TV_DVAR^.DE_STRI);
  k := ObjectCopyNumber(PV);
  If k > 0 Then
  Begin
    Str(k,ks);
    StrAppend(s, '_' + ks) { FIXME: make sure it is an invalid variable name }
  End;
  GetVarNameAsString := s
End;


Procedure WriteTermBis( s : StrPtr; T : TermPtr; ArgList,Quotes,Solution : Boolean); Forward;

{ write a comma-separated list of arguments, coded as F(arg1,F(arg2,Nil)) }
Procedure WriteArgument( s : StrPtr; F : FuncPtr; Solution : Boolean );
Var 
  T : TermPtr;
  FT : FuncPtr Absolute T;
Begin
  WriteTermBis(s,FLeftArg(F),False,True,Solution);
  If FRightArg(F) <> Nil Then
  Begin
    StrAppend(s,',');
    CheckCondition((FRightArg(F)=Nil) Or (TypeOfTerm(FRightArg(F))=FuncSymbol),
      'broken argument list');
    T := FRightArg(F);
    WriteArgument(s,FT,Solution)
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
Procedure WriteTuple( s : StrPtr; F : FuncPtr; Solution : Boolean );
Begin
  StrAppend(s,'<');
  WriteArgument(s,F,Solution);
  StrAppend(s,'>')
End;

{ write a term, possibly as an argument of a predicate, and with quotes; 
  if Solution is true, we are currently printing a solution (that is, a 
  reduced system), and not the item as given in the source code ) }
Procedure WriteTermBis; (* ( s : StrPtr; T : TermPtr; ArgList,Quotes,Solution : Boolean ); *)
Var
  { all casts of T: }
  CT : ConstPtr Absolute T;
  VT : VarPtr Absolute T;
  IT : IdPtr Absolute T;
  FT : FuncPtr Absolute T;
  PT : TPObjPtr Absolute T;
  { others: }
  F : FuncPtr;
  LeftT : TermPtr;
  ILeftT : IdPtr Absolute LeftT;
  T1,T2 : TermPtr;
  FT1 : FuncPtr Absolute T1;
  FT2 : FuncPtr Absolute T2;
Begin
  Case TypeOfTerm(T) Of
  Constant:
    Begin
      WriteConst(s,CT,Quotes)
    End;
  Identifier: { isolated identifier, e.g., hello -> world }
    Begin
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
            WriteTermBis(s,VRed(VT),False,Quotes,Solution)
          End;
        If WatchIneq(VT) <> Nil Then
          AddIneq(WatchIneq(VT))
      End
    End;
  FuncSymbol:
    Begin
      F := FT;
      LeftT := FLeftArg(F);
      If (TypeOfTerm(LeftT) = Identifier) Then
      Begin
        If IdentifierEqualTo(ILeftT,'.') Then { F(.,F(a,F(b,Nil))) => a.b }
        Begin
          If ArgList Then 
            StrAppend(s,'(');
          T1 := FRightArg(F);
          WriteTermBis(s,FLeftArg(FT1),True,True,Solution);
          StrAppend(s,'.');
          T2 := FRightArg(FT1); { TODO: check type }
          WriteTermBis(s,FLeftArg(FT2),False,True,Solution);
          If ArgList Then 
            StrAppend(s,')')
        End
        Else
        Begin 
          WriteIdentifier(s,ILeftT);
          If FRightArg(F)<>Nil Then { F(name,F(a,F(b,Nil) => name(a,b) }
          Begin
            T1 := FRightArg(F);
            StrAppend(s,'(');
            WriteArgument(s,FT1,Solution); { TODO: check type }
            StrAppend(s, ')')
          End
        End
      End
      Else { F(a,F(b,Nil) => <a,b> where a not an identifier }
      Begin
        WriteTuple(s,F,Solution)
      End
    End
  End
End;

Procedure WriteTerm( s : StrPtr; T : TermPtr; Solution : Boolean ); Forward;

{ write a single equation or inequation }
Procedure WriteOneEquation( s : StrPtr; E : EqPtr; Solution : Boolean  );
Begin
  WriteTerm(s,E^.EQ_LTER,Solution);
  Case E^.EQ_TYPE Of
  REL_EQUA:
    StrAppend(s,' = ');
  REL_INEQ:
    StrAppend(s,' <> ');
  End;
  WriteTerm(s,E^.EQ_RTER,Solution)
End;


{ write equations and inequations in the list E, starting with a comma 
  if Comma is True }
Procedure WriteEquationsBis( s : StrPtr; E : EqPtr; Var Comma : Boolean );
Begin
  If Comma Then 
    StrAppend(s,', ');
  Comma := True;
  WriteOneEquation(s,E,False); { not part of a solution }
  If E^.EQ_NEXT <> Nil Then
    WriteEquationsBis(s,E^.EQ_NEXT,Comma)
End;

{ write a list of equations }
Procedure WriteEquations( s : StrPtr; E : EqPtr );
Var Comma : Boolean;
Begin
  Comma := False;
  WriteEquationsBis(s,E,Comma)
End;

{ write a list of equations or inequations; source code only }
Procedure WriteSourceSystem( s : StrPtr; E : EqPtr );
Begin
  StrAppend(s,'{ ');
  WriteEquations(s,E);
  StrAppend(s,' }')
End;

{ write a system; source code only; dead code for now }
Procedure WriteSourceSys( s : StrPtr; Sys : SysPtr );
Begin
  StrAppend(s,'{ ');
  WriteEquations(s,Sys^.SY_EQUA);
  If (Sys^.SY_EQUA<>Nil) And (Sys^.SY_INEQ<>Nil) Then
    StrAppend(s,', ');
  WriteEquations(s,Sys^.SY_INEQ);
  StrAppend(s,' }')
End;

{ write all inequations in the stack }
Procedure WriteStackedInequations( s : StrPtr; Var Before : Boolean );
Var I,K : Integer;
Begin
  K := IdxIneq;
  For I := K DownTo 1 Do 
  Begin
    If Before Then
      StrAppend(s,', ');
    WriteOneEquation(s,Ineq[I],True);
    Before := True
  End
End;

{ write a term that is not an argument of a predicate }
Procedure WriteTerm; (* ( s : StrPtr; T : TermPtr; Solution : Boolean ); *)
Begin
  WriteTermBis(s,T,False,True,Solution)
End;

{ print an opening curly brace if it has not been printed yet }
Procedure WriteCurlyBrace( s : StrPtr; SpaceBeforeCurl : Boolean; Var Printed : Boolean );
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
Procedure WriteSolutionBis( s : StrPtr; DV1,DV2 : DictPtr;
    Curl, SpaceBeforeCurl : Boolean; 
    Var Printed, Before : Boolean );
Var 
  V : VarPtr;
  TV : TermPtr Absolute V;
Begin
  If (DV1<>Nil) And (DV1<>DV2) Then
  Begin
    WriteSolutionBis(s,DV1^.DE_NEXT,DV2,Curl,SpaceBeforeCurl,Printed,Before);
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
      WriteTerm(s,VRed(V),True)
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
Procedure WriteSolution( s : StrPtr; start, stop : DictPtr;
    Curl, SpaceBeforeCurl : Boolean );
Var
  Printed, Before : Boolean;
Begin
  Printed := False; { no curly brace printed yet }
  Before := False; { no equation printed yet }
  If Curl Then 
    WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
  WriteSolutionBis(s,start,stop,Curl,SpaceBeforeCurl,Printed,Before);
  WriteStackedInequations(s,Before);
  If Printed Then
    StrAppend(s,' }')
End;

{ write BTerm B }
Procedure WriteOneBTerm( s : StrPtr; B : BTermPtr );
Begin
  WriteTerm(s,B^.BT_TERM,False)
End;

{ write a list of BTerms }
Procedure WriteTerms( s : StrPtr; B : BTermPtr; sep : AnyStr );
Var 
  First : Boolean;
  Procedure DoWriteTerms( B : BTermPtr );
  Begin
    If B <> Nil Then
    Begin
      If Not First Then
        StrAppend(s,sep);
      WriteOneBTerm(s,B);
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
  prefix : AnyStr;
Begin
  prefix := CRLF + '        ';
  InitIneq;
  B := R^.RU_FBTR;
  WriteOneBTerm(s,B);
  StrAppend(s,' ->');
  if B^.BT_NEXT <> Nil Then
    StrAppend(s,prefix);
  WriteTerms(s,B^.BT_NEXT,prefix);
  If R^.RU_SYST <> Nil Then
  Begin
    StrAppend(s,' ');
    WriteSourceSystem(s,R^.RU_SYST)
  End;
  StrAppend(s,';')
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

{ write the rules that can be used to clear a query (scoping) }
Procedure WriteQuestionRules( s : StrPtr; Q : QueryPtr; RuleType : RuType );
Var
  R : RulePtr;
  Stop : Boolean;
Begin
  R := Q^.QU_FRUL;
  Stop := R = Nil;
  While Not Stop Do
  Begin
    If R^.RU_TYPE = RuleType Then
    Begin
      WriteOneRule(s,R);
      StrAppendCR(s)
    End;
    Stop := R = Q^.QU_LRUL;
    R := NextRule(R);
    Stop := Stop Or (R = Nil)
  End
End;

{ write a query }
Procedure WriteOneQuery( s : StrPtr; Q : QueryPtr );
Begin
  StrAppend(s,'->');
  WriteTerms(s,Q^.QU_FBTR,' ');
  If Q^.QU_SYST <> Nil Then
  Begin
    StrAppend(s,' ');
    WriteSourceSystem(s,Q^.QU_SYST)
  End;
  StrAppend(s,';')
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
  WriteQueries(s,P^.PP_FQRY)
End;


{ write all rules in a program }
Procedure WriteProgramRules( s : StrPtr; P : ProgPtr );
Begin
  WriteRules(s,P^.PP_FRUL)
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

Procedure OutOneEquation( E : EqPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneEquation(s,E,False); { source code }
  OutString(s,UseIOStack)
End;

Procedure OutSolution( start,stop : DictPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteSolution(s,start,stop,True,False);
  OutString(s,UseIOStack)
End;

{ output the reduced system for the variables in the current query }
Procedure OutQuerySolution( Q : QueryPtr; UseIOStack : Boolean );
Begin
  InitIneq;
  OutSolution(Q^.QU_FVAR,Q^.QU_LVAR,UseIOStack)
End;

{ output a term that is not an argument of a predicate }
Procedure OutTermBis( T : TermPtr; ArgList,Quotes : Boolean; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteTermBis(s,T,ArgList,Quotes,True);
  OutString(s,UseIOStack)
End;

{ output a term }
Procedure OutTerm( T : TermPtr; UseIOStack : Boolean );
Begin
  OutTermBis(T,False,True,UseIOStack)
End;

Procedure OutTerms( B : BTermPtr; sep : AnyStr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteTerms(s,B,sep);
  OutString(s,UseIOStack)
End;

Procedure OutOneRule( R : RulePtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneRule(s,R);
  OutStringCR(s,UseIOStack)
End;

Procedure OutQuestionRules( Q : QueryPtr; RuleType : RuType; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteQuestionRules(s,Q,RuleType);
  OutString(s,UseIOStack)
End;

Procedure OutOneQuery( Q : QueryPtr; UseIOStack : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneQuery(s,Q);
  OutStringCR(s,UseIOStack)
End;
