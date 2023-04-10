{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Unparse.pas                                                }
{   Author      : Christophe Bisière                                         }
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
    Found := Ineq[I] = E;
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
  s := StrClone(V^.TV_DVAR^.DV_NAME);
  k := PObjectCopyNumber(PV);
  If k > 0 Then
  Begin
    Str(k,ks);
    StrAppend(s, '_' + ks) { FIXME: make sure it is an invalid variable name }
  End;
  GetVarNameAsString := s
End;


Procedure WriteTermBis( s : StrPtr; T : TermPtr; ArgList,Quotes : Boolean); Forward;

{ write a comma-separated list of arguments, coded as F(arg1,F(arg2,Nil)) }
Procedure WriteArgument( s : StrPtr; F : FuncPtr );
Var 
  T : TermPtr;
  FT : FuncPtr Absolute T;
Begin
  WriteTermBis(s,F^.TF_LTER,False,True);
  If F^.TF_RTER <> Nil Then
  Begin
    StrAppend(s,',');
    CheckCondition((F^.TF_RTER=Nil) Or (TypeOfTerm(F^.TF_RTER)=FuncSymbol),
      'broken argument list');
    T := F^.TF_RTER;
    WriteArgument(s,FT)
  End
End;

{ write the name of a user or temporary variable }
Procedure WriteVarName( s : StrPtr; V : VarPtr );
Begin
  StrConcat(s,GetVarNameAsString(V))
End;

{ write a constant }
Procedure WriteConst( s : StrPtr; C : ConstPtr );
Begin
  StrConcat(s,GetConstAsString(C,True))
End;

{ return the constant the term T is equal to, of Nil if T is not equal to a 
  constant }
Function EvaluateToConstant( T : TermPtr ) : ConstPtr;
Var 
  C : ConstPtr;
  CT : ConstPtr Absolute T;
  VT : VarPtr Absolute T;
Begin
  C := Nil;
  Case TypeOfTerm(T) Of
  Constant :
    C := CT;
  Variable :
    If VT^.TV_TRED <> Nil Then
      C := EvaluateToConstant(VT^.TV_TRED)
  End;
  EvaluateToConstant := C
End;

{ write a tuple }
Procedure WriteTuple( s : StrPtr; F : FuncPtr );
Begin
  StrAppend(s,'<');
  WriteArgument(s,F);
  StrAppend(s,'>')
End;

{ write a term, possibly as an argument of a predicate, and with quotes }
Procedure WriteTermBis; (* ( s : StrPtr; T : TermPtr; ArgList,Quotes : Boolean ); *)
Var
  V : VarPtr;
  F : FuncPtr;
  C : ConstPtr;
  LeftT : TermPtr;
  CT : ConstPtr Absolute T;
  VT : VarPtr Absolute T;
  FT : FuncPtr Absolute T;
  PV : TPObjPtr Absolute V;
  CLeftT : ConstPtr Absolute LeftT;
  T1,T2 : TermPtr;
  FT1 : FuncPtr Absolute T1;
  FT2 : FuncPtr Absolute T2;
Begin
  Case TypeOfTerm(T) Of
  Constant :
    Begin
      C := CT;
      StrConcat(s,GetConstAsString(C,Quotes))
    End;
  Variable  :
    Begin
      V := VT;
      If PObjectCopyNumber(PV) = 0 Then
      Begin
        WriteVarName(s,V)
      End
      Else
        If V^.TV_TRED = Nil Then
          WriteVarName(s,V)
        Else
        Begin
          WriteTermBis(s,V^.TV_TRED,False,Quotes)
        End;
      If V^.TV_FWAT <> Nil Then
        AddIneq(V^.TV_FWAT)
    End;
  FuncSymbol  :
    Begin
      F := FT;
      LeftT := F^.TF_LTER;
      If (TypeOfTerm(LeftT) = Constant) Then
      Begin
        If ConstStartWith(CLeftT,Digits) Then { should only happen when printing subtrees during debugging}
        Begin
          WriteTuple(s,F)
        End
        Else If ConstEqualTo(CLeftT,'.') Then { F(.,F(a,F(b,Nil))) => a.b }
        Begin
          If ArgList Then 
            StrAppend(s,'(');
          T1 := F^.TF_RTER;
          WriteTermBis(s,FT1^.TF_LTER,True,True);
          StrAppend(s,'.');
          T2 := FT1^.TF_RTER; { TODO: check type }
          WriteTermBis(s,FT2^.TF_LTER,False,True);
          If ArgList Then 
            StrAppend(s,')')
        End
        Else
        Begin 
          StrConcat(s,ConstGetStr(CLeftT));
          If F^.TF_RTER<>Nil Then { F(name,F(a,F(b,Nil) => name(a,b) }
          Begin
            T1 := F^.TF_RTER;
            StrAppend(s,'(');
            WriteArgument(s,FT1); { TODO: check type }
            StrAppend(s, ')')
          End
        End
      End
      Else { F(a,F(b,Nil) => <a,b> where a not a constant }
      Begin
        WriteTuple(s,F)
      End
    End
  End
End;

Procedure WriteTerm( s : StrPtr; T : TermPtr ); Forward;

{ write a single equation or inequation }
Procedure WriteOneEquation( s : StrPtr; E : EqPtr );
Begin
  WriteTerm(s,E^.EQ_LTER);
  Case E^.EQ_TYPE Of
  REL_EQUA:
    StrAppend(s,' = ');
  REL_INEQ:
    StrAppend(s,' <> ');
  End;
  WriteTerm(s,E^.EQ_RTER)
End;


{ write equations in the list E, starting with a comma if Comma is True }
Procedure WriteEquationsBis( s : StrPtr; E : EqPtr; Var Comma : Boolean );
Begin
  If E <> Nil Then
  Begin
    WriteEquationsBis(s,E^.EQ_NEXT, Comma);
    If Comma Then 
      StrAppend(s,', ');
    Comma := True;
    WriteOneEquation(s,E)
  End
End;

{ write a list of equations }
Procedure WriteEquations( s : StrPtr; E : EqPtr );
Var Comma : Boolean;
Begin
  Comma := False;
  WriteEquationsBis(s,E,Comma)
End;

{ write a system }
Procedure WriteSys( s : StrPtr; Sys : SysPtr );
Begin
  StrAppend(s,'{ ');
  WriteEquations(s,Sys^.SY_EQUA);
  If (Sys^.SY_EQUA<>Nil) And (Sys^.SY_INEQ<>Nil) Then
    StrAppend(s,', ');
  WriteEquations(s,Sys^.SY_INEQ);
  StrAppend(s,' }')
End;

{ write all inequations in the stack }
Procedure WriteInequations( s : StrPtr; Var Before : Boolean );
Var I,K : Integer;
Begin
  K := IdxIneq;
  For I := K DownTo 1 Do 
  Begin
    If Before Then
      StrAppend(s,', ');
    WriteOneEquation(s,Ineq[I]);
    Before := True
  End
End;

{ write a term that is not an argument of a predicate }
Procedure WriteTerm; (* ( s : StrPtr; T : TermPtr ); *)
Begin
  WriteTermBis(s,T,False,True)
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
Procedure WriteSystemBis( s : StrPtr; DV1,DV2 : DictVarPtr; 
    Curl, SpaceBeforeCurl : Boolean; 
    Var Printed, Before : Boolean );
Var V : VarPtr;
Begin
  If (DV1<>Nil) And (DV1<>DV2) Then
  Begin
    WriteSystemBis(s,DV1^.DV_NEXT,DV2,Curl,SpaceBeforeCurl,Printed,Before);
    V := DV1^.DV_PVAR;
    { equation in the reduced system }
    If V^.TV_TRED <> Nil Then
    Begin
      WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
      If Before Then 
        StrAppend(s,', ');
      WriteVarName(s,V);
      StrAppend(s,' = ');
      Before := True;
      WriteTerm(s,V^.TV_TRED)
    End;
    { inequations in the reduced system }
    If V^.TV_FWAT <> Nil Then
    Begin
      WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
      AddIneq(V^.TV_FWAT)
    End
  End
End;

Procedure WriteSystem( s : StrPtr; start, stop : DictVarPtr; 
    Curl, SpaceBeforeCurl : Boolean );
Var
  Printed, Before : Boolean;
Begin
  Printed := False;
  Before := False;
  If Curl Then 
    WriteCurlyBrace(s,SpaceBeforeCurl,Printed);
  WriteSystemBis(s,start,stop,Curl,SpaceBeforeCurl,Printed,Before);
  WriteInequations(s,Before);
  If Printed Then
    StrAppend(s,' }')
End;

{ write BTerm B }
Procedure WriteOneBTerm( s : StrPtr; B : BTermPtr );
Begin
  WriteTerm(s,B^.BT_TERM)
End;

{ write a list of BTerms }
Procedure WriteTerms( s : StrPtr; B : BTermPtr; CR : Boolean);
Begin
  If B <> Nil Then
  Begin
    If CR Then
    Begin
      StrAppend(s,Chr(13)+Chr(10));
      StrAppend(s,'        ')
    End
    Else
      StrAppend(s,' ');
    WriteOneBTerm(s,B);
    WriteTerms(s,B^.BT_NEXT,CR)
  End
End;

{ write a single rule }
Procedure WriteOneRule( s : StrPtr; R : RulePtr );
Var B : BTermPtr;
Begin
  InitIneq;
  B := R^.RU_FBTR;
  WriteOneBTerm(s,B);
  StrAppend(s,' ->');
  WriteTerms(s,B^.BT_NEXT,True);
  WriteSystem(s,R^.RU_FVAR,R^.RU_LVAR,False,True);
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
      StrAppend(s,Chr(13)+Chr(10))
    End;
    Stop := R = Q^.QU_LRUL;
    R := NextRule(R);
    Stop := Stop Or (R = Nil)
  End
End;

{ write a query }
Procedure WriteOneQuery( s : StrPtr; Q : QueryPtr );
Begin
  InitIneq;
  StrAppend(s,'->');
  WriteTerms(s,Q^.QU_FBTR,False);
  WriteSystem(s,Q^.QU_FVAR,Q^.QU_LVAR,False,True);
  StrAppend(s,';')
End;

{ write a list of query }
Procedure WriteQueries( s : StrPtr; Q : QueryPtr );
Begin
  if Q <> Nil Then
  Begin
    WriteOneQuery(s,Q);
    WriteQueries(s,Q^.QU_NEXT)
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

Procedure OutConst( C : ConstPtr );
Var s : StrPtr;
Begin
  s := NewString;
  WriteConst(s,C);
  StrWrite(s)
End;

Procedure OutVarName( V : VarPtr );
Var s : StrPtr;
Begin
  s := NewString;
  WriteVarName(s,V);
  StrWrite(s)
End;

Procedure OutOneEquation( E : EqPtr );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneEquation(s,E);
  StrWrite(s)
End;

Procedure OutSystem( start,stop : DictVarPtr; Curl : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteSystem(s,start,stop,Curl,False);
  StrWrite(s)
End;

{ output a term that is not an argument of a predicate }
Procedure OutTermBis( T : TermPtr; ArgList, Quotes : Boolean );
Var s : StrPtr;
Begin
  s := NewString;
  WriteTermBis(s,T,ArgList,Quotes);
  StrWrite(s)
End;

{ output a term }
Procedure OutTerm( T : TermPtr );
Begin
  OutTermBis(T,False,True)
End;

Procedure OutTerms( B : BTermPtr; CR : Boolean);
Var s : StrPtr;
Begin
  s := NewString;
  WriteTerms(s,B,CR);
  StrWrite(s)
End;

Procedure OutOneRule( R : RulePtr );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneRule(s,R);
  StrWriteln(s)
End;

Procedure OutQuestionRules( Q : QueryPtr; RuleType : RuType );
Var s : StrPtr;
Begin
  s := NewString;
  WriteQuestionRules(s,Q,RuleType);
  StrWrite(s)
End;

Procedure OutOneQuery( Q : QueryPtr );
Var s : StrPtr;
Begin
  s := NewString;
  WriteOneQuery(s,Q);
  StrWriteln(s)
End;
