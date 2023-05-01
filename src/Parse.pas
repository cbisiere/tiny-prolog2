{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Parse.pas                                                  }
{   Author      : Christophe Bisière                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{                              P A R S I N G                                 }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{-----------------------------------------------------------------------}
{                                                                       }
{   The functional symbol F is used encode predicates as follows:       }
{                                                                       }
{           F                                                           }
{          / \               (1) tuple: the name of the predicate is    }
{       name  F                  the first argument                     }
{            / \                                                        }
{         Arg1  F            (2) of course Arg1 .. ArgN can also be     }
{              / \               predicates.                            }
{           Arg2   ...                                                  }
{                   \        (3) A '.' (Prolog list) is considered as   }
{                    F           a regular predicate.                   }
{                  /  \                                                 }
{               ArgN    Nil                                             }
{                                                                       }
{-----------------------------------------------------------------------}

{ read a term, possibly accepting a cut as a valid term }
Function ReadOneTerm( P : ProgPtr; Cut : Boolean) : TermPtr; Forward;

{ read the argument of a predicate (EndChar=')') or a tuple (EndChar='>') }
Function GetArgument( P : ProgPtr; EndChar : Char ) : FuncPtr;
Var
  T : TermPtr;
  c : Char;
  F : FuncPtr;
  TF : TermPtr Absolute F;
Begin
  T := ReadOneTerm(P,False);
  c := GetCharNb(c);
  If c =  ',' Then
  Begin
    F := GetArgument(P,EndChar);
    GetArgument := NewSymbol(T,TF)
  End
  Else
  If c = EndChar  Then
    GetArgument := NewSymbol(T,Nil)
  Else
  Begin
    RaiseError('"' + EndChar + '" expected');
    GetArgument := Nil
  End
End;

{ read and return a constant string, not including the double quotes }
Function ReadString : StrPtr;
Var
  c : Char;
  Ch : StrPtr;
  Done : Boolean;
Begin
  CheckCondition(NextChar(c) = '"', 'string expected');
  c := GetChar(c); { discard it, since double quotes are not part of the string }
  Ch := NewString;
  Repeat
    Done := False;
    GetCharUntil(Ch,['"',EndOfLine,EndOfInput]);
    Case NextChar(c) Of
    '"':
      Begin
        c := GetChar(c); { discard it }
        If NextChar(c) = '"' Then { doubled: keep only one }
          StrAppendChar(Ch,GetChar(c))
        Else
          Done := True
      End;
    EndOfLine:
      If StrEndsWith(Ch,['\']) Then { string continuation on next line }
      Begin
        c := GetChar(c);
        StrDeleteLastChar(Ch)
      End
      Else
        RaiseError('End of line while reading a constant string');
    EndOfInput:
      RaiseError('End of input while reading a constant string')
    End
  Until Error or Done;
  ReadString := Ch
End;

{ read a term, possibly accepting a cut as a valid term }
Function ReadOneTerm; (* ( P : ProgPtr; Cut : Boolean ) : TermPtr *)
Var
  T : TermPtr;
  C : ConstPtr;
  TC : TermPtr Absolute C;
  F : FuncPtr;
  TF : TermPtr Absolute F;
  V : VarPtr;
  TV : TermPtr Absolute V;
  I : IdPtr;
  TI : TermPtr Absolute I;
  F2 : FuncPtr;
  TF2 : TermPtr Absolute F2;
  F3 : FuncPtr;
  TF3 : TermPtr Absolute F3;
  Ch  : StrPtr;
  c1,c2 : Char;
  n : LongInt;
Begin
  Spaces;
  Ch := NewString;
  n := GrabLetters(Ch);
  If n=0 Then
  Begin
    c1 := NextChar(c1);
    If (c1 In Digits) Or 
      (c1='-') And (NextNextChar(c2) in Digits) Then  { an integer }
    Begin
      If c1='-' Then
        StrAppendChar(Ch,GetChar(c1));
      n := GetCharWhile(Ch,Digits);
      C := InstallConst(P^.PP_DCON,Ch,CN);
      T := TC
    End
    Else
    If c1 ='(' Then          { ( <term> ) }
    Begin
      c1 := GetChar(c1);
      T := ReadOneTerm(P,False);
      Verify(')')
    End
    Else
    If c1 = '<' Then        { a tuple }
    Begin
      c1 := GetChar(c1);
      If NextChar(c1)='>' Then
      Begin
        { empty tuple "<>" }
        c1 := GetChar(c1);
        F := NewSymbol(Nil,Nil)
      End
      Else
        F := GetArgument(P,'>');
      T := TF
    End
    Else
    If c1 = '"' Then        { a string }
    Begin
      Ch := ReadString;
      C := InstallConst(P^.PP_DCON,Ch,CS);
      T := TC
    End
    Else
    If Cut and (c1 In ['!','/']) Then    { the "cut" }
    Begin
      c1 := GetChar(c1);
      I := InstallIdentifier(P^.PP_DIDE,NewStringFrom('!'));
      T := TI
    End
    Else
      RaiseError('term expected')
  End
  Else if n=1 Then
  Begin                      { a variable }
    { ["-"<letters>]* }
    While (NextChar(c1)='-') And IsLetter(NextNextChar(c2)) Do
    Begin
      StrAppendChar(Ch,GetChar(c1)); { read the '-' }
      n := GrabLetters(Ch);
    End;
    { [<digits>]["'"]* }
    n := GetCharWhile(Ch,Digits);
    n := GetCharWhile(Ch,['''']);
    V := InstallVariable(P^.PP_DVAR,P^.PP_LVAR,Ch);
    T := TV;
  End
  Else { at least 2 letters: an identifier }
  Begin
    n := GrabLetters(Ch);
    { ["-"<letters>]* }
    While (NextChar(c1)='-') And IsLetter(NextNextChar(c2)) Do
    Begin
      StrAppendChar(Ch,GetChar(c1)); { read the '-' }
      n := GrabLetters(Ch);
    End;
    { [<digits>] }
    n := GetCharWhile(Ch,Digits);
    If NextChar(c1) = '(' Then { a predicate }
    Begin
      c1 := GetChar(c1);
      I := InstallIdentifier(P^.PP_DIDE,Ch);
      { predicate's argument }
      F := GetArgument(P,')');
      F2 := NewSymbol(TI,TF);
      T := TF2
    End
    Else
    Begin { an identifier that is not a predicate }
      I := InstallIdentifier(P^.PP_DIDE,Ch);
      T := TI
    End
  End;
  c1 := NextCharNb(c1);
  If c1 = '.' Then    { a list element }
  Begin
    c1 := GetChar(c1);
    I := InstallIdentifier(P^.PP_DIDE,NewStringFrom('.'));
    F := NewSymbol(ReadOneTerm(P,False),Nil); { q: new term }
    F2 := NewSymbol(T,TF); { t: term read above }
    F3 := NewSymbol(TI,TF2); { t.q }
    T := TF3
  End;
  ReadOneTerm := T
End;

{ read an equations or a inequation }
Function ReadEquation( P : ProgPtr ) : EqPtr;
Var
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
  c : Char;
Begin
  E := Nil;
  T1 := ReadOneTerm(P,False);
  If Not Error Then
  Begin
    c := GetCharNb(c);
    Case c Of
    '=' :
      Code := REL_EQUA;
    '<' :
      Begin
        Verify('>');
        Code := REL_INEQ
      End;
    Else
      RaiseError('= or <> expected')
    End
  End;
  If Not Error Then
    T2 := ReadOneTerm(P,False);  { right term }
  If Not Error Then
    E := NewEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations }
Function ReadSystem( P : ProgPtr ) : EqPtr;
Var
  E, FirstE, PrevE : EqPtr;
  First : Boolean;
  c     : Char;
Begin
  FirstE := Nil;
  PrevE := Nil;
  Verify('{');
  If Not Error Then
  Begin
    First := True;
    Repeat
      E := ReadEquation(P);
      If First Then
      Begin
        FirstE := E;
        First := False
      End
      Else
        PrevE^.EQ_NEXT := E;
      PrevE := E
    Until (Error) Or (GetCharNb(c) <> ',');
    If (Not Error) And (c <> '}') Then RaiseError('Missing }')
  End;
  ReadSystem := FirstE
End;

{ compile a system of equations and inequations }
Function CompileSystem( P : ProgPtr ) : EqPtr;
Begin
  CompileSystem := ReadSystem(P)
End;

{ compile a term }
Function CompileOneTerm( P : ProgPtr; Cut : Boolean ) : BTermPtr;
Var B : BTermPtr;
Begin
  B := NewBTerm;
  With B^ Do
  Begin
    BT_TERM := ReadOneTerm(P,Cut);
    BT_ACCE := AccessIdentifier(BT_TERM)
  End;
  CompileOneTerm := B
End;

{ compile a sequence of terms, stopping at a char in StopChars  }
Function CompileTerms( P : ProgPtr; StopChars : CharSet ) : BTermPtr;
Var
  B : BTermPtr;
  c : Char;
Begin
  c := NextCharNb(c);
  If (Not (c In StopChars)) And (Not Error) Then
  Begin
    B := NewBTerm;
    With B^ Do
    Begin
      BT_TERM := ReadOneTerm(P,True);
      BT_ACCE := AccessIdentifier(BT_TERM);
      BT_NEXT := CompileTerms(P,StopChars)
    End
  End
  Else
    B := Nil;
  CompileTerms := B
End;

{ set up local variable context to prepare compiling a new rule }
Procedure OpenLocalContextForRule( P : ProgPtr; R : RulePtr );
Begin
  P^.PP_LVAR := P^.PP_DVAR;
  R^.RU_LVAR := P^.PP_DVAR
End;

{ close this local variable context }
Procedure CloseLocalContextForRule( P : ProgPtr; R : RulePtr );
Begin
  R^.RU_FVAR := P^.PP_DVAR
End;

{ compile a rule; note that the system cannot be reduced right away
  as the reduction may depends on global assignments }
Procedure CompileOneRule( P : ProgPtr; R : RulePtr );
Var 
  B : BTermPtr;
  c : Char;
Begin
  OpenLocalContextForRule(P,R);
  Spaces;
  With R^ Do
  Begin
    RU_SYST := Nil;
    B := CompileOneTerm(P,False); { head }
    RU_FBTR := B;
    Verify('->');
    If Not Error Then
    Begin
      B^.BT_NEXT := CompileTerms(P,['{',';',EndOfInput]);
      c := NextCharNb(c);
      If c = '{' Then
        RU_SYST := CompileSystem(P);
      Verify(';')
    End
  End;
  CloseLocalContextForRule(P,R)
End;

{ compile a sequence of rules, stopping at a char in StopChars  }
Function CompileRules( P : ProgPtr; StopChars : CharSet; RuleType : RuType ) : RulePtr;
Var
  R : RulePtr;
  c : Char;
Begin
  c := NextCharNb(c);
  If (Not (c In StopChars)) And (Not Error) Then
  Begin
    R := NewRule(RuleType);
    CompileOneRule(P,R);
    R^.RU_NEXT := CompileRules(P,StopChars,RuleType)
  End
  Else
    R := Nil;
  CompileRules := R
End;

{ set up local variable context to prepare compiling a new query }
Procedure OpenLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  P^.PP_LVAR := P^.PP_DVAR;
  Q^.QU_LVAR := P^.PP_DVAR
End;

{ close this local variable context }
Procedure CloseLocalContextForQuery( P : ProgPtr; Q : QueryPtr );
Begin
  Q^.QU_FVAR := P^.PP_DVAR
End;

{ compile a query, including a system of equations and equations if any;
  note that this system cannot be reduced right away, as its 
  solvability may depend on global variables }
Procedure CompileOneQuery( P : ProgPtr; Q : QueryPtr );
Var
  c : Char;
Begin
  OpenLocalContextForQuery(P,Q);
  Spaces;
  With Q^ Do
  Begin
    QU_FRUL := Nil;
    QU_LRUL := Nil;
    QU_FBTR := CompileTerms(P,['{',';',EndOfInput]);
    QU_SYST := Nil;
    c := NextCharNb(c);
    If c = '{' Then
      QU_SYST := CompileSystem(P);
    Verify(';')
  End;
  CloseLocalContextForQuery(P,Q) 
End;

{ compile a sequence of queries; if ContChar is not empty, each query 
  must start with a char in this set; the sequence ends with a char 
  in StopChars }
Function CompileQueries( P : ProgPtr; WithArrow : Boolean;
  ContChar, StopChars : CharSet ) : QueryPtr;
Var
  Q : QueryPtr;
  ptr : TPObjPtr Absolute Q;
  c : Char;
Begin
  Q := Nil;
  c := NextCharNb(c);
  If ((ContChar=[]) Or (c In ContChar))
    And (Not (c In StopChars)) And (Not Error) Then
  Begin
    If WithArrow Then
      Verify('->');
    ptr := NewPrologObject(QU,SizeOf(TObjQuery),7,True,5);
    CompileOneQuery(P,Q);
    Q^.QU_FRUL := P^.PP_FRUL;
    Q^.QU_LRUL := P^.PP_LRUL;
    If Not Error Then
      Q^.QU_NEXT := CompileQueries(P,WithArrow,ContChar,StopChars)
  End;
  CompileQueries := Q
End;

{ remove the queries typed by the user; FIXME: we cannot discard constants 
  and identifiers, since they have a global scope (plus, identifiers can be
  assigned); as more queries are submitted, more memory is consumed }
Procedure RemoveCommandLineQueries( P : ProgPtr );
Begin
  P^.PP_DVAR := P^.PP_UVAR { forget variables }
End;

{ compile queries typed by the user }
Function CompileCommandLineQueries( P : ProgPtr ) : QueryPtr;
Var
  Q : QueryPtr;
  c : Char;
Begin
  Q := CompileQueries(P,False,[],[';',EndOfInput]);
  c := NextCharNb(c);
  if c <> EndOfInput Then
    RaiseError('unexpected characters after the last query: "' + c + '"');
  CompileCommandLineQueries := Q
End;

{ append rules and queries to a program; return the first query if any }
Function CompileRulesAndQueries( P : ProgPtr; RuleType : RuType ) : QueryPtr;
Var
  FirstQ, HeadQ, Q : QueryPtr;
  HeadR, R : RulePtr;
  c : Char;
  Comment : StrPtr;
  Stop : Boolean;
Begin
  Stop := False;
  Error := False;
  FirstQ := Nil;
  HeadQ := P^.PP_FQRY;
  If HeadQ <> Nil Then
    HeadQ := LastQuery(HeadQ);
  HeadR := P^.PP_FRUL;
  If HeadR <> Nil Then
    HeadR := LastRule(HeadR);
  Repeat
    c := NextCharNb(c);
    If (c=EndOfInput) Or (c=';') Then
      Stop := True
    Else If c='"' Then { a comment, as a constant string }
    Begin
      Comment := ReadString
    End
    Else If c='-' Then  { a query }
    Begin
      Q := CompileQueries(P,True,['-'],[EndOfInput,';']);
      { note if Q is the first query read }
      If FirstQ = Nil Then
        FirstQ := Q;
      { set program's first query if not set yet. }
      If P^.PP_FQRY = Nil Then
        P^.PP_FQRY := Q;
      { attach this list to the previous one, if any }
      If (HeadQ <> Nil) Then
        HeadQ^.QU_NEXT := Q;
      { the new head is the last query in this list }
      Q := LastQuery(Q);
      { set program's last query. }
      If Q <> Nil Then
        P^.PP_LQRY := Q;
      HeadQ := Q
    End
    Else { a rule }
    Begin
      R := CompileRules(P,[EndOfInput,';','-','"'],RuleType);
      { set program's first rule if not set yet }
      If P^.PP_FRUL = Nil Then
        P^.PP_FRUL := R;
      { attach this list to the previous one, if any }
      If (HeadR <> Nil) Then
        HeadR^.RU_NEXT := R;
      { the new head is the last query in this list }
      R := LastRule(R);
      { set program's last rule. }
      If R <> Nil Then
        P^.PP_LRUL := R;
      HeadR := R
    End
  Until Stop Or Error;
  { machine state }
  P^.PP_UVAR := P^.PP_DVAR;
  P^.PP_UCON := P^.PP_DCON;
  CompileRulesAndQueries := FirstQ
End;
