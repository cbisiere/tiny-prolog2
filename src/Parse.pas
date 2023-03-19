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

Var TopVar : Integer; { Start index when searching a variable (locals) }

Function ReduceSystem( S : SysPtr; Backtrackable : Boolean; Var L : RestorePtr) : Boolean; Forward;

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
Function ReadOneTerm( Cut : Boolean) : TermPtr; Forward;

{ read the argument of a predicate (EndChar=')') or a tuple (EndChar='>') }
Function GetArgument( EndChar : Char ) : FuncPtr;
Var
  T : TermPtr;
  c : Char;
Begin
  T := ReadOneTerm(False);
  c := GetCharNb(c);
  If c =  ',' Then
    GetArgument := NewSymbol(T,TermPtr(GetArgument(EndChar)))
  Else
  If c = EndChar  Then
    GetArgument := NewSymbol(T,Nil)
  Else
  Begin
    RaiseError('"' + EndChar + '" expected');
    GetArgument := Nil
  End
End;

{ read and return a constant string, including the double quotes }
Function ReadString : AnyStr;
Var
  c : Char;
  Ch : AnyStr;
  Done : Boolean;
Begin
  CheckCondition(NextChar(c) = '"', 'string expected');
  Ch := GetChar(c); { both quotes are part of the string }
  Repeat
    Done := False;
    GetCharUntil(Ch,['"',EndOfLine,EndOfInput]);
    Case NextChar(c) Of
    '"':
      Begin
        Ch := Ch + GetChar(c); { accept it }
        If NextChar(c) = '"' Then { doubled: discard the second one }
          c := GetChar(c)
        Else
          Done := True
      End;
    EndOfLine:
      If Ch[Length(Ch)] = '\' Then { string continuation on next line }
      Begin
        c := GetChar(c);
        Delete(Ch,Length(Ch),1)
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
Function ReadOneTerm; (* ( Cut : Boolean ) : TermPtr *)
Var
  T : TermPtr;
  Ch  : AnyStr;
  c,c2 : Char;
  Count : Byte;
Begin
  Ch := '';
  Spaces;
  Count := GrabLetters(Ch);
  Case Count Of
  0 :
    Begin
      c := NextChar(c);
      If c In Digits Then     { an integer }
      Begin
        GetCharWhile(Ch,Digits);
        T := TermPtr(InstallConst(Ch));
      End
      Else
      If c ='(' Then          { ( <term> ) }
      Begin
        c := GetChar(c);
        T := ReadOneTerm(False);
        Verify(')')
      End
      Else
      If c = '<' Then        { a tuple }
      Begin
        c := GetChar(c);
        T := TermPtr(GetArgument('>'))
      End
      Else
      If c = '"' Then        { a string }
      Begin
        Ch := ReadString;
        T := TermPtr(InstallConst(Ch))
      End
      Else
      If Cut and (c In ['!','/']) Then    { the "cut" }
      Begin
        c := GetChar(c);
        T := TermPtr(InstallConst('!'))
      End
      Else
        RaiseError('term expected')
    End;
  1 :
    Begin                      { a variable }
      GetCharWhile(Ch,Digits);
      GetCharWhile(Ch,['''']);
      T := TermPtr(InstallVariable(Ch,TopVar));
    End;
  Else { at least 2 letters: an identifier }
    Begin
      Count := GrabLetters(Ch);
      While (NextChar(c)='-') And IsLetter(NextNextChar(c2)) Do
      Begin
        Ch := Ch + GetChar(c); { read the '-' }
        Count := GrabLetters(Ch);
      End;
      GetCharWhile(Ch,Digits);
      If NextChar(c) = '(' Then { a predicate }
      Begin
        c := GetChar(c);
        T := TermPtr(NewSymbol(TermPtr(InstallConst(Ch)),TermPtr(GetArgument(')'))));
      End
      Else
        T := TermPtr(InstallConst(Ch)) { a constant }
    End
  End;
  c := NextChar(c);
  If c = '.' Then    { a list element }
  Begin
    c := GetChar(c);
    T := TermPtr(NewSymbol( TermPtr(InstallConst('.')),
        TermPtr(NewSymbol(T,TermPtr(NewSymbol(ReadOneTerm(False),Nil)))) ))
  End;
  ReadOneTerm := T
End;

{ read an equations or a inequation }
Function ReadEquation : EqPtr;
Var
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
  c : Char;
Begin
  E := Nil;
  T1 := ReadOneTerm(False);
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
    T2 := ReadOneTerm(False);  { right term }
  If Not Error Then
    E := PushEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations }
Function ReadSystem : EqPtr;
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
      E := ReadEquation;
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

{ compile and reduce a system; equations in the reduced system are encoded
  within the variables; inequations in the reduced system are encoded 
  as a list of inequations }
Function CompileSystem : EqPtr;
Var
  E : EqPtr;
  S : SysPtr;
  U : RestorePtr;
Begin
  E := ReadSystem;
  If Not Error Then
  Begin

    S := NewSys;
    CopyAllEqInSys(S,E);

    U := Nil;
    If Not ReduceSystem(S,False,U) Then
      RaiseError('Constraints cannot be satisfied');
    FreeSys(S);
  End;
  CompileSystem := E
End;

{ compile a term }
Function CompileOneTerm( Cut : Boolean ) : BTermPtr;
Var B : BTermPtr;
Begin
  B := NewBTerm;
  With B^ Do
  Begin
    BT_TERM := ReadOneTerm(Cut);
    BT_CONS := Access(BT_TERM)
  End;
  CompileOneTerm := B
End;

{ compile a sequence of terms, stopping at a char in StopChars  }
Function CompileTerms( StopChars : CharSet ) : BTermPtr;
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
      BT_TERM := ReadOneTerm(True);
      BT_CONS := Access(BT_TERM);
      BT_NEXT := CompileTerms(StopChars)
    End
  End
  Else
    B := Nil;
  CompileTerms := B
End;

{ compile a rule }
Procedure CompileOneRule( R : RulePtr );
Var 
  B : BTermPtr;
  E : EqPtr;
  c : Char;
Begin
  TopVar := NbVar + 1;
  Spaces;
  With R^ Do
  Begin
    RU_FVAR := NbVar + 1;
    RU_SYST := Nil;
    B := CompileOneTerm(False); { head }
    RU_FBTR := B;
    Verify('->');
    If Not Error Then
    Begin
      B^.BT_NEXT := CompileTerms(['{',';',EndOfInput]);
      c := NextCharNb(c);
      If c = '{' Then
      Begin
        E := CompileSystem; { WARNING: MAY ADD THINGS }
        RU_SYST := E 
      End;
      Verify(';');
      RU_LVAR := NbVar;
      TopVar := NbVar + 1
    End
  End
End;

{ compile a sequence of rules, stopping at a char in StopChars  }
Function CompileRules( StopChars : CharSet; RuleType : RuType ) : RulePtr;
Var
  R : RulePtr;
  c : Char;
Begin
  c := NextCharNb(c);
  If (Not (c In StopChars)) And (Not Error) Then
  Begin
    R := NewRule(RuleType);
    CompileOneRule(R);
    R^.RU_NEXT := CompileRules(StopChars,RuleType)
  End
  Else
    R := Nil;
  CompileRules := R
End;

{ compile a query; reduce the system iif any }
Procedure CompileOneQuery( Q : QueryPtr );
Var
  E : EqPtr;
  c : Char;
Begin
  Spaces;
  TopVar := NbVar + 1;
  With Q^ Do
  Begin
    QU_FRUL := Nil;
    QU_LRUL := Nil;
    QU_FVAR := NbVar + 1;
    QU_FCON := NbConst + 1;
    QU_FBTR := CompileTerms(['{',';',EndOfInput]);
    QU_SYST := Nil;
    c := NextCharNb(c);
    If c = '{' Then
    Begin
      E := CompileSystem;
      QU_SYST := E { WARNING: MAY ADD THINGS }
    End;
    Verify(';');
    QU_LVAR := NbVar
  End
End;

{ compile a sequence of queries; if ContChar is not empty, each query 
  must start with a char in this set; the sequence ends with a char 
  in StopChars }
Function CompileQueries( P : ProgPtr; WithArrow : Boolean;
  ContChar, StopChars : CharSet ) : QueryPtr;
Var
  Q : QueryPtr;
  c : Char;
Begin
  Q := Nil;
  c := NextCharNb(c);
  If ((ContChar=[]) Or (c In ContChar))
    And (Not (c In StopChars)) And (Not Error) Then
  Begin
    If WithArrow Then
      Verify('->');
    Q := QueryPtr(NewPrologObject(QU, SizeOf(TObjQuery), 5));
    CompileOneQuery(Q);
    Q^.QU_FRUL := P^.PP_FRUL;
    Q^.QU_LRUL := P^.PP_LRUL;
    Q^.QU_NEXT := CompileQueries(P,WithArrow,ContChar,StopChars)
  End;
  CompileQueries := Q
End;

{ remove the queries typed by the user }
Procedure RemoveCommandLineQueries( P : ProgPtr );
Begin
  NbVar   := P^.PP_LVAR;
  NbConst := P^.PP_LCON
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
  Comment : AnyStr;
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
      R := CompileRules([EndOfInput,';','-','"'],RuleType);
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
  P^.PP_LVAR := NbVar;
  P^.PP_LCON := NbConst;
  CompileRulesAndQueries := FirstQ
End;
