{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Parse.pas                                                  }
{   Author      : Christophe Bisiere                                         }
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

{ per-syntax elements }
Type
  TSyntaxElement = Array[TSyntax] Of Record
    RuleArrow: String[2];
    RuleEnd: Char;
    QueryArrow: String[2];
    PromptEnd: Char;
    AcceptSys: Boolean
  End;
Const
  Syntax : TSyntaxElement = (
    (RuleArrow:'->';RuleEnd:';';QueryArrow:'->';PromptEnd:';';AcceptSys:False),
    (RuleArrow:'->';RuleEnd:';';QueryArrow:'->';PromptEnd:';';AcceptSys:True),
    (RuleArrow:'->';RuleEnd:';';QueryArrow:'->';PromptEnd:';';AcceptSys:False),
    (RuleArrow:':-';RuleEnd:'.';QueryArrow:':-';PromptEnd:'.';AcceptSys:False)
  );

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

{ read a term, possibly in a global context (that is, when parsing a
  rule), possibly accepting a cut as a valid term }
Function ReadOneTerm( P : ProgPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr; Forward;

{ read the argument of a predicate (EndChar=')') or a tuple (EndChar='>');
  return Nil if an error occurred }
Function GetArgument( P : ProgPtr; glob : Boolean; 
    EndChar : Char ) : FuncPtr;
Var
  T : TermPtr;
  c : Char;
  F : FuncPtr;
  TF : TermPtr Absolute F;
Begin
  F := Nil;
  T := ReadOneTerm(P,glob,False);
  If Not Error Then
  Begin
    c := GetCharNb(c);
    If c =  ',' Then
    Begin
      F := GetArgument(P,glob,EndChar);
      If Not Error Then
        F := NewSymbol(T,TF)
    End
    Else
    If c = EndChar  Then
      F := NewSymbol(T,Nil)
    Else
    Begin
      RaiseError('"' + EndChar + '" expected');
      F := Nil
    End
  End;
  GetArgument := F
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
Function ReadOneTerm; (* ( P : ProgPtr; glob : Boolean; 
    Cut : Boolean ) : TermPtr *)
Var
  T : TermPtr;
  T2 : TermPtr;
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
  IsVar,IsIdent : Boolean;
  IsUpper : Boolean;
  n,m : LongInt;
  y : TSyntax; 
Begin
  T := Nil;
  y := GetSyntax(P);
  IsVar := False;
  IsIdent := False;
  Spaces;
  Ch := NewString;
  c1 := NextChar(c1);
  c2 := NextNextChar(c2);
  If (c1 In Digits) Or 
    (c1='-') And (c2 In Digits) Then  { an integer }
  Begin
    If c1='-' Then
      StrAppendChar(Ch,GetChar(c1));
    n := GetCharWhile(Ch,Digits);
    C := InstallConst(P^.PP_DCON,Ch,CN,glob);
    T := TC
  End
  Else
  If c1 ='(' Then { ( <term> ) }
  Begin
    c1 := GetChar(c1);
    T := ReadOneTerm(P,glob,False);
    If Not Error Then
      Verify(')')
  End
  Else
  If c1 = '<' Then { a tuple }
  Begin
    c1 := GetChar(c1);
    If NextCharNb(c1)='>' Then
    Begin
      { "<>" }
      c1 := GetCharNb(c1);
      If (y In [PrologIIp,Edinburgh]) And { "<>(t1,...tn)" }
          (NextCharNb(c1) = '(') Then
      Begin
        c1 := GetCharNb(c1);
        F := GetArgument(P,glob,')')
      End
      Else { "<>" only }
        F := NewSymbol(Nil,Nil)
    End
    Else 
      If y <> Edinburgh Then 
        F := GetArgument(P,glob,'>')
      Else
        RaiseError('this tuples syntax is not allowed in Edinburgh mode');
    If Not Error Then
      T := TF
  End
  Else
  If c1 = '"' Then { a string }
  Begin
    Ch := ReadString;
    If Not Error Then
    Begin
      C := InstallConst(P^.PP_DCON,Ch,CS,glob);
      T := TC
    End
  End
  Else
  If Cut And { the "cut" }
    ((c1 = '/') And (y In [PrologII,PrologIIc]) Or
     (c1 = '!') And (y In [PrologIIp,Edinburgh])) Then 
  Begin
    c1 := GetChar(c1);
    I := InstallIdentifier(P^.PP_DIDE,NewStringFrom('!'),glob);
    T := TI
  End
  Else
  If (c1 = '_') And   { a variable: PrologII+ basic syntax }
      (y In [PrologIIp,Edinburgh]) Then
  Begin
    n := GrabAlpha(Ch); { letters (inc. accented), digits, underscore }
    IsVar := True
  End
  Else { TODO: Edinburgh: identifiers made of graphic_char }
  If GrabOneLetter(Ch,IsUpper) Then { variable or identifiers }
  Begin
    { Edinburgh variables start with a "big letter"; Marseille variables 
     start with a single letter }
    If y = Edinburgh Then
      IsVar := IsUpper
    Else
      IsVar := Not GrabOneLetter(Ch,IsUpper);
    IsIdent := Not IsVar;

    If y In [PrologII,PrologIIc] Then { old Prolog II syntax, w/ accented letters }
    Begin
      Repeat
        If NextChar(c1)='-' Then { "-"<word> continuation }
          StrAppendChar(Ch,GetChar(c1));
        n := GrabLetters(Ch);
        m := GetCharWhile(Ch,Digits);
        m := GetCharWhile(Ch,['''']);
        RaiseErrorIf((c1 = '-') And (n = 0),'Dash must be followed by a letter')
      Until Error Or (NextChar(c1)<>'-');
    End
    Else { PrologII+ and Edinburgh extended syntax }
    Begin
      n := GrabAlpha(Ch);
      If GetSyntax(P) <> Edinburgh Then
        n := GetCharWhile(Ch,[''''])
    End
  End
  Else
    RaiseError('term expected');

  { delayed installations: variable }
  If IsVar Then
  Begin
    V := InstallVariable(P^.PP_DVAR,P^.PP_LVAR,Ch,glob);
    T := TV
  End;

  { delayed installations: identifier, with possible arguments }
  If IsIdent Then
  Begin
    I := InstallIdentifier(P^.PP_DIDE,Ch,glob);
    T := TI;
    If NextCharNb(c1) = '(' Then { predicate's arguments }
    Begin
      c1 := GetCharNb(c1);
      F := GetArgument(P,glob,')');
      If Not Error Then
      Begin
        F2 := NewSymbol(TI,TF);
        T := TF2
      End
    End
  End;

  { dotted lists }
  If (Not Error) And (y <> Edinburgh) And 
      (NextCharNb(c1)='.') Then
  Begin
    c1 := GetChar(c1);
    I := InstallIdentifier(P^.PP_DIDE,NewStringFrom('.'),glob);
    T2 := ReadOneTerm(P,glob,False);
    If Not Error Then
    Begin
      F := NewSymbol(T2,Nil); { q: new term }
      F2 := NewSymbol(T,TF); { t: term read above }
      F3 := NewSymbol(TI,TF2); { t.q }
      T := TF3
    End
  End;

  ReadOneTerm := T
End;

{ read an equations or a inequation }
Function ReadEquation( P : ProgPtr; glob : Boolean ) : EqPtr;
Var
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
  c : Char;
Begin
  E := Nil;
  T1 := ReadOneTerm(P,glob,False);
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
    T2 := ReadOneTerm(P,glob,False);  { right term }
  If Not Error Then
    E := NewEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations }
Function ReadSystem( P : ProgPtr; glob : Boolean ) : EqPtr;
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
      E := ReadEquation(P,glob);
      If First Then
      Begin
        FirstE := E;
        First := False
      End
      Else
        PrevE^.EQ_NEXT := E;
      PrevE := E
    Until (Error) Or (GetCharNb(c) <> ',');
    RaiseErrorIf(c <> '}','Missing }')
  End;
  ReadSystem := FirstE
End;

{ compile a system of equations and inequations }
Function CompileSystem( P : ProgPtr; glob : Boolean ) : EqPtr;
Begin
  CompileSystem := ReadSystem(P,glob)
End;

{ compile a term }
Function CompileOneTerm( P : ProgPtr; glob : Boolean; Cut : Boolean ) : BTermPtr;
Var B : BTermPtr;
Begin
  B := NewBTerm;
  With B^ Do
  Begin
    BT_TERM := ReadOneTerm(P,glob,Cut);
    BT_ACCE := AccessIdentifier(BT_TERM)
  End;
  CompileOneTerm := B
End;

{ compile a sequence of terms, stopping at a char in StopChars; set HasCut to
  true if the queue contains a cut, false otherwise }
Function CompileTerms( P : ProgPtr; glob : Boolean; StopChars : CharSet; 
  Var HasCut : Boolean ) : BTermPtr;

  Function DoCompileTerms : BTermPtr;
  Var
    B : BTermPtr;
    c : Char;
    Must : Boolean;
  Begin
    c := NextCharNb(c);
    If (Not (c In StopChars)) And (Not Error) Then
    Begin
      B := NewBTerm;
      With B^ Do
      Begin
        BT_TERM := ReadOneTerm(P,glob,True);
        If Not Error Then
        Begin
          HasCut := HasCut Or TermIsCut(BT_TERM);
          BT_ACCE := AccessIdentifier(BT_TERM);
          Must := (GetSyntax(P) = Edinburgh) And (NextCharNb(c) = ',');
          If Must Then
            c := GetCharNb(c);
          BT_NEXT := DoCompileTerms;
          RaiseErrorIf(Must And (BT_NEXT = Nil),'Term expected after the comma')
        End
      End
    End
    Else
      B := Nil;
    DoCompileTerms := B
  End;

Begin
  HasCut := False;
  CompileTerms := DoCompileTerms
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
Procedure CompileOneRule( P : ProgPtr; RuleType : RuType );
Var 
  R : RulePtr;
  B : BTermPtr;
  c : Char;
  HasCut : Boolean;
  y : TSyntax;
  StopChars : CharSet;
Begin
  y := GetSyntax(P);
  StopChars := [Syntax[y].RuleEnd];
  If Syntax[y].AcceptSys Then
    StopChars := StopChars + ['{'];
  R := NewRule(RuleType);
  OpenLocalContextForRule(P,R);
  With R^ Do
  Begin
    RU_SYST := Nil;
    B := CompileOneTerm(P,True,False); { head }
    RU_FBTR := B;
    If (y = Edinburgh) And (NextCharNb(c) = Syntax[y].RuleEnd) Then
      Verify(Syntax[y].RuleEnd)
    Else
    Begin
      Verify(Syntax[y].RuleArrow);
      If Not Error Then
        B^.BT_NEXT := CompileTerms(P,True,StopChars,HasCut);
      RU_ACUT := HasCut;
      If Not Error Then
        If y = Edinburgh Then 
          RaiseErrorIf(B^.BT_NEXT = Nil,'Term expected after :-');
      If Not Error Then
        If (Syntax[y].AcceptSys) And (NextCharNb(c) = '{') Then
          RU_SYST := CompileSystem(P,True);
      If Not Error Then
        Verify(Syntax[y].RuleEnd)
    End
  End;
  If Not Error Then
  Begin
    CloseLocalContextForRule(P,R);
    AppendOneRule(P,R)
  End
End;

{ compile a sequence of rules, stopping at a char in StopChars  }
Procedure CompileRules( P : ProgPtr; StopChars : CharSet; RuleType : RuType );
Var
  c : Char;
Begin
  While (Not (NextCharNb(c) In StopChars)) And (Not Error) Do
    CompileOneRule(P,RuleType);
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
Procedure CompileOneQuery( P : ProgPtr );
Var
  Q : QueryPtr;
  c : Char;
  HasCut : Boolean;
  y : TSyntax;
  StopChars : CharSet;
Begin
  Q := NewQuery(P^.PP_LEVL);
  OpenLocalContextForQuery(P,Q);
  y := GetSyntax(P);
  StopChars := [Syntax[y].PromptEnd,EndOfInput];
  If Syntax[y].AcceptSys Then
    StopChars := StopChars + ['{'];
  With Q^ Do
  Begin
    QU_FBTR := CompileTerms(P,False,StopChars,HasCut);
    If Not Error Then
    Begin
      QU_ACUT := HasCut;
      If (Syntax[y].AcceptSys) And (NextCharNb(c) = '{') Then
        QU_SYST := CompileSystem(P,False);
      If Not Error Then
        Verify(Syntax[y].PromptEnd)
    End
  End;
  CloseLocalContextForQuery(P,Q);
  UpdateQueryScope(P,Q);
  If Not Error Then
    AppendOneQuery(P,Q)
End;

{ compile a sequence of queries; if ContChar is not empty, each query 
  must start with a char in this set; the sequence ends with a char 
  in StopChars }
Procedure CompileQueries( P : ProgPtr; WithArrow : Boolean;
  ContChar, StopChars : CharSet );
Var
  More : Boolean;
  c : Char;
Begin
  Repeat
    c := NextCharNb(c);
    More := ((ContChar=[]) Or (c In ContChar))
      And (Not (c In StopChars)) And (Not Error);
    If More Then
    Begin
      If WithArrow Then
        Verify(Syntax[GetSyntax(P)].QueryArrow);
      If Not Error Then
        CompileOneQuery(P)
    End
  Until Error Or Not More
End;

{ compile queries typed by the user; note that we stop on EndOfInput
  (instead of EndOfLine), even if the keyboard system ensures 
  that EndOfInput is the last char in the input string, because The *Nb
  input primitives consider EndOfLine as one of the blank characters;
  thus, at the end, we replace EndOfInput with EndOfLine }  
Procedure CompileCommandLineQueries( P : ProgPtr );
Var
  c : Char;
Begin
  CompileQueries(P,False,[],[EndOfInput]);
  c := NextCharNb(c);
  RaiseErrorIf(c <> EndOfInput,
      'unexpected characters after the last query: "' + c + '"');
  { replace EndOfInput with EndOfLine }
  c := GetChar(c);
  UnGetChar(EndOfLine)
End;

{ append rules and queries to a program }
Procedure CompileRulesAndQueries( P : ProgPtr; RuleType : RuType );
Var
  c : Char;
  qc : Char; { first char of the query prompt }
  Comment : StrPtr;
  Stop : Boolean;
Begin
  Stop := False;
  Error := False;
  qc := Syntax[GetSyntax(P)].QueryArrow[1];
  Repeat
    c := NextCharNb(c);
    If (c=EndOfInput) Or (c=';') Then
      Stop := True
    Else If c = '"' Then { comment }
      Comment := ReadString
    Else If c = qc Then  { queries }
      CompileQueries(P,True,[qc],[EndOfInput,';'])
    Else { rules }
      CompileRules(P,[EndOfInput,';',qc,'"'],RuleType)
  Until Stop Or Error;
  { machine state }
  P^.PP_UVAR := P^.PP_DVAR;
  P^.PP_UCON := P^.PP_DCON
End;
