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
    RuleEnd: TTokenType;
    PromptEnd: TTokenType;
    AcceptSys: Boolean
  End;
Const
  Syntax : TSyntaxElement = (
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:False),
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:True),
    (RuleEnd:TOKEN_SEMICOLON;PromptEnd:TOKEN_SEMICOLON;AcceptSys:False),
    (RuleEnd:TOKEN_DOT;PromptEnd:TOKEN_DOT;AcceptSys:False)
  );

{ Note: in the following procedures, the K parameter is the *current* token 
 to analyze; the procedures themselves must make sure that, upon return, K 
 is the next token to analyze (since it will be passed to another parsing 
 procedure) }

{ read a term, possibly in a global context (that is, when parsing a
  rule), possibly accepting a cut as a valid term }
Function ReadOneTerm( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean) : TermPtr; Forward;


{----------------------------------------------------------------------------}
{ helpers for emitting                                                       }
{----------------------------------------------------------------------------}

{-----------------------------------------------------------------------}
{                                                                       }
{   The functional symbol F is used to encode predicates as follows:    }
{                                                                       }
{           F                                                           }
{          / \               (1) tuple: the name of the predicate is    }
{       name  F                  the first argument: <name,arg1,...>    }
{            / \                                                        }
{         arg1  F            (2) of course arg1 .. argN can also be     }
{              / \               predicates.                            }
{           arg2   ...                                                  }
{                   \        (3) a '.' (Prolog list) is considered as   }
{                    F           a regular predicate.                   }
{                  /  \                                                 }
{               argN    Nil                                             }
{                                                                       }
{-----------------------------------------------------------------------}

{ return a new identifier as a term, from a string }
Function EmitIdent( P : ProgPtr; ident : TString; glob : Boolean ) : TermPtr;
Var
  I : IdPtr;
  TI : TermPtr Absolute I;
Begin
  EmitIdent := Nil;
  I := InstallIdentifier(P^.PP_DIDE,NewStringFrom(ident),glob);
  EmitIdent := TI
End;

{ return a new term "ident(T1,T2)"; following the comment above, "a(b,c)" is 
 implemented as "F(a,F(b,F(c,Nil)))" }
Function EmitFunc( P : ProgPtr; ident : TString; T1,T2 : TermPtr; 
    glob : Boolean ) : TermPtr;
Var
  I,F1,F2,F3 : TermPtr;
Begin
  EmitFunc := Nil;
  CheckCondition((T1 <> Nil) And (T2 <> Nil),'EmitFunc: Nil argument');
  I := EmitIdent(P,ident,glob);
  F3 := NewF(T2,Nil); { F(b,Nil) }
  F2 := NewF(T1,F3); { F(a,F(b,Nil)) }
  F1 := NewF(I,F2); { F(ident,F(a,F(b,Nil))) }
  EmitFunc := F1
End;

{ return a term "a.b", viewed as '.'(a,b)" and thus implemented as 
 "F('.',F(a,F(b,Nil)))"; if b is Nil, replace it with "F('nil',Nil)", that is,
 add ".nil" at the end of the dotted list; this helps implementing the 
 following equivalences:
  [a|b] <=> a.b
  [a,b] <=> a.b.nil
 (see PII+ syntax (cf. pdf doc p.45) }
Function EmitDot( P : ProgPtr; T1,T2 : TermPtr ) : TermPtr;
Begin
  EmitDot := Nil;
  If T2 = Nil Then
    T2 := EmitIdent(P,'nil',True);
  EmitDot := EmitFunc(P,'.',T1,T2,True)
End;

{----------------------------------------------------------------------------}
{ parser                                                                     }
{----------------------------------------------------------------------------}

{ raise an error if a token is not of a certain type; read the token 
 following the token to verify }
Procedure VerifyToken( P : ProgPtr; Var K : TokenPtr; typ : TTokenType );
Var
  y : TSyntax;
Begin
  y := GetSyntax(P);
  If TokenType(K) = typ Then
    K := ReadToken(y)
  Else
    SyntaxError(TokenStr[typ] + ' expected')
End;

{ read the comma-separated list of arguments of:
 - a predicate: "a(b,c,d)" EndToken = ')' 
 - a tuple: "<a,b,c,d>", EndToken = '>'
 return Nil if an error occurs; set K to the token *after* EndToken;
}
Function GetArgument( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    EndToken : TTokenType ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  GetArgument := Nil;
  y := GetSyntax(P);
  T := ReadOneTerm(P,K,glob,False);
  If Error Then Exit;
  If TokenType(K) = TOKEN_COMMA Then
  Begin
    K := ReadToken(y);
    T2 := GetArgument(P,K,glob,EndToken);
    If Error Then Exit;
    T := NewF(T,T2)
  End
  Else 
  Begin
    VerifyToken(P,K,EndToken);
    If Error Then Exit;
    T := NewF(T,Nil)
  End;
  GetArgument := T
End;


{ read the "remaining part" (that is, anything but the head) part of a list; 
 T1 is the (already read) list head }
Function ReadListQueue( P : ProgPtr; Var K : TokenPtr; T1 : TermPtr; 
    glob : Boolean ) : TermPtr;
Var
  T2 : TermPtr;
Begin
  ReadListQueue := Nil;
  T2 := ReadOneTerm(P,K,glob,False);
  If Error Then Exit;
  ReadListQueue := EmitDot(P,T1,T2)
End;

{ read a []-style list expression:
 listexpr = "expr" or "expr, list_expr" or "expr | expr" 
 if comma is true, the listexpr follows a comma, that is, is the remaining part 
 of a comma-separated list as in [a,b,c] }
Function ReadListExpr( P : ProgPtr; Var K : TokenPtr; comma, glob, Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T,T2 : TermPtr;
Begin
  ReadListExpr := Nil;
  y := GetSyntax(P);
  T := ReadOneTerm(P,K,glob,Cut); { TODO: cut allowed here?? }
  If Error Then Exit;
  If TokenType(K) = TOKEN_COMMA Then  { "a,b" <=> a.b.nil }
  Begin
    K := ReadToken(y);
    If Error Then Exit;
    T2 := ReadListExpr(P,K,True,glob,Cut); { remaining list }
    If Error Then Exit;
    T := EmitDot(P,T,T2)
  End
  Else If TokenType(K) = TOKEN_PIPE Then  { "a|b" <=> a.b }
  Begin
    K := ReadToken(y);
    If Error Then Exit;
    T2 := ReadOneTerm(P,K,glob,Cut);
    If Error Then Exit;
    T := EmitDot(P,T,T2)
  End
  Else If comma Then { "b" as the end of "a,b" generates a.b.nil }
  Begin
    T := EmitDot(P,T,Nil)
  End;
  ReadListExpr := T
End;

{ read a []-style list }
Function ReadList( P : ProgPtr; Var K : TokenPtr; glob,Cut : Boolean ) : TermPtr;
Var
  y : TSyntax;
  T : TermPtr;
Begin
  ReadList := Nil;
  y := GetSyntax(P);
  VerifyToken(P,K,TOKEN_LEFT_BRA);
  If TokenType(K) = TOKEN_RIGHT_BRA Then { "[]": atom }
  Begin
    T := EmitIdent(P,'[]',True);
    K := ReadToken(y)
  End
  Else
  Begin
    T := ReadListExpr(P,K,False,glob,Cut); { TODO: cut allowed here?? }
    If Error Then Exit;
    VerifyToken(P,K,TOKEN_RIGHT_BRA)
  End;
  ReadList := T
End;

{ read a term, possibly accepting a cut as a valid term }
Function ReadOneTerm; (* ( P : ProgPtr; Var K : TokenPtr; glob : Boolean; 
    Cut : Boolean ) : TermPtr *)
Var
  y : TSyntax;
  T : TermPtr;
  C : ConstPtr;
  TC : TermPtr Absolute C;
  F : TermPtr;
  V : VarPtr;
  TV : TermPtr Absolute V;
  I : IdPtr;
  TI : TermPtr Absolute I;
  s : StrPtr;
  cot : TypePrologObj;
Begin
  ReadOneTerm := Nil;
  y := GetSyntax(P);
  T := Nil;
  Case TokenType(K) Of
  TOKEN_MINUS, TOKEN_PLUS, { [+|-] number}
  TOKEN_INTEGER,TOKEN_REAL:
    Begin
      s := NewString;
      If TokenType(K) In [TOKEN_INTEGER,TOKEN_REAL] Then
        StrConcat(s,K^.TK_STRI)
      Else If TokenType(K) = TOKEN_MINUS Then
        StrAppend(s,'-');
      If Not (TokenType(K) In [TOKEN_INTEGER,TOKEN_REAL]) Then
      Begin
        K := ReadToken(y);
        If Not (TokenType(K) In [TOKEN_INTEGER,TOKEN_REAL]) Then
          SyntaxError('number expected after unary sign');
        If Error Then Exit;
        StrConcat(s,K^.TK_STRI)
      End;
      If TokenType(K) = TOKEN_INTEGER Then
        cot := CI
      Else
        cot := CR;
      If Not NormalizeConstant(s,ObjectTypeToConstType(cot)) Then
        SyntaxError('invalid constant');
      If Error Then Exit;
      C := InstallConst(P^.PP_DCON,s,cot,glob);
      T := TC;
      K := ReadToken(y)
    End;
  TOKEN_STRING:
    Begin
      C := InstallConst(P^.PP_DCON,K^.TK_STRI,CS,glob);
      T := TC;
      K := ReadToken(y)
    End;
  TOKEN_VARIABLE:
    Begin
      V := InstallVariable(P^.PP_DVAR,P^.PP_LVAR,K^.TK_STRI,glob);
      T := TV;
      K := ReadToken(y)
    End;
  TOKEN_IDENT: { "a" or "a(b,c)" }
    Begin
      I := InstallIdentifier(P^.PP_DIDE,K^.TK_STRI,glob);
      T := TI;
      K := ReadToken(y);
      If TokenType(K) = TOKEN_LEFT_PAR Then { predicate's arguments }
      Begin
        K := ReadToken(y);
        F := GetArgument(P,K,glob,TOKEN_RIGHT_PAR);
        If Error Then Exit;
        T := NewF(TI,F)
      End
    End;
  TOKEN_CUT:
    Begin
      If Cut Then 
      Begin
        T := EmitIdent(P,'!',True);
        K := ReadToken(y)
      End
      Else
        SyntaxError(TokenStr[TOKEN_CUT] + ' not allowed here')
    End;
  TOKEN_LEFT_PAR: { parenthesized term }
    Begin
      K := ReadToken(y);
      T := ReadOneTerm(P,K,glob,False);
      If Error Then Exit;
      VerifyToken(P,K,TOKEN_RIGHT_PAR);
      If Error Then Exit
    End;
  TOKEN_LEFT_CHE: { tuple }
    Begin
      K := ReadToken(y);
      If TokenType(K) = TOKEN_RIGHT_CHE Then
      Begin
        { "<>" }
        K := ReadToken(y);
        If (y In [PrologIIp,Edinburgh]) And { "<>(t1,...tn)" }
            (TokenType(K) = TOKEN_LEFT_PAR) Then
        Begin
          K := ReadToken(y);
          F := GetArgument(P,K,glob,TOKEN_RIGHT_PAR)
        End
        Else { "<>" only }
          F := NewF(Nil,Nil)
      End
      Else 
        If y <> Edinburgh Then 
          F := GetArgument(P,K,glob,TOKEN_RIGHT_CHE)
        Else
          SyntaxError('such tuples syntax is not allowed in Edinburgh mode');
      If Error Then Exit;
      T := F
    End;
  TOKEN_LEFT_BRA: { PII+ []-style list }
    Begin
      If (y In [PrologIIp,Edinburgh]) Then
        T := ReadList(P,K,glob,Cut)
      Else
        SyntaxError('[]-style list not allowed in the current syntax');
    End;
  Else
    SyntaxError(TokenTypeAsString(K) + ' not allowed here')
  End;
  If Error Then Exit;
  
  { dotted lists of terms; this seems to be allowed in PII+ Edinburgh, see 
   PII+ doc, pdf p. 45; we do not allowed it for now, as it is tricky to 
   distinguish this dot from an end-of-rule dot }
  { TODO: exclude CUT? }
  If (Not Error) And (y <> Edinburgh) And (TokenType(K) = TOKEN_DOT) Then
  Begin
    K := ReadToken(y);
    T := ReadListQueue(P,K,T,glob)
  End;

  ReadOneTerm := T
End;

{ read an equations or a inequation }
Function ReadEquation( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Var
  y : TSyntax;
  E : EqPtr;
  T1, T2 : TermPtr;
  Code : EqType;
Begin
  ReadEquation := Nil;
  y := GetSyntax(P);
  E := Nil;
  T1 := ReadOneTerm(P,K,glob,False);
  If Error Then Exit;
  Case TokenType(K) Of
  TOKEN_EQUAL:
    Begin
      Code := REL_EQUA;
      K := ReadToken(y)
    End;
  TOKEN_LEFT_CHE:
    Begin
      K := ReadToken(y);
      VerifyToken(P,K,TOKEN_RIGHT_CHE);
      If Error Then Exit;
      Code := REL_INEQ
    End;
  Else
    SyntaxError('comparison symbol expected')
  End;
  If Error Then Exit;
  T2 := ReadOneTerm(P,K,glob,False);  { right term }
  If Error Then Exit;
  E := NewEquation(Code,T1,T2);
  ReadEquation := E
End;

{ read a system of equations or inequations }
Function ReadSystem( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Var
  E, FirstE, PrevE : EqPtr;
  First : Boolean;
Begin
  ReadSystem := Nil;
  FirstE := Nil;
  PrevE := Nil;
  VerifyToken(P,K,TOKEN_LEFT_CUR);
  If Error Then Exit;
  First := True;
  Repeat
    If Not First Then
      VerifyToken(P,K,TOKEN_COMMA);
    If Error Then Exit;
    E := ReadEquation(P,K,glob);
    If Error Then Exit;
    If First Then
    Begin
      FirstE := E;
      First := False
    End
    Else
      PrevE^.EQ_NEXT := E;
    PrevE := E
  Until (TokenType(K) <> TOKEN_COMMA) Or Error;
  If Error Then Exit;
  VerifyToken(P,K,TOKEN_RIGHT_CUR);
  If Error Then Exit;
  ReadSystem := FirstE
End;

{ compile a system of equations and inequations }
Function CompileSystem( P : ProgPtr; Var K : TokenPtr; glob : Boolean ) : EqPtr;
Begin
  CompileSystem := ReadSystem(P,K,glob)
End;

{ compile a term }
Function CompileOneTerm( P : ProgPtr; Var K : TokenPtr; 
  glob : Boolean; Cut : Boolean ) : BTermPtr;
Var 
  B : BTermPtr;
Begin
  B := NewBTerm;
  With B^ Do
  Begin
    BT_TERM := ReadOneTerm(P,K,glob,Cut);
    BT_ACCE := AccessIdentifier(BT_TERM)
  End;
  CompileOneTerm := B
End;

{ compile a (possibly empty) sequence of terms, stopping at a token in 
 StopTokens; set HasCut to true if the queue contains a cut, false otherwise }
Function CompileTerms( P : ProgPtr; Var K : TokenPtr; 
    glob : Boolean; StopTokens : TTokenSet; Var HasCut : Boolean ) : BTermPtr;
Var
  y : TSyntax;

  Function DoCompileTerms : BTermPtr;
  Var
    B : BTermPtr;
    Must : Boolean;
  Begin
    DoCompileTerms := Nil;
    If (Not (TokenType(K) In StopTokens)) And (Not Error) Then
    Begin
      B := NewBTerm;
      With B^ Do
      Begin
        BT_TERM := ReadOneTerm(P,K,glob,True);
        If Error Then Exit;
        HasCut := HasCut Or TermIsCut(BT_TERM);
        BT_ACCE := AccessIdentifier(BT_TERM);
        Must := (GetSyntax(P) = Edinburgh) And (TokenType(K) = TOKEN_COMMA);
        If Must Then
          K := ReadToken(y);
        BT_NEXT := DoCompileTerms;
        If Must And (BT_NEXT = Nil) Then
          SyntaxError('term expected after ' + TokenStr[TOKEN_COMMA])
      End
    End
    Else
      B := Nil;
    DoCompileTerms := B
  End;

Begin
  y := GetSyntax(P);
  CompileTerms := Nil;
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
Procedure CompileOneRule( P : ProgPtr; Var K : TokenPtr; RuleType : RuType );
Var 
  y : TSyntax;
  R : RulePtr;
  B : BTermPtr;
  HasCut : Boolean;
  StopTokens : TTokenSet;
Begin
  y := GetSyntax(P);
  StopTokens := [Syntax[y].RuleEnd];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  R := NewRule(RuleType);
  OpenLocalContextForRule(P,R);
  With R^ Do
  Begin
    RU_SYST := Nil;
    B := CompileOneTerm(P,K,True,False); { head }
    If Error Then Exit;
    RU_FBTR := B;
    If (y = Edinburgh) And (TokenType(K) = Syntax[y].RuleEnd) Then
      K := ReadToken(y)
    Else
    Begin
      VerifyToken(P,K,TOKEN_ARROW);
      If Error Then Exit;
      B^.BT_NEXT := CompileTerms(P,K,True,StopTokens,HasCut); {FIXME: can return Nil without Error?}
      RU_ACUT := HasCut;
      If y = Edinburgh Then 
        If B^.BT_NEXT = Nil Then
          SyntaxError('term expected after ' + TokenStr[TOKEN_ARROW]);
      If Error Then Exit;
      If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
        RU_SYST := CompileSystem(P,K,True);
      If Error Then Exit;
      VerifyToken(P,K,Syntax[y].RuleEnd)
    End
  End;
  If Error Then Exit;
  CloseLocalContextForRule(P,R);
  AppendOneRule(P,R)
End;

{ compile a sequence of rules, stopping at a token in StopTokens  }
Procedure CompileRules( P : ProgPtr; Var K : TokenPtr; 
    StopTokens : TTokenSet; RuleType : RuType );
Begin
  While (Not (TokenType(K) In StopTokens)) And (Not Error) Do
    CompileOneRule(P,K,RuleType);
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
 - note that this system cannot be reduced right away, as its 
   solution (or lack thereof) may depend on global variables;
 - read the token after the end-of-query mark only if ReadNextToken is true;
   setting this parameter to false is useful when reading a goal typed at
   the prompt, as the char following the end-of-query mark is supposed to be 
   returned when the goal is in_char(c) }
Procedure CompileOneQuery( P : ProgPtr; Var K : TokenPtr; 
    ReadNextToken : Boolean );
Var
  Q : QueryPtr;
  HasCut : Boolean;
  y : TSyntax;
  StopTokens : TTokenSet;
Begin
  Q := NewQuery(P^.PP_LEVL);
  OpenLocalContextForQuery(P,Q);
  y := GetSyntax(P);
  StopTokens := [Syntax[y].PromptEnd,TOKEN_END_OF_INPUT];
  If Syntax[y].AcceptSys Then
    StopTokens := StopTokens + [TOKEN_LEFT_CUR];
  With Q^ Do
  Begin
    QU_FBTR := CompileTerms(P,K,False,StopTokens,HasCut);
    If Error Then Exit;
    QU_ACUT := HasCut;
    If (Syntax[y].AcceptSys) And (TokenType(K) = TOKEN_LEFT_CUR) Then
      QU_SYST := CompileSystem(P,K,False);
    If Error Then Exit;
    { verify end-of-query mark }
    If TokenType(K) <> Syntax[y].PromptEnd Then
      SyntaxError(TokenStr[Syntax[y].PromptEnd] + ' expected');
    { read the next token only when requested; beware of infinite loops :) }
    If ReadNextToken Then
      K := ReadToken(y);
    If Error Then Exit
  End;
  CloseLocalContextForQuery(P,Q);
  UpdateQueryScope(P,Q);
  AppendOneQuery(P,Q)
End;

{ compile a sequence of queries; if ContTokens is not empty, each query 
  must start with a token in this set; the sequence ends with a token 
  in StopTokens;  }
Procedure CompileQueries( P : ProgPtr; Var K : TokenPtr; WithArrow : Boolean;
  ContTokens, StopTokens : TTokenSet );
Var
  More : Boolean;
Begin
  Repeat
    More := ((ContTokens=[]) Or (TokenType(K) In ContTokens))
      And (Not (TokenType(K) In StopTokens)) And (Not Error);
    If More Then
    Begin
      If WithArrow Then
        VerifyToken(P,K,TOKEN_ARROW);
      If Error Then Exit;
      CompileOneQuery(P,K,True);
      If Error Then Exit
    End
  Until Not More Or Error
End;

{----------------------------------------------------------------------------}
{ public procedures and functions                                            }
{----------------------------------------------------------------------------}

{ these high-level entry points must start with reading one token }

{ parse a term from the current input stream; stop token chars stay in the 
 input buffer }
Function ParseOneTerm( P : ProgPtr ) : TermPtr;
Var
  y : TSyntax;
  K : TokenPtr;
  line : TLineNum;
  col : TCharPos;
Begin
  ParseOneTerm := Nil;
  y := GetSyntax(P);
  K := ReadToken(y);
  If Error Then Exit;
  ParseOneTerm := ReadOneTerm(P,K,False,False);
  { since K is now the token *following* the compiled term, we must unread it 
   (and all the spaces before) so that in_char will read the first char after 
   the term }
  GetTokenLocation(K,line,col);
  UngetChars(line,col)
End;

{ compile a query typed by the user; return false if there was no query to
 compile, that is, the user just hit the return key; chars after the 
 end-of-query mark stay in the input buffer }  
Function ParseCommandLineQuery( P : ProgPtr ) : Boolean;
Var
  y : TSyntax;
  K : TokenPtr;
Begin
  ParseCommandLineQuery := False;
  y := GetSyntax(P);
  K := ReadToken(y);
  If Error Then Exit;
  If TokenType(K) <> TOKEN_END_OF_INPUT Then
  Begin
    CompileOneQuery(P,K,False);
    If Error Then Exit;
    ParseCommandLineQuery := True
  End
End;

{ parse and append rules and queries to a program; top-level strings (that is, 
 when a rule is expected) are taken to have the value of a comment in all the 
 supported Prolog syntaxes }
Procedure ParseRulesAndQueries( P : ProgPtr; RuleType : RuType );
Var
  Stop : Boolean;
  y : TSyntax;
  K : TokenPtr;
  StopTokens : TTokenSet;
Begin
  y := GetSyntax(P);
  K := ReadToken(y);
  If Error Then Exit;
  Stop := False;
  Error := False;
  { common tokens ending a series of queries or rules }
  StopTokens := [TOKEN_END_OF_INPUT,TOKEN_STRING];
  If GetSyntax(P) = PrologII Then
    StopTokens := StopTokens + [TOKEN_SEMICOLON]; 
  Repeat
    Case TokenType(K) Of
    TOKEN_END_OF_INPUT:
      Stop := True;
    TOKEN_SEMICOLON:
      If GetSyntax(P) = PrologII Then { old Prolog II termination }
        Stop := True
      Else
        SyntaxError(TokenStr[TOKEN_SEMICOLON] + ' not expected here');
    TOKEN_STRING:
      K := ReadToken(y);
    TOKEN_ARROW:
      CompileQueries(P,K,True,[TOKEN_ARROW],StopTokens);
    Else { rules }
      CompileRules(P,K,StopTokens + [TOKEN_ARROW],RuleType)
    End
  Until Stop Or Error;
  If Error Then Exit;
  { machine state }
  P^.PP_UVAR := P^.PP_DVAR;
  P^.PP_UCON := P^.PP_DCON
End;
