{----------------------------------------------------------------------------}
{                                                                            }
{   Application : PROLOG II                                                  }
{   File        : Run.pas                                                    }
{   Author      : Christophe Bisiere                                         }
{   Date        : 1988-01-07                                                 }
{   Updated     : 2023                                                       }
{                                                                            }
{----------------------------------------------------------------------------}
{                                                                            }
{       L O A D   R U L E S   A N D   E X E C U T E   Q U E R I E S          }
{                                                                            }
{----------------------------------------------------------------------------}

{$R+} { Range checking on. }
{$V-} { No strict type checking for strings. }

{ file suffix: for PII+ and Edinburgh, see PII+ doc p297 }
Type 
  TFileExt = Array[TSyntax] Of String[3];
Const 
  FileExt : TFileExt = ('pro','p2c','p2','p2E'); { TODO: rewrite to handle .pl }


{ initialize the input/output system }
Procedure InitIO;
Begin
  InitIFileStack;
  InitOFileStack
End;

{ reset the input/output system, closing all open files, but preserving the
 input console buffer }
Procedure ResetIO;
Begin
  ResetIFileStack;
  ResetOFileStack
End;

{ execute query Q }
Procedure AnswerQuery( P : ProgPtr; Q : QueryPtr; Echo : Boolean );
Begin
  If Echo Then
    OutOneQuery(Q,False);
  Clock(P,Q);
  ResetIO
End;

{ execute queries starting at the current insertion level in P }
Procedure AnswerQueries( P : ProgPtr; Echo : Boolean );
Var
  Q : QueryPtr;
Begin
  Q := FirstQueryToExecute(P);
  While Q <> Nil Do
  Begin
    AnswerQuery(P,Q,Echo);
    Q := NextQuery(Q)
  End;
  RemoveQueries(P)
End;

{ return True if filename fn can be set to be an input file, using
 default program extensions for syntax y;
 NOTE: probably less TOCTOU-prone than looking for the file and then setting it  
 as input
 FIXME: where to convert through OSFilename/1 and which name to store in the IO
 stream objects is a question that need to be solved; maybe two members? 
 (the user provided name, without alteration, and the actual path to the file)}
Function SetPrologFileForInput( y : TSyntax; fn : TString ) : Boolean;
Var
  Found : Boolean;
Begin
  Found := SetFileForInput(fn);
  If Not Found Then
  Begin
    If y = Edinburgh Then
      Found := SetFileForInput(fn + '.pl');
    If Not Found Then
      Found := SetFileForInput(fn + '.' + FileExt[y])
  End;
  SetPrologFileForInput := Found
End;

{ load rules and queries from a Prolog file, and execute the queries it 
 contains, if any; if TryPath is True, try to use the main program dir first }
Procedure LoadProgram( P : ProgPtr; s : StrPtr; TryPath : Boolean );
Var 
  y : TSyntax;
  FileName, Path : TString;
  Opened : Boolean;
Begin
  y := GetSyntax(P);
  If StrLength(s) <= StringMaxSize Then
  Begin
    FileName := StrGetString(s);
    Path := GetProgramPath(P);
    Opened := False;
    If TryPath And (path <> '') And 
        (Length(Path) + Length(FileName) <= StringMaxSize) Then
      Opened := SetPrologFileForInput(y,Path + FileName);
    If Not Opened Then
      Opened := SetPrologFileForInput(y,FileName);
    If Opened Then
    Begin
      BeginInsertion(P);
      ParseRulesAndQueries(P,GetRuleType(P));
      If Error Then Exit;
      CloseCurrentInput;
      If Error Then Exit;
      { clearing goals only after closing the input file is the right way to  
        do it, as calls to input_is, etc. must not consider the program file 
        as an input file }
      AnswerQueries(P,GetRuleType(P)=RTYPE_USER);
      EndInsertion(P)
    End
    Else
      RuntimeError('Cannot open file ')
  End
  Else
    RuntimeError('filename is too long');
End;
