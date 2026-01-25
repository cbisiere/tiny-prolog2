{ switch directives and defines }

{$V-} { No strict type checking for strings }

{ supported compilers: Free Pascal, Turbo Pascal 4 }
{$IFNDEF FPC}
{$DEFINE TPC}
{$ENDIF}

{ TP: make sure CPU16 is defined }
{$IFDEF MSDOS}
{$DEFINE CPU16}
{$ENDIF}
