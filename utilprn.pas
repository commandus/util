unit utilprn;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  P  R  N   Ensen's printer routines for Delphi      *
*                                                                 *
*   Copyright (c) 1998, A.Ivanov. All rights reserved.             *
*   Based on strutil unit (part of VGAX graphic library) for BP7  *
*   Conditional defines:                                           *
*                                                                 *
*   Last Revision: Mar 19 1998                                     *
*   Last fix     : Oct 01 1998                                    *
*   Lines        : 42                                              *
*   History      : Moved from util1 Mar 09 2000                   *
*   Printed      : ---                                             *
*                                                                 *
********************************************************************)
(*##*)
{ print file routines
}

interface
uses
  SysUtils, Printers;

function PrintFile(const TextFN: String; Control: Word): Boolean;

implementation

function PrintFile(const TextFN: String; Control: Word): Boolean;
var
  IFile,
  PrnFile: TextFile;
  S: String;
begin
  PrintFile:= False;
  if FileExists(TextFN) then begin
    Assign(IFile, TextFN);
    Reset(IFile);
    if IOResult <> 0
    then Exit;
    AssignPrn(PrnFile);
    Rewrite(PrnFile);
    while not EOF(IFile) do begin
      Readln(IFile, S);
      Writeln(PrnFile, S);
    end;
    System.CloseFile(PrnFile);
  end;
  PrintFile:= True;
end;

end.
