unit utildb;
(*##*)
(*******************************************************************
*                                                                 *
*   U  T  I  L  D  B    Ensen's database routines for Delphi       *
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
{ database routines }

interface
uses
  db;
{ db
}
function getParType(const S: ShortString): TFieldType;

implementation

function getParType(const S: ShortString): TFieldType;
var
  ADT: TFieldType;
  C: Char;
begin
  ADT:= ftString;
{   ftUnknown;}
  getParType:= ftString;
  if Length(S) = 0
  then Exit;
  C:= Upcase(S[1]);
  case C of
  'A':;                  { Alpha byte }
  'N': ADT:= ftFloat;    { Number }
  '$': ADT:= ftCurrency; { Money }
  'S': ADT:= ftSmallint; { Short }
  'I': ADT:= ftInteger;  { Long Integer}
  '#': ADT:= ftBCD;      { 0 - 32* BCD }
  'D': ADT:= ftDate;
  'T','@': ADT:= ftDateTime;
{$IFDEF VER90}
  'M': ADT:= ftMemo;
  'F': ADT:= ftFmtMemo;
  'O': ADT:= ftParadoxOLE;
  'B': ADT:= ftTypedBinary;
{$ENDIF}
  'G': ADT:= ftGraphic;
  'L': ADT:= ftBoolean;
   end;
  getParType:=ADT;
end;

end.
 